package com.jdsu.interfaceImpl;

import java.security.InvalidParameterException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.jdsu.beans.DataItem;
import com.jdsu.beans.DataSet;
import com.jdsu.helpers.JdbcHelper;
import com.jdsu.interfaces.Component;

public class ComponentImpl implements Component
{

    public static final String[] LEVELS =
    { "REGION", "STATE", "LATA", "LOCATION", "DSLAM" };

    public DataSet getMsmt(DataSet params)
    {
        
        /*
         * Optional parameters:
         * compType     - component type
         * MAPPING      - keyword column mapping
         * customer     - customer id
         * date         - Historical date for which data needs to be 
         *                retrieved
         * 
         */

        String compType = (String) params.get("compType"); 
        String name = (String) params.get("customer");
        Date date = (Date) params.get("date");
        // Checked if required parameters were passed
        if (name == null || compType == null || name.length() == 0
                || compType.length() == 0)
        {
            throw new IllegalArgumentException("Required parameters "
                    + "Customer Id/Component Type missing");
        }

        // FIXME: SQL needs refining
        StringBuffer sql = new StringBuffer(
                "select measurement.collection_date");
        List<Map<String, Object>> mapping = (List) params.get("MAPPING");

        if (mapping!=null)
        {
            boolean absCompSpecified = false;
            
            if (compType!= null &&
                    compType.indexOf('%')==-1 &&   
                    compType.indexOf('?')==-1)
            { 
                // if no wild card, comp type is specified
                // safe to assume no duplicate keywords
                // so flag to apply labels
                absCompSpecified = true;
                
            }

            // Build sql dynamically based on the mappings
            for (int i=0; i<mapping.size(); i++)
            {
                Map<String, Object> keyColumnMap = mapping.get(i);
                sql.append( ", " +
                            keyColumnMap.get("table")+
                            "." +
                            keyColumnMap.get("column")
                            );
                if (absCompSpecified)
                {
                    sql.append(" as \""+keyColumnMap.get("name")+"\" ");
                }
            }
        }

        
        String filter = " where ";
        String from = "from  measurement "
                + "join component comp on measurement.component_id = comp.id "
                + "join ne_comp nc on nc.id = comp.ne_comp_id "
                + "join ne_comp_type nct on nc.ne_comp_type_id = nct.id ";
        
        sql.append(from);
        // Get just one row -- the latest or the closest to the date given.
        sql.append(" where rownum=1 ");
        sql.append("and nc.name='" + name + "'  ");
        filter = filter.concat(" nc.name='" + name + "'  ");
        sql.append("and nct.name='" + compType + "' ");
        filter = filter.concat("and nct.name='" + compType + "' ");

        if (date != null)
        {
            DateFormat df = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
            // get the closest date
            sql.append("and abs (collection_date - to_date('"
                    + df.format(date) + "','YYYY-MM-DD HH24:MI:SS'))="
                    + "(select min(abs(collection_date- to_date('"
                    + df.format(date)
                    + "','YYYY-MM-DD HH24:MI:SS'))) ");
            sql.append(from);
            sql.append(filter + ")");
        } else
        {
            // latest row
            sql.append("order by measurement.collection_date desc");
        }

        DataSet msmts = JdbcHelper.runAQuery(sql.toString(),"MEASUREMENT");

        msmts.add("DS NAME", "MEASUREMENT");
        return msmts;
    }

    public DataSet getLatestMsmtSumm(String compType, String name)
    {
        return getMsmtSumm(compType, name, null);
    }

    public DataSet getMsmtSumm(String compType, String name, Date date)
    {
        // Check for required parameters
        if (name == null || compType == null || name.length() == 0
                || compType.length() == 0)
        {
            throw new IllegalArgumentException("Required parameters "
                    + "Customer Id/Component Type missing");
        }

        // FIXME: SQL needs refining
        String sql = "select measurement_summary.* "
                + "from measurement_summary "
                + "join component comp on measurement_summary.component_id = comp.id "
                + "join ne_comp nc on nc.id = comp.ne_comp_id "
                + "join ne_comp_type nct on nc.ne_comp_type_id = nct.id "
                + "where ";
        String filter = " where ";
        // Get just one row -- the latest or the closest to the date given.
        sql = sql.concat("rownum=1 ");
        sql = sql.concat("and nc.name='" + name + "'  ");
        filter = filter.concat(" nc.name='" + name + "'  ");
        sql = sql.concat("and nct.name='" + compType + "' ");
        filter = filter.concat("and nct.name='" + compType + "' ");

        if (date != null)
        {
            DateFormat df = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
            // get the closest date
            sql = sql.concat("and abs (collection_date - to_date('"
                    + df.format(date) + "','YYYY-MM-DD HH24:MI:SS'))="
                    + "(select min(abs(collection_date- to_date('"
                    + df.format(date) + "','YYYY-MM-DD HH24:MI:SS'))) "
                    + "from measurement_summary");
            sql = sql.concat(filter + ")");
        } else
        {
            // latest row
            sql = sql.concat("order by measurement.collection_date desc");
        }

        DataSet msmtSumm = JdbcHelper.runAQuery(sql);

        return msmtSumm;
    }

    public Date[] getMsmtSummTimestamps(String compType, String name)
    {
        String sql = "select SUMMARY_DATE as SDATE "
                + "from  measurement_summary "
                + "join component comp on measurement_summary.component_id =comp.id "
                + "join ne_comp nc on nc.id = comp.ne_comp_id "
                + "join ne_comp_type nct on nc.ne_comp_type_id = nct.id "
                + "where nc.name='" + name + "' " + "and nct.name='" + compType
                + "' " + "order by measurement_summary.summary_date";
        DataSet collection_dates = JdbcHelper.runAQuery(sql,"MSMT_SUMM_DATES");
        List<Map<String, Object>> dates = (List<Map<String, Object>>) collection_dates.get("MSMT_SUMM_DATES");
        Date[] dateArray = new Date[dates.size()];

        for (int i = 0; i < dates.size(); i++)
        {
            dateArray[i] = (Date) dates.get(i).get("CDATE");
        }

        return dateArray;
    }

    public Date[] getMsmtTimestamps(String compType, String name)
    {
        String sql = "select collection_date as CDATE "
                + "from  measurement "
                + "join component comp on measurement.component_id =comp.id "
                + "join ne_comp nc on nc.id = comp.ne_comp_id "
                + "join ne_comp_type nct on nc.ne_comp_type_id = nct.id "
                + "where nc.name='" + name + "' " + "and nct.name='" + compType
                + "' " + "order by measurement.collection_date";
        DataSet collection_dates = JdbcHelper.runAQuery(sql,"MSMT_DATES");
        List<Map<String, Object>> dates = (List<Map<String, Object>>) collection_dates.get("MSMT_DATES");
        Date[] dateArray = new Date[dates.size()];

        for (int i = 0; i < dates.size(); i++)
        {
            dateArray[i] = (Date) dates.get(i).get("CDATE");
        }

        return dateArray;
    }

    public DataSet getNeComp(DataSet params)
    {
        /*
         * Optional parameters:
         * compType     - component type
         * MAPPING      - keyword column mapping
         * dslam        - DSLAM id
         * customer     - customer id
         * 
         */

        StringBuffer sql = new StringBuffer("SELECT ne_comp.name as CUSTOMER, " +
                "nct.name AS COMP_TYPE, " +
                "ne.name as DSLAM, ne.region as REGION, " +
                "ne.state as STATE," + 
                " ne.lata as LATA, ne.location as LOCATION ");
        
        List<Map<String, Object>> mapping = (List) params.get("MAPPING");
        String compType = (String)params.get("compType");

        if (mapping!=null)
        {
            boolean absCompSpecified = false;
            
            if (compType!= null &&
                    compType.indexOf('%')==-1 &&   
                    compType.indexOf('?')==-1)
            { 
                // if no wild card, comp type is specified
                // safe to assume no duplicate keywords
                // so flag to apply labels
                absCompSpecified = true;
                
            }

            // Build sql dynamically based on the mappings
            for (int i=0; i<mapping.size(); i++)
            {
                Map<String, Object> keyColumnMap = mapping.get(i);
                sql.append( ", " +
                            keyColumnMap.get("table")+
                            "." +
                            keyColumnMap.get("column")
                            );
                if (absCompSpecified)
                {
                    sql.append(" as \""+keyColumnMap.get("name")+"\"");
                }
            }
        }
        sql.append(" FROM NE, NE_COMP "
                + " JOIN ne_comp_type nct ON ne_comp.ne_comp_type_id = nct.id ");
        if (compType!=null && compType.length()>0)
        {
            sql.append(" AND nct.name LIKE '" + compType
                    + "' AND nct.name NOT like 'CBT OPS KPI SUM' ");
        }
        sql.append(" WHERE ne_comp.ne_id = ne.id ");

        String dslam = (String) params.get("dslam");
        if (dslam !=null && dslam.length()>0)
        {
            sql.append(" AND ne.name = '" + dslam + "' ");
        }
        
        String customer = (String) params.get("customer");
        if (customer !=null && customer.length()>0)
        {
            sql.append(" AND NE_COMP.name = '" + customer + "' ");
        }

        sql.append(" ORDER BY ne_comp.name, nct.name");

        DataSet custStats = JdbcHelper.runAQuery(sql.toString(),"NE_COMP");

        // Specify a name for this DataSet.
        custStats.add("DS NAME", "NE_COMP");

        return custStats;
    }

    public DataSet getNe(String hierarchyLevel, DataSet hierarchy)
    {
        // make sure it is a valid level number
        String regex = "[0-" + LEVELS.length + "]";
        if (hierarchyLevel == null || !hierarchyLevel.matches(regex))
        {
            throw new InvalidParameterException("");
        }
        int level = Integer.valueOf(hierarchyLevel).intValue();

        
        // FIXME: SQL needs refining
        String sql = "SELECT n.ID, n.NAME, n.NE_TYPE_VERSION_ID, n.REGION,"
                + " n.STATE, n.LATA, n.LOCATION, " +
                        "nc.NUM_PARM1 AS WAN_GOOD, " +
                        "nc.NUM_PARM2 AS WAN_FAIR, " +
                        "nc.NUM_PARM3 AS WAN_POOR, " +
                        "nc.NUM_PARM4 AS WAN_UNKNOWN, " +
                        "nc.NUM_PARM5 AS LAN_GOOD, " +
                        "nc.NUM_PARM6 AS LAN_FAIR, " +
                        "nc.NUM_PARM7 AS LAN_POOR, " +
                        "nc.NUM_PARM8 AS LAN_UNKNOWN, " +
                        "nc.NUM_PARM9 AS IPTV_GOOD, " +
                        "nc.NUM_PARM10 AS IPTV_FAIR, " +
                        "nc.NUM_PARM11 AS IPTV_POOR, " +
                        "nc.NUM_PARM12 AS IPTV_UNKNOWN " +
                        "FROM ne n, " +
                        "ne_type nt JOIN ne_type_version ntv " +
                        "ON ntv.NE_TYPE_ID = nt.ID, " +
                        "ne_comp nc JOIN ne_comp_type nct " +
                "ON nc.ne_comp_type_id = nct.id " +
                "WHERE       nc.ne_id = n.id AND " +
                "n.NE_TYPE_VERSION_ID = ntv.ID "
        + "AND nct.name = 'CBT OPS KPI SUM' ";

        if (level != 4) // not DSLAM
        {
            sql = sql.concat(" AND nt.MODEL = 'Virtual' "
                    + "AND nt.VENDOR = 'JDSU' ");
 
        }
        sql = sql.concat("AND nct.name = 'CBT OPS KPI SUM' ");
        sql = sql.concat("and nt.TYPE='" + LEVELS[level] + "' ");

        // Region has no previous level
        // - ignore
        if (level > 0)
        {
            // Add filters for all the previous levels
            for (int index=level-1; index>=0; index--)
            {  
                // check if it exists!
                if (hierarchy.get(Integer.toString(index)) != null)
                {
                    sql = sql.concat("and n." + LEVELS[index] + " = '" + 
                            hierarchy.get(Integer.toString(index))+ "' ");
                }
            }

        }

        DataSet compInfo = JdbcHelper.runAQuery(sql, "NE");

        // Specify a name for this DataSet.
        DataItem dsName = new DataItem("DS NAME", "NE", "DataSet Name", null);

        compInfo.add("DS NAME", dsName);

        return compInfo;
    }

}
