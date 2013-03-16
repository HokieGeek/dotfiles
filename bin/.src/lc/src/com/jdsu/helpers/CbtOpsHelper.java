package com.jdsu.helpers;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.security.InvalidParameterException;
import java.util.ArrayList;
 import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.jdsu.beans.DataSet;
import com.jdsu.interfaceImpl.ComponentImpl;

/**
 * Helper class to get customer PM stats
 */
public class CbtOpsHelper extends ComponentImpl
{

    /** Logger for this class and subclasses */
    protected static final Log logger = LogFactory.getLog(CbtOpsHelper.class);

    /**
     * 
     * Default Constructor
     */
    public CbtOpsHelper()
    {

    }

    /**
     * Study Model - fixed value for this application
     */
    public static final String STUDY_MODEL = "1440";
    
    /**
     * All known Component types
     */
    public static final String[] COMPONENT_TYPES =
    { "CBT OPS CUST", "CBT OPS RG-LAN", "CBT OPS RG-WAN", "CBT OPS STB-IPTV" };
    
    
    /**
     * Keyword to Column mapping for this application 
     */
    protected static DataSet mapping = new DataSet();

    /**
     * Get customer, RG-WAN, RG-LAN, and IPTV component data. Returns null if no
     * data is found. If the date is null, the latest data is returned.
     * 
     * @param name
     *            Component name (required)
     * @param date
     *            Measurement date
     * 
     */
    public DataSet getCustomerData(String name, Date date)
    {
        int numComp = COMPONENT_TYPES.length;
        DataSet msmts = new DataSet();
        msmts.add("DS NAME", "CUSTOMER DATA");
        msmts.add("DS TIME", new Date());
        for (int i = 1, j=0; i < numComp; i++,j++)
        {
            DataSet params = new DataSet();
            params.add("compType", COMPONENT_TYPES[i]);
            DataSet custData = null;
            if (date!=null)
            {
                params.add("tableName", "measurement");
                DataSet custDataMap = getKeywordColumnMapping(params);

                custDataMap.add("customer", name);
                custDataMap.add("compType", COMPONENT_TYPES[i]);
                custDataMap.add("date", date);
                custData = getMsmt(custDataMap);
                msmts.add(COMPONENT_TYPES[i], custData.get("MEASUREMENT"));
            }
            else
            {
                params.add("tableName", "ne_comp");
                DataSet custDataMap = getKeywordColumnMapping(params);

                custDataMap.add("customer", name);
                custDataMap.add("compType", COMPONENT_TYPES[i]);
                custData = getNeComp(custDataMap);
                msmts.add(COMPONENT_TYPES[i], custData.get("NE_COMP"));
            }
        }
        logger.info("return latestMsmt.." + msmts);
        return msmts;
    }

    /**
     * Collect and return customer, RG-WAN, RG-LAN, and IPTV component data.
     * Returns null if no data is found. If the date is null, the latest data is
     * returned.<br>
     * If the date is specified, return the row with the closest date.
     * 
     * @param name
     *            Component name (required)
     * @param date
     *            Measurement date
     * 
     */
    public DataSet collectCustomerData(String name, Date date)
    {
        int numComp = COMPONENT_TYPES.length;
        DataSet msmts = new DataSet();
        msmts.add("DS NAME", "CUSTOMER DATA");
        msmts.add("DS TIME", new Date());
        for (int i = 1; i < numComp; i++)
        {
            DataSet params = new DataSet();
            params.add("compType", COMPONENT_TYPES[i]);
            params.add("tableName", "ne_comp");
            DataSet custDataMap = getKeywordColumnMapping(params);

            custDataMap.add("customer", name);
            custDataMap.add("compType", COMPONENT_TYPES[i]);
            custDataMap.add("date", date);
            DataSet custData = getMsmt(custDataMap);
            msmts.add(COMPONENT_TYPES[i], custData.get("NE_COMP"));
        }
        return msmts;
    }

    
    /**
     * Get Keyword - DB table column mapping in the 
     * Net Optimize schema for this study model
     * 
     * @param params
     * @return
     */
    private DataSet getKeywordColumnMapping(DataSet params)
    {
        String compType = (String) params.get("compType");
        String tableName = (String) params.get("tableName");
        String label = (String) params.get("label");
        StringBuffer id = new StringBuffer();

        if (tableName!=null) id.append(tableName.toUpperCase());
        if (compType!=null) id.append(compType.toUpperCase());
        if (label!=null) id.append(label.toUpperCase());
        
        /*
         * If this map already exists in the static mapping,
         * just return it.
         */
        if (mapping.get(id.toString())!= null)
        {
            DataSet retMapping = new DataSet();
            retMapping.add("MAPPING", mapping.get(id.toString()));
            return retMapping;
        }
        
        /*
         * Does not exist already, so fetch and save it.
         */
        StringBuffer sql = new StringBuffer(
           "select da.table_name \"table\", da.attribute \"column\", " +
                       "nect.name \"component\"," +
                       "lw.keyword_label \"label\", aa.label \"name\" "
                        + "from assigned_attribute aa "
                        + "join word_used wu on wu.assigned_attribute_id = aa.id "
                        + "join db_attr_assign daa on aa.db_attr_assign_id = daa.id "
                        + "join db_attribute da on da.id = daa.db_attribute_id "
                        + "join line_word lw on wu.line_word_id = lw.id "
                        + "join ne_comp_type nect on daa.ne_comp_type_id = nect.id "
                        + "where aa.study_model_id = "
                        + STUDY_MODEL);
        if (compType != null && compType.length()>0)
        {
            sql.append(" and nect.name like '" + compType + "'");
        }
        if (tableName != null && tableName.length()>0)
        {
            sql.append(" and da.table_name = '"+tableName+"' ");
        }
        if (label != null && label.length()>0)
        {
            sql.append(" and lw.keyword_label = '"+label+"' ");
        }
        sql.append("order by da.sort_order");

        DataSet retMapping = JdbcHelper.runAQuery(sql.toString(), "MAPPING");
        
        // save it in static mapping for later use
        mapping.add(id.toString(), retMapping.get("MAPPING"));
        mapping.add("DS TIME", new Date());
        
        return retMapping;

    }

    
    
    
    public DataSet getComponent(String level, DataSet hierarchy)
    {
        /*
         * The DataSet is expected to contain hierarchy information.
         *  e.g:
         * 0 - MID-ATLANTIC
         * 1 - MD
         * 2 - 236
         * 3 - GERMANTOWN
         * etc.
         * 
         * These will be converted to SQL filter appropriately.
         * 
         * If level 2 is requested, the DataSet is expected to have
         * level 0,1 values for appriate filter generation.
         * 
         */
        return getNe(level, hierarchy);
    }

    public DataSet getDlamCustStats(String dslamName)
    {
        DataSet params = new DataSet();
        params.add("tableName", "ne_comp");
        params.add("compType", "CBT OPS %");
        params.add("label", "kpithv");
        DataSet dslamMap = getKeywordColumnMapping(params);
        dslamMap.add("dslam", dslamName);
        dslamMap.add("compType", "CBT OPS %");
        DataSet custStats = getNeComp(dslamMap);

        // Filter 4 records into one
        List<Map<String, Object>> customers = (List<Map<String, Object>>) custStats.get("NE_COMP");
        List<Map<String,String>> custStatSummary = new ArrayList<Map<String,String>>();
        List<Map> kpiMap = (List) dslamMap.get("MAPPING");

        String prevCust = "";
        Map<String, String> currCust = new TreeMap<String, String>();
        for (int i = 0; i < customers.size(); i++)
        {
            
            Map<String, String> currRec = (Map) customers.get(i);
            String cust = (String) currRec.get("CUSTOMER");
            String compType = (String) currRec.get("COMP_TYPE");
            if (prevCust.length()>0 && !cust.equals(prevCust))
            {
                // next customer. Save previous one 
                custStatSummary.add(currCust);
                currCust = new TreeMap<String, String>();
            }

            // ordered by customer... so safe to do this
            if (compType.equals(COMPONENT_TYPES[0]))
            {
                currCust = new TreeMap<String, String>();
                currCust.put(COMPONENT_TYPES[0], cust);
                currCust.put("REGION", (String) currRec.get("REGION"));
                currCust.put("STATE", (String) currRec.get("STATE"));
                currCust.put("LATA", (String) currRec.get("LATA"));
                currCust.put("LOCATION", (String) currRec.get("LOCATION"));
                
            }
            else if (!compType.equals(COMPONENT_TYPES[0]))
            {
                for (int ct=0; ct<kpiMap.size();ct++)
                {
                    Map<String, String> curr = (Map<String, String>) kpiMap.get(ct);
                    if (compType.equals(curr.get("component")))
                    {
                        currCust.put(compType, currRec.get(curr.get("column")));
                        break;
                    }
                }
            }
            prevCust=cust;
        }

        if (!prevCust.equals("") && customers.size()>0)
        {
            // last record 
            custStatSummary.add(currCust);
        }
        DataSet retCusts = new DataSet();
        retCusts.add("DSLAMCUST", custStatSummary);
        retCusts.add("DS NAME", "NE_COMP");
        retCusts.add("DS TIME", new Date());
        return retCusts;
    }
    
    /**
     * Perform a real-time test/diagnostic for a customer
     *
     * @param targetId Target identified (required) - i.e. customer id
     * @param name Test name (required) - i.e. ping
     * @param DataSet Test parameters
     * @throws IOException 
     *
     */
    public DataSet peformOperation(String testName,
            DataSet params) throws IOException
    {
        DataSet results = null;
        // Test name, sql file hard-coded to avoid execution of
        // random sql files.
        // Comma separated sql file names, if more than one
        Properties testCmds = new Properties();
        testCmds.setProperty("CriticalDSLAMS", "CriticalDslams.sql");
        testCmds.setProperty("CriticalCustomer", "CriticalCustomer.sql");
        testCmds.setProperty("Equipment", "EquipDSLAM.sql,EquipRG.sql,EquipSTB.sql");
        
        // FIXME: Add params passed into the SQL
        if (testCmds.containsKey(testName))
        {
            results = new DataSet();
            results.add("DS NAME", "Perf Opr");
            results.add("DS TIME", new Date());
            String[] sqlFile = testCmds.getProperty(testName).split(",");
            
            for (int i=0; i<sqlFile.length; i++)
            {
                StringBuffer sqlBuffer = readSql(sqlFile[i]);
                // test specific
                if (testName.equals("CriticalDSLAMS"))
                {
                    // if search options are passed, find index to add filters
                    int offset = sqlBuffer.indexOf("order by");
                    if (offset == -1) // not found
                    {
                        offset = sqlBuffer.indexOf("ORDER BY");
                    }

                    // create filter
                    StringBuffer filter = new StringBuffer(" AND NE.REGION ");
                    String region = null;
                    if (params != null)
                    {
                        region = (String) params.get("REGION");
                    }

                    if (region != null && region.length() > 0)
                    {
                        filter.append(" = '" + region + "' ");
                    } else
                    {
                        filter.append("like '%' ");
                    }

                    // append or insert the filter
                    if (offset == -1) // still not found
                    {
                        // append at the end
                        sqlBuffer.append(filter);
                    } else
                    {
                        sqlBuffer.insert(offset, filter);
                    }
                }

                DataSet rtnVal = JdbcHelper.runAQuery(sqlBuffer.toString());
                results.add(sqlFile[i].substring(0, sqlFile[i].indexOf('.'))
                        .toUpperCase(), rtnVal.get("LAST_QRY"));
            }
            return results;
        }
        throw new InvalidParameterException("Invalid Test Name: " + testName);
    }

    
    private StringBuffer readSql(String fileName) throws IOException
    {
        
        StringBuffer sqlBuffer = new StringBuffer();
        try
        {
            ClassLoader cl = this.getClass().getClassLoader();
            java.net.URL url = cl.getResource(fileName);
            BufferedReader sqlIn = new BufferedReader(new InputStreamReader(url.openStream()));
            char buf[] = new char[1024];
            int readChars = 0;
            while ((readChars = sqlIn.read(buf, 0, 1024)) != -1)
            {
                // read 1024 bytes each time.
                sqlBuffer.append(buf, 0, readChars);
            }
        } catch (FileNotFoundException e)
        {
            throw new InvalidParameterException("File "+fileName+" not found");
        } 
        return sqlBuffer;
    }
    
    
}
