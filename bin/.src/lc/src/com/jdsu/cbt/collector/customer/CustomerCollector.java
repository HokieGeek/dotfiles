/**
 * @author mur50399
 *
 */

package com.jdsu.cbt.collector.customer;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Date;

import com.jdsu.netopt.collector.InteractiveCollector;
import com.jdsu.netopt.collector.CollectFailException;
import com.jdsu.netopt.collector.CollectorParameters;
import com.jdsu.netopt.collector.CollectorStatus;
import com.jdsu.netopt.collector.PassiveFile;
import com.acterna.netopt.core.AdaProps;

import com.jdsu.cbt.collector.util.ProxyServerEMSRequest;
import com.jdsu.cbt.collector.util.ProxyServerInteraction;

import com.jdsu.netopt.util.RecordSet;
import com.jdsu.netopt.util.RecordList;
import com.jdsu.netopt.util.DBQueryManager;
import com.jdsu.netopt.util.StringProcessor;
import com.jdsu.netopt.ifc.cbt.util.Tracer;

public class CustomerCollector extends InteractiveCollector
{
	protected CollectorStatus status = null;
	protected PassiveFile pf = null;
	protected String request_filepattern = null;
	protected Connection dbConn = null;
	protected String emsIDList = "";

    // The below values are mapped to the scripts cbt_customer-collector & cbt_customer-collector.params
    protected static final String COLLECTION_TYPE_PARAM = "p_user_parm5"; //for either values 2/3 from script, default 2 below
    protected static final String COLLECTION_TRACING_PARAM = "p_user_parm16"; //yes/no? in interactive script
    protected static final String REQUEST_FILE_PATTERN_PARAM = "p_user_parm7"; //"_request_" from script
    protected static final String COLLECTOR_24HR_SQL_CONFIG_FILE_PROPERTY = "COLLECTOR_24HR_SQL_CONFIG_FILE";
    protected static final String COLLECTOR_3HR_SQL_CONFIG_FILE_PROPERTY = "COLLECTOR_3HR_SQL_CONFIG_FILE";
    protected static final String COLLECTOR_SQL_CONFIG_DIR_PROPERTY = "COLLECTOR_SQL_CONFIG_DIR";
	protected static final String COLLECTOR_24HR_NAME_PROPERTY = "COLLECTOR_24HR_NAME";
	protected static final String COLLECTOR_3HR_NAME_PROPERTY = "";
	protected static final String COLLECTOR_24HR_INTERVAL_PROPERTY = "COLLECTOR_24HR_INTERVAL";
	protected static final String COLLECTOR_3HR_INTERVAL_PROPERTY = "";
	protected static final String COLLECTOR_24HR_CLASS_PROPERTY = "COLLECTOR_24HR_CLASS";
	protected static final String COLLECTOR_3HR_CLASS_PROPERTY = "";
    protected static final String SELECT_QUERY = "NCC_SELECT";
    protected static final String UPDATE_QUERY = "NCC_UPDATE";
    protected static final String COUNT_QUERY = "NCC_COUNT_SELECT";

    protected String timestamp = "";
	protected String requestDate = "";
	protected String postDate = "";
	String emsID = "tmp";//"ACSWEST";//

    /*	public static void main(String args[]){
		System.out.println("Customer Collector Reference Packaging works!!!");
	}*/

	public CustomerCollector() {
	}

	public int collect(String sitename, PassiveFile inPF, CollectorParameters params) throws Exception {
		status = new CollectorStatus(sitename);
		status.setStatus("initializing");

		pf = inPF;
		Tracer.setPassiveFile(pf);

		status.setStatus("Begin configuration of all params for collection");
		// Enable passive file tracing if desired.
		String tracing = params.getParam(COLLECTION_TRACING_PARAM);
		pf.setTracing(tracing);
		Tracer.trace("INFO","Begin Customer Collector");

		String select24HRQry = "", update24HRQry= "", countQuery="", queryConfigFileProperty = null;
		ProxyServerInteraction spInteract = new ProxyServerInteraction();

		// Convert "yyyyMMddHHmm" to "yyyy-MM-dd HH:mm" format
		timestamp = params.getParam("p_period_end");
		requestDate = 
			timestamp.substring(0,4) + "-" +
			timestamp.substring(4,6) + "-" +
			timestamp.substring(6,8);
		postDate = 
			timestamp.substring(0,4) + "-" +
			timestamp.substring(4,6) + "-" +
			timestamp.substring(6,8) + " " +
			timestamp.substring(8,10) + ":" +
			timestamp.substring(10,12) + " ";
		Tracer.trace("INFO","timestamp:"+timestamp+"requestDate:"+requestDate+"postDate:"+postDate);

		String collectionTypeParam = params.getParam(COLLECTION_TYPE_PARAM); //"2";//
		if(collectionTypeParam == null || collectionTypeParam.length() == 0){
			throw new CollectFailException("Cannot determine value for type of query to be selected. Should be either 24 or 3.");
		} else{
			if(collectionTypeParam.trim().equals("2")){
				queryConfigFileProperty = COLLECTOR_24HR_SQL_CONFIG_FILE_PROPERTY;
			}else if(collectionTypeParam.trim().equals("3")){
				queryConfigFileProperty = COLLECTOR_3HR_SQL_CONFIG_FILE_PROPERTY;
			}else{
				throw new CollectFailException("Collection qry values should be either 24 or 3");
			}
		}
		Tracer.trace("INFO","Collection Qry selected of Type:"+queryConfigFileProperty);


		//START: Get Values from cbt_customer-collector
		String queryConfigFile = AdaProps.getProperty(queryConfigFileProperty);
		if(queryConfigFile == null || queryConfigFile.length() == 0){
			throw new CollectFailException("Collector File Query not set !!!" + 
					"collectionTypeParam value:"+collectionTypeParam+"queryConfigFileProperty value:"+queryConfigFileProperty+"\n"+
					"queryConfigFile value:"+queryConfigFile);
		}

		String queryConfigDir = AdaProps.getProperty(COLLECTOR_SQL_CONFIG_DIR_PROPERTY);
		if( queryConfigDir == null || queryConfigDir.length() == 0){
			throw new CollectFailException("Collector Directory Query not set !!!" + "queryConfigDir value:" + queryConfigDir);
		}
		status.setStatus("Query Type:" + queryConfigFileProperty + "\n" + "Query Directory:" + queryConfigDir + "\n" + 
				"Query File:" + queryConfigFile);
		Tracer.trace("INFO","QRY confi dir:" + queryConfigDir + " and qry config file:" + queryConfigFile);

		String collectorName = AdaProps.getProperty(COLLECTOR_24HR_NAME_PROPERTY, "DSLAM_HR24");
		if( collectorName == null || collectorName.length() == 0){
			throw new CollectFailException("Collector Name not set !!!" + "collectorName value:" + collectorName);
		}

		String collectorInterval = AdaProps.getProperty(COLLECTOR_24HR_INTERVAL_PROPERTY, "3600");
		if( collectorInterval == null || collectorInterval.length() == 0){
			throw new CollectFailException("Collector Interval not set !!!" + "collectorInterval value:" + collectorInterval);
		}

		String collectorClass = AdaProps.getProperty(COLLECTOR_3HR_CLASS_PROPERTY, "qoecheck");
		if( collectorClass == null || collectorClass.length() == 0){
			throw new CollectFailException("Collector Class not set !!!" + "collectorClass value:" + collectorClass);
		}
		//END: Get Values from cbt_customer-collector

		String sqlFile = queryConfigDir + File.separator + queryConfigFile;
		status.setStatus("Query Type:" + queryConfigFileProperty + "\n" + "Query Directory:" + queryConfigDir + "\n" +
				"Query File:" + queryConfigFile + "sqlFile:" + sqlFile + "fileseparator:" + File.separator);
		Tracer.trace("INFO","SQL file is:"+sqlFile);

		DBQueryManager queries = null;
		try {
			queries = new DBQueryManager(sqlFile);
		} catch (Exception e) {
			throw new CollectFailException("Error encountered reading collector SQL file " + sqlFile + ":\n" + e.getMessage());
		}
		Tracer.trace("","");

		select24HRQry = queries.getStatement(SELECT_QUERY);
		if(select24HRQry == null || select24HRQry.length() == 0){
			throw new CollectFailException("Query '" + SELECT_QUERY + "' not present in collector SQL file " + sqlFile);
		}
		countQuery = queries.getStatement(COUNT_QUERY);
		if(countQuery == null || countQuery.length() == 0){
			throw new CollectFailException("Query '" + COUNT_QUERY + "' not present in collector SQL file " + sqlFile);
		}
    	status.setStatus("select 24hr qry:"+select24HRQry);

    	update24HRQry = queries.getStatement(UPDATE_QUERY);
    	if(update24HRQry == null || update24HRQry.length() == 0){
    		throw new CollectFailException("Query '" + UPDATE_QUERY + "' not present in collector SQL file " + sqlFile);
    	}
    	status.setStatus("update 24hr qry:"+update24HRQry);
		Tracer.trace("SQL","select 24hr qry:"+select24HRQry+"update 24hr qry:"+update24HRQry);

		request_filepattern = params.getParam(REQUEST_FILE_PATTERN_PARAM);
		if ((request_filepattern == null) || (request_filepattern.equals("")))
		{
			request_filepattern = ProxyServerEMSRequest.REQUEST_FILE_PATTERN;
		}

		//DB connection
		try {
			// Load the JDBC driver
			String driverName = "oracle.jdbc.driver.OracleDriver";
			Class.forName(driverName);
			
			// Create a connection to the database
			String serverName = "zinc";
			String portNumber = "1521";
			String sid = "tdce";
			String url = "jdbc:oracle:thin:@" + serverName + ":" + portNumber + ":" + sid;
			String username = "tdce";
			String password = "linesize";
			dbConn = DriverManager.getConnection(url, username, password);
	    } catch (ClassNotFoundException e) {
	    	Tracer.trace("INFO","Could not find the database driver"+e);
	    } catch (SQLException e) {
	    	Tracer.trace("INFO","Could not connect to the database"+e);
	    }
		status.setStatus("run queries and begin collection"+dbConn);
		Tracer.trace("INFO","Connected to DB:");

		LineQuery lq = new LineQuery(dbConn, collectorClass, select24HRQry, collectorInterval, countQuery);
		Map nes = null;
		try{
			nes = lq.performQuery(requestDate, collectorName);
			Tracer.trace("INFO", "Returned " + nes.size() + " Files");
		}
		catch (SQLException e){
			throw new CollectFailException("Error querying lines for collection:\n" + e.getMessage());
		}
		Tracer.trace("INFO","Select QRY return successful");

		PreparedStatement psUpdateLines;
		try {
			psUpdateLines = dbConn.prepareStatement(update24HRQry);
		} catch (SQLException e1) {
			throw new CollectFailException("Error collecting lines for updating NEs:\n" + e1.getMessage());
		}

		for (Iterator i = nes.keySet().iterator(); i.hasNext(); )
		{
				String neName = (String)i.next();
				Tracer.trace("INFO", "Returned DSLAM:" + neName);
				RecordSet ne = (RecordSet) nes.get(neName);
		
				// get EMS ID from first record in set
				RecordList records = ne.getRecordList();
				//LineRecord first = (LineRecord) records.getList().get(0);
				List ls = records.getList();
				//String emsID = (String) first.getAttribute(LineRecord.EMS_ID);

				try {
					ProxyServerEMSRequest spRequest = new ProxyServerEMSRequest(spInteract, emsID, neName, 
							timestamp, request_filepattern);
					spRequest.open();
					spRequest.getRecordWriter().writeRecordSet(ne);
					spRequest.close();
			
					spRequest = null;
				}
				catch (Exception e) {
					e.printStackTrace();
					Tracer.trace("INFO", "Proxy Server request log:\n");
					throw new CollectFailException("Error producing Proxy Server Request File:\n" + e.getMessage());
				}

				try {
					psUpdateLines.setString(1, postDate);

					StringBuffer namebuf = new StringBuffer(); 
					namebuf.append("(");
					for(int cnt=0;cnt<ls.size();cnt++) 
					{
				        Map map = (Map)ls.get(cnt);
						String id = (String)map.get(LineRecord.CUSTOMER_ID);

						Tracer.trace("INFO", "Name Value:"+id);
						psUpdateLines.setString(2, id);
						psUpdateLines.executeUpdate();

//			            if (cnt > 0)
//			            	namebuf.append(",");
//			            namebuf.append(StringProcessor.oracleQuote(id));
			        }
					namebuf.append(")");

//					psUpdateLines.setString(2, namebuf.toString()); // check this value for updating either DSLAM/customer
					Tracer.trace("INFO", "Update query args, date:"+postDate+" AND name:"+namebuf.toString());

//					psUpdateLines.executeUpdate();
					
					dbConn.commit();
				} catch (SQLException e) {
					e.printStackTrace();
					Tracer.trace("INFO", "Update Error log:\n");
					throw new CollectFailException("Error updating 24hr qry:\n" + e.getMessage());
				}
		}

		psUpdateLines.close();
		dbConn.close();
		dbConn = null;
		pf.close();

		status.setStatus("Quering completed. Write data into file and post on Server");
		Tracer.trace("INFO","Customer Collector Successful\n"+"File Posted on Server");

		return 0;
	}

}
