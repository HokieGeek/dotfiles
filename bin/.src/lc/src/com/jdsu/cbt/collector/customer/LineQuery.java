/**
 * @author mur50399
 *
 */

package com.jdsu.cbt.collector.customer;

import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;
import java.util.TreeMap;

import com.jdsu.netopt.util.RecordHeader;
import com.jdsu.netopt.util.RecordSet;

import com.jdsu.netopt.ifc.cbt.util.Tracer;

public class LineQuery {
	private Connection dbConn = null;
	private String collectorClass = null;
	private String collectorInterval = null;
	private String queryLines = null;
	private String queryCountLines = null;
	private RecordHeader neRecordHeader = new RecordHeader();

	public LineQuery(Connection conn, String collClass, String linesQuery, String collInterval, String linesCountQuery)
	{
		dbConn = conn;
		collectorClass = collClass;
		collectorInterval = collInterval;
		queryLines = linesQuery;
		queryCountLines = linesCountQuery;
		neRecordHeader.addAll(LineRecord.headerStrings);
	}

	/**
	 * @param requestDate
	 * @param collectorName
	 * @return
	 * @throws SQLException
	 */
	public Map performQuery(String requestDate, String collectorName) throws SQLException
	{
		TreeMap map = new TreeMap();
		int collectionsPerHour = 0;

		PreparedStatement psSelectLines = dbConn.prepareStatement(queryLines);
		psSelectLines.setString(1, requestDate);
		//psSelectLines.setString(2, collectorName);
		try {
			Tracer.trace("INFO","Input Params for Select Qry, requestDate:"+requestDate+"collectorName:"+collectorName+"Select 24hr qry:"+queryLines);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		ResultSet rsLines = psSelectLines.executeQuery();

		PreparedStatement psSelectCountLines = dbConn.prepareStatement(queryCountLines);
		psSelectCountLines.setString(1, collectorName);
		psSelectCountLines.setString(2, requestDate);
		ResultSet rsCountLines = psSelectCountLines.executeQuery();
		rsCountLines.next();
		int rsCountLinesSize = rsCountLines.getInt(1);
		int rsCountLinesInterval = rsCountLines.getInt(2);

		//get interval in seconds either from DB or from config file(cbt_customer-collector)
		if(rsCountLinesInterval == 0){
			try {
				collectionsPerHour = Integer.parseInt(collectorInterval);
			} catch (NumberFormatException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}else{
			try {
				collectionsPerHour = rsCountLinesInterval;
			} catch (NumberFormatException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		//To calculate and limit the no of customer records per collection
		int customerRecordsPerCollection = (collectionsPerHour*rsCountLinesSize)/(60*60*24);

		try {
			Tracer.trace("INFO","Round Robin Values, rsLinesSize:"+rsCountLinesSize+"collectionsPerHour:"+collectionsPerHour+"customerRecordsPerCollection:"+customerRecordsPerCollection);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		for (int i = 0; i < customerRecordsPerCollection; i++) 
		{
//			while (rsLines.next())
//	        {
				rsLines.next();

					String neName = rsLines.getString(16);
					RecordSet ne = (RecordSet) map.get(neName);
					if (ne == null) {
						ne = new RecordSet(neRecordHeader);
						map.put(neName, ne);
					}
	
					LineRecord rec = new LineRecord();
					rec.addAttribute(LineRecord.RECORD_TYPE, rsLines.getString(1));
					rec.addAttribute(LineRecord.CUSTOMER_ID, rsLines.getString(2));
					rec.addAttribute(LineRecord.EMS_ID, rsLines.getString(3));
					rec.addAttribute(LineRecord.CLASS, rsLines.getString(4));
					rec.addAttribute(LineRecord.PRIORITY, rsLines.getString(5));
					rec.addAttribute(LineRecord.NE_VENDOR, rsLines.getString(6));
					rec.addAttribute(LineRecord.NE_MODEL, rsLines.getString(7));
					rec.addAttribute(LineRecord.NE_VERSION, rsLines.getString(8));
					rec.addAttribute(LineRecord.NE_ID, rsLines.getString(9));
					rec.addAttribute(LineRecord.NE_PORT, rsLines.getString(10));
					rec.addAttribute(LineRecord.NE_VENDOR2, rsLines.getString(11));
					rec.addAttribute(LineRecord.NE_MODEL2, rsLines.getString(12));
					rec.addAttribute(LineRecord.NE_VERSION2, rsLines.getString(13));
					rec.addAttribute(LineRecord.NE_ID2, rsLines.getString(14));
					rec.addAttribute(LineRecord.NE_PORT2, rsLines.getString(15));

					ne.addRecord(rec);
//	        }
		}

		if(rsCountLines != null){
			rsCountLines.close();
		}
		psSelectCountLines.close();

		if(rsLines != null){
			rsLines.close();
		}
		psSelectLines.close();

		return map;
	}

}
