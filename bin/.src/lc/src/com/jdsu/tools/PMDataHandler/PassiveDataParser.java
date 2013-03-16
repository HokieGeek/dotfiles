package com.jdsu.tools.PMDataHandler;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Array;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

public class PassiveDataParser {
	// Member Variables
	private static ArrayList m_necompIds = null;
	private static String m_target_prefix = null;
	private static Connection m_db = null;
	private static int m_studyModelId = -1;
	protected static Hashtable m_sqlBlocksMap = null;
	private static final String END_TAG = "_END_OF_STRUCTURED_DATA";

	// SQL Block Strings
	private static final String GetDslamByCust = "GetDslamFromCustomerID:";
	private static final String GetCompTypeIds = "GetCompTypeIds:";
	private static final String AddNEComp = "AddNEComp:";
	private static final String GetStudyModelId = "GetStudyModel:";
	private static final String GetCollectorId = "GetCurrCollectorID:";
	private static final String CollectorExists = "CollectorExists:";
	private static final String AddCollector = "AddCollector:";
	private static final String AddStudy = "AddStudy:";
	private static final String AddDedicated = "AddDedicated:";

	PassiveDataParser(Hashtable sqlBlocks, String prefix) 
	{
		// Initialize member variables
		m_sqlBlocksMap = sqlBlocks;
		m_target_prefix = prefix;
	}

	/**
	  *
    	  */
  	private static void connect() throws SQLException, ClassNotFoundException {
		final String username = "tdce";
		final String passwd = "linesize";

     		try { Class.forName("oracle.jdbc.driver.OracleDriver"); }
     		catch (ClassNotFoundException ex) { throw ex; }

     		try {
        		m_db = DriverManager.getConnection("jdbc:oracle:thin:@zinc:1521:tdce", username, passwd);
		}
     		catch (SQLException ex) { throw ex; }
  	}

	/**
	TODO: 	1) Read information past END_TAG
		2) Save header in 'data'
	*/
	public static Hashtable parseFile(String passiveFileName)
	{

		Hashtable data = new Hashtable();

		InputStream inS = null;
		String inputLine = null;
		boolean endFlag = false;
		String type_prefix = null;
		char[] t_prefix = new char[1];

		try {

			// Save the customer id as retrieved from the file name
			data.put("ID", 
				passiveFileName.substring(passiveFileName.lastIndexOf('/')+1, passiveFileName.indexOf('.')));

			//
			if (passiveFileName.startsWith(File.separator))
				inS = new FileInputStream(passiveFileName);
			else
            			inS = ClassLoader.getSystemResourceAsStream(passiveFileName);
	
			BufferedReader inR = new BufferedReader(new InputStreamReader(inS));

			while ((inputLine = inR.readLine()) != null) {
				if (inputLine.equals(END_TAG))	endFlag = true; //break;
				if (inputLine.length() <= 0) continue;

				if (endFlag && inputLine.startsWith("DSLAM")) {
					Hashtable dslam = new Hashtable();
					dslam.put("NAME", inputLine.substring(inputLine.indexOf(':')+1, inputLine.indexOf(',')));
					dslam.put("ID", inputLine.substring(inputLine.indexOf(',')+1));
					data.put("DSLAM", dslam);
					//System.out.println(dslam);
				}
				//if (endFlag) System.out.println(inputLine);
	
				if (inputLine.startsWith("TIMESTAMP")) {
					data.put("TIMESTAMP", inputLine.substring(inputLine.indexOf(':')+1).trim());
					continue;
				}

				t_prefix[0] = inputLine.charAt(0);
				type_prefix = new String(t_prefix);

				if (inputLine.charAt(1) == '/') {
					Hashtable header = new Hashtable();
					header.put("name", inputLine.substring(2, inputLine.indexOf(',')));
					header.put("values", inputLine.substring(inputLine.indexOf(',')+1));

					data.put(type_prefix, header);
					//data.put(inputLine.charAt(0), header);
				} else {
					//if (data.containsKey(inputLine.charAt(0))) {
					if (data.containsKey(type_prefix)) {
						Hashtable stats = new Hashtable();
						Hashtable Component = (Hashtable)data.get(type_prefix);
						//Hashtable Component = (Hashtable)data.get(inputLine.charAt(0));
						String labels = (String)Component.get("values");
						String values = inputLine.substring(2);
						int token_label = 0;
						int token_value = 0;
						int offset = 1;

						while (labels.length() > 0) {  
							//
							token_label = labels.indexOf(',');
							token_value = values.indexOf(',');
							if ((token_label <= 0) || (token_value <= 0)) {
								token_value = values.length();
								offset = 0;
							}
							if (token_label <= 0) {
								token_label = labels.length();
								offset = 0;
							}

							stats.put(labels.substring(0, token_label), values.substring(0, token_value));

							labels = labels.substring(token_label+offset);
							values = values.substring(token_value+offset);
						}

						Component.put("values", stats);
						data.put(type_prefix, Component);
						//data.put(inputLine.charAt(0), Component);
					}
				}
			}
		}
		catch (FileNotFoundException ex) {
			ex.printStackTrace(System.err);
                 	System.exit(-1);	
		}
		catch (IOException ex) {
			ex.printStackTrace(System.err);
                 	System.exit(-1);	
		}

		return data;
	}

	public static void parseData(Hashtable data)
	{
		// Local Variables
		PreparedStatement pStmt = null;
		Statement stmt = null;
		ResultSet rs = null;
		String dslam_name = null;
		int dslam_id = -1;
		String cust_id = null;
		PreparedStatement addCollector = null;
		PreparedStatement addStudy = null;

		try {
			// Initialize the DB
		 	if((m_db == null) || m_db.isClosed()) connect();

			// Get the ne_comp_type ids
			if (m_necompIds == null) {
				m_necompIds = new ArrayList(4);
				stmt = m_db.createStatement();
				rs = stmt.executeQuery((String)m_sqlBlocksMap.get(GetCompTypeIds));
				while (rs.next())
					m_necompIds.add(new Integer(rs.getInt(1)));
				rs.close();
				//System.out.println(m_necompIds);
			}
			
			// Determine customer ID and dslam information
			cust_id = (String)data.get("ID");
			dslam_name = (String)((Hashtable)data.get("DSLAM")).get("NAME");
			dslam_id = (new Integer((String)((Hashtable)data.get("DSLAM")).get("ID"))).intValue();

			/*pStmt = m_db.prepareStatement((String)m_sqlBlocksMap.get(GetDslamByCust));
			pStmt.setString(1, cust_id.replace('-', '/'));
			rs = pStmt.executeQuery();
			if (rs.next()) {
				dslam_name = rs.getString(1);
				dslam_id = rs.getInt(2);
			}
			rs.close();
			pStmt.close();*/
			//System.out.println(dslam_name+" ("+dslam_id+")");
				
			// Insert 4 NE_COMP rows
			//System.out.println("            adding ne_comp rows");
			pStmt = m_db.prepareStatement((String)m_sqlBlocksMap.get(AddNEComp));
			for (int i = 0; i < m_necompIds.size(); i++) {
				pStmt.setString(1, cust_id); // NAME
				pStmt.setInt(2, ((Integer)m_necompIds.get(i)).intValue()); // NE_COMP_TYPE_ID
				pStmt.setInt(3, dslam_id); // NE_ID
				pStmt.executeQuery();
			}
			pStmt.close();

			// Make sure that the collector doesn't exist yet
			//System.out.println("            checking if collector exists");
			pStmt = m_db.prepareStatement((String)m_sqlBlocksMap.get(CollectorExists));
			pStmt.setString(1, m_target_prefix+dslam_name);
			rs = pStmt.executeQuery();

			if (rs.next() && (rs.getInt(1) <= 0)) {
				// Get the study model id
				if (m_studyModelId == -1) {
					stmt = m_db.createStatement();
					rs = stmt.executeQuery((String)m_sqlBlocksMap.get(GetStudyModelId));
					if (rs.next())
						m_studyModelId = rs.getInt(1);
					rs.close();
					stmt.close();
				}

				// Determine the hostname
				String hostname = InetAddress.getLocalHost().getHostName();
				hostname = hostname.substring(0, hostname.indexOf('.'));
			
				// Add the collector data to the db [tables: collector, study, dedicated]
				//System.out.println("            adding collector");
				addCollector = m_db.prepareStatement((String)m_sqlBlocksMap.get(AddCollector));
				addCollector.setInt(1, dslam_id); // NE_ID
				addCollector.setString(2, m_target_prefix+dslam_name); // NAME
				addCollector.setString(3, hostname); // RPU_HOSTNAME 
				addCollector.executeQuery();
				addCollector.close();
				//System.out.println("            adding study");
				addStudy = m_db.prepareStatement((String)m_sqlBlocksMap.get(AddStudy));
				addStudy.setString(1, dslam_name+".cpm"); // NAME
				addStudy.setInt(2, m_studyModelId); // STUDY_MODEL_ID
				addStudy.setInt(3, dslam_id); // NE_ID
				addStudy.executeQuery();
				addStudy.close();
				//System.out.println("            adding dedicated");
				stmt = m_db.createStatement();
				stmt.executeQuery((String)m_sqlBlocksMap.get(AddDedicated));
				stmt.close();
			}
			rs.close();
			pStmt.close();
		}
		catch (ClassNotFoundException ex) {
			ex.printStackTrace(System.err);
			System.exit(-1);
		}
		catch (SQLException sqlex) {
		 	if (sqlex.getErrorCode() != 1) {
				sqlex.printStackTrace(System.err);
		 		System.exit(-1);
			} else {
				System.out.println("              Unique found");
				try {
					if (addCollector != null) addCollector.close();
					if (addStudy != null) addStudy.close();
					if (stmt != null) stmt.close();
					m_db.close(); // Cleaning up
				}
				catch (SQLException sqlex2) {
					sqlex2.printStackTrace(System.err);
		 			System.exit(-1);
				}
			}
		}
		catch (UnknownHostException uhex) {
		 	uhex.printStackTrace(System.err);
		 	System.exit(-1);
		}
	}
}
