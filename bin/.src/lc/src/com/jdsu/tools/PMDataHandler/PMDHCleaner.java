package com.jdsu.tools.PMDataHandler;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.sql.Connection; 
import java.sql.DriverManager; 
import java.sql.SQLException; 
import java.sql.Statement; 
import java.sql.PreparedStatement; 
import java.sql.ResultSet; 
import java.util.Hashtable;

public class PMDHCleaner implements Runnable {

  // Member Variables
  private static Connection m_connection = null;
  private static final String m_sqlFile = "pmdh_cleanup.sql";
  private static Hashtable m_sqlBlocksMap = null;
  private static String m_pm_target = null;

  // SQL Block Strings
  private static final String delMeasurements = "DelMeasurements:";
  private static final String delComponents = "DelComponents:";
  private static final String delDedicateds = "DelDedicateds:";
  private static final String delStudyStatuses = "DelStudyStatuses:";
  private static final String delStudies = "DelStudies:";
  private static final String delCollectors = "DelCollectors:";
  private static final String delNeComps = "DelNeComps:";

  	public void run() { }

  	public static void clean() throws Exception{
		Statement stmt = null;

		try {

			//if (m_pm_target != null) {
				
			//}
	
			// Load the sql
			if (m_sqlBlocksMap == null)
				m_sqlBlocksMap = createSQLBlocksMapFromFile(m_sqlFile);

			// Initialize the DB
			if((m_connection == null) || m_connection.isClosed()) connect();
		
			// 
			stmt = m_connection.createStatement();
			stmt.executeQuery((String)m_sqlBlocksMap.get(delMeasurements));
			stmt.executeQuery((String)m_sqlBlocksMap.get(delComponents));
			stmt.executeQuery((String)m_sqlBlocksMap.get(delDedicateds));
			stmt.executeQuery((String)m_sqlBlocksMap.get(delStudyStatuses));
			stmt.executeQuery((String)m_sqlBlocksMap.get(delStudies));
			stmt.executeQuery((String)m_sqlBlocksMap.get(delCollectors));
			stmt.executeQuery((String)m_sqlBlocksMap.get(delNeComps));

			m_connection.close();
		}
		catch (ClassNotFoundException ex) {
		 	ex.printStackTrace(System.err);
		 	System.exit(-1);
		}
		catch (SQLException sqlex) {
		 	sqlex.printStackTrace(System.err);
		 	System.exit(-1);
		}
  	}

	public static void main(String[] args) throws Exception {
		if (args.length > 0) m_pm_target = args[0];
		PMDHCleaner.clean();
	}

  	private static void connect() throws SQLException, ClassNotFoundException {
     		final String username = "tdce";
     		final String passwd = "linesize";

     		try { Class.forName("oracle.jdbc.driver.OracleDriver"); }
     		catch (ClassNotFoundException ex) { throw ex; }

     		try { m_connection = DriverManager.getConnection("jdbc:oracle:thin:@zinc:1521:tdce", username, passwd); }
     		catch (SQLException ex) { throw ex; }
  	}


	/** retrieve SQL queries from file */
	private static Hashtable createSQLBlocksMapFromFile(String sqlFilePath) throws Exception {
		Hashtable sqlHt =  new Hashtable() ;
	
       		InputStream inS;
		String inputLine;
		if (sqlFilePath.startsWith(File.separator))
                        inS = new FileInputStream(sqlFilePath);
		else
                   	inS = ClassLoader.getSystemResourceAsStream(sqlFilePath);
	
       		BufferedReader inR = new BufferedReader(new InputStreamReader(inS));

		boolean blockBeginFlag = false ;
		String blockName = "" ;
		String SQLStmnt = "" ;
		while ((inputLine=inR.readLine())!=null) {
                        if( inputLine.trim().startsWith("--")) continue;

			if( inputLine.indexOf("DATA_QRY_BLOCK_BEGIN") >=0 ){
					blockName = inputLine.substring(((String)"DATA_QRY_BLOCK_BEGIN").length()).trim() ;
				 blockBeginFlag = true ;
				 //System.out.println("BLOCK_NAME:"+blockName );
                                continue;
                        }

			if( inputLine.indexOf("DATA_QRY_BLOCK_END") >=0 ) {
					if( !blockName.equals("") &&  !SQLStmnt.equals("") )
                               		sqlHt.put(blockName,SQLStmnt) ;
					blockBeginFlag = false ;
				 blockName = "" ;
				 SQLStmnt ="" ;
                                continue ;
                        }

			if( blockBeginFlag ){
					SQLStmnt +=inputLine ;
				 SQLStmnt +="\n" ;
                        }
		}
                return sqlHt;
	}
}
