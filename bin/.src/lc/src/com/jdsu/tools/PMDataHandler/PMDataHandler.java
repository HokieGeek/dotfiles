package com.jdsu.tools.PMDataHandler;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.Calendar;
import java.util.Hashtable;
import java.util.Properties;
import java.util.Random;
import java.util.TimeZone;

public class PMDataHandler implements Runnable {
	
	// Member Variables
	private Properties m_properties = new Properties();
	private static final String m_PropsFile = "PMDataHandler.properties";
	private static final String m_sqlFile = "PMDataHandler.sql";
	protected Hashtable m_sqlBlocksMap = null;

	/**
	  */
	public void run() {

		Calendar cal = null;

		try {	
			// Time 0
			cal = Calendar.getInstance();
			System.out.println(cal.get(Calendar.HOUR_OF_DAY)+":"+cal.get(Calendar.MINUTE)+":"+
					   cal.get(Calendar.SECOND)+"."+cal.get(Calendar.MILLISECOND));
			System.out.println("  Deleting old data");
			PMDHCleaner.clean();

			// Time 1
			cal = Calendar.getInstance();
			System.out.println(cal.get(Calendar.HOUR_OF_DAY)+":"+cal.get(Calendar.MINUTE)+":"+
					   cal.get(Calendar.SECOND)+"."+cal.get(Calendar.MILLISECOND));

			// Populate the sql blocks hashtable
			System.out.println("  Loading data");
			m_sqlBlocksMap = createSQLBlocksMapFromFile(m_sqlFile);

			// Load the properties
			/////////////////////////////////////
			m_properties.load(new FileInputStream(m_PropsFile));
			int max_customers = (new Integer(m_properties.getProperty("CSV_NUM_CUSTOMERS"))).intValue();
			String csv_target = m_properties.getProperty("CSV_LOCATION");
			if (csv_target.charAt(csv_target.length()-1) != '/') csv_target += "/";

			String pm_site = m_properties.getProperty("PM_TARGET_SITE");
			String pm_target_prefix = m_properties.getProperty("PM_TARGET_PREFIX");

			// Time 2
			cal = Calendar.getInstance();
			System.out.println(cal.get(Calendar.HOUR_OF_DAY)+":"+cal.get(Calendar.MINUTE)+":"+
					   cal.get(Calendar.SECOND)+"."+cal.get(Calendar.MILLISECOND));
			
			// Generate the csv files
			/////////////////////////////////////
			System.out.println("  Generating "+max_customers+" files");
			CSVGenerator csvgen = new CSVGenerator(csv_target, m_sqlBlocksMap);
			csvgen.generateCSVFiles(max_customers);
		
			// Time 3
			cal = Calendar.getInstance();
			System.out.println(cal.get(Calendar.HOUR_OF_DAY)+":"+cal.get(Calendar.MINUTE)+":"+
					   cal.get(Calendar.SECOND)+"."+cal.get(Calendar.MILLISECOND));

			// Read the csv files and retrieve the data
			/////////////////////////////////////
			System.out.println("  Parsing csv files");
			File csv_loc = new File(csv_target);
			File pm_target = null;
			String[] files = null;
			Hashtable data = null;
			String target_file = null;
			String timestamp = null;
			String collector_name = null;
			PassiveDataParser csvparser = new PassiveDataParser(m_sqlBlocksMap, pm_target_prefix);

			String inputLine = null;
                	Runtime r = Runtime.getRuntime();
			Process p = null;
			BufferedReader in = null;
			String[] move = new String[3];
			move[0] = "mv";
			String[] cmd = new String[3];
			cmd[0] = "/ada/tdce/bin/rft_scan";
			cmd[1] = "-c";
                	if (!(new File(cmd[0]).exists())) throw new FileNotFoundException(cmd[0]);

			if (csv_loc.exists() && csv_loc.isDirectory()) {
				System.out.println("      reading directory...");
				files = csv_loc.list();
				for (int i = 0; (files != null) && (i < files.length); i++) {
					System.out.println("          "+i+": "+files[i]);
					// Process the data
					//System.out.println("   reading...");
					data = csvparser.parseFile(csv_loc.getAbsolutePath()+"/"+files[i]);
					//System.out.println("   parsing...");
					csvparser.parseData(data);

					// Move the csv
					/*timestamp = (String)data.get("TIMESTAMP");
					timestamp = timestamp.substring(0, timestamp.indexOf(' ')).trim();
					collector_name = pm_target_prefix+(String)((Hashtable)data.get("DSLAM")).get("NAME");
					pm_target = new File(pm_site+"/"+collector_name+"/process");
					if (!pm_target.exists())
						pm_target.mkdirs();

					//System.out.println("   moving...");
					move[1] = csv_loc.getAbsolutePath()+"/"+files[i];
					move[2] = pm_target.getAbsolutePath()+"/passive."+timestamp;
					p = r.exec(move);
					//p.waitFor();
                			//in = new BufferedReader(new InputStreamReader(p.getInputStream()));
                			in = new BufferedReader(new InputStreamReader(p.getErrorStream()));
                			while ((inputLine = in.readLine()) != null) 
                        			System.out.println(inputLine);
					p.getInputStream().close();
					p.getOutputStream().close();
					p.getErrorStream().close();
					in.close();

					// RFT scan
					//System.out.println("   rft scan...");
					cmd[2] = collector_name;
					p = r.exec(cmd);
					p.waitFor();
					*/
                			/*in = new BufferedReader(new InputStreamReader(p.getInputStream()));
                			while ((inputLine = in.readLine()) != null) 
                        			System.out.println(inputLine);
					p.getInputStream().close();
					p.getOutputStream().close();
					p.getErrorStream().close();
					in.close();*/
				}
			}

			// Time 4
			cal = Calendar.getInstance();
			System.out.println(cal.get(Calendar.HOUR_OF_DAY)+":"+cal.get(Calendar.MINUTE)+":"+
					   cal.get(Calendar.SECOND)+"."+cal.get(Calendar.MILLISECOND));
		}
		catch (Exception ex) {
			ex.printStackTrace(System.err);
			System.exit(-1);
		}
	}

        public static void main(String[] args) throws Exception {
		PMDataHandler me = new PMDataHandler();
		me.run();
	}

	/** retrieve SQL queries from file */
  	private Hashtable createSQLBlocksMapFromFile(String sqlFilePath) throws Exception {
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

