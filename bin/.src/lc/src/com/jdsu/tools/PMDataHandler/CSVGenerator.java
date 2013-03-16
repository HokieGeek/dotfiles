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
import java.sql.Array;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Hashtable;
import java.util.Random;
import java.util.TimeZone;

public class CSVGenerator {

  // Member Variables
  private Connection m_connection = null;
  private ArrayList m_status = new ArrayList(4);
  private ArrayList m_necompIds = new ArrayList(4);
  private String m_csv_target = "";
  protected Hashtable m_sqlBlocksMap = null;

  private static final String CSV_HEADER = "CBT Operations Customer PM Data File version 0001";
  private static final String CSV_END_TAG = "_END_OF_STRUCTURED_DATA";
	

  // SQL Block Strings
  private static final String GetCustomerPhoneNumbers = "GetCustomerPhoneNumbers:";

  CSVGenerator(String target, Hashtable sqlBlocks) throws Exception {
	m_csv_target = target;
	m_sqlBlocksMap = sqlBlocks;
	init();
  }

  /**
    *
    */
  public void init() throws Exception {
	//System.out.println("CSVGenerator: init()");

	// Initialize constant status array
	m_status.add("good");
	m_status.add("fair");
	m_status.add("poor");
	m_status.add("unknown");
	
	//try {
	// Make sure that the target location exists
	File target = new File(m_csv_target);
	if (!target.exists()) 
		target.mkdirs();

	/*}
	catch (ClassNotFoundException ex) {
		ex.printStackTrace(System.err);
		 System.exit(-1);
	}*/
  }

  private String getRandStatus() {
	int status = 0;
        int top = 100;
        int minGood = 65;
        int topGood = (new Random()).nextInt(25)+minGood;
        int topFair = topGood+((top-topGood)/2);
        int topPoor = topFair+((top-topGood)/4);
        int rand = (new Random()).nextInt(top);

	if (rand > topGood) {
                if (rand <= topFair) status = 1;
		else if (rand <= topPoor) status = 2;
		else status = 3;
	}

	return (String)m_status.get(status);
  }

  /**
    *
    */
  public void connect() throws SQLException, ClassNotFoundException {
     final String username = "tdce";
     final String passwd = "linesize";

     try { Class.forName("oracle.jdbc.driver.OracleDriver"); }
     catch (ClassNotFoundException ex) { throw ex; }

     try { m_connection = DriverManager.getConnection("jdbc:oracle:thin:@zinc:1521:tdce", username, passwd); }
     catch (SQLException ex) { throw ex; }
  }

  private String formatPhoneNum(String num) {
	return num.replace('/', '-');
  }

  public void generateCSVFiles(int max_customers) {
	Statement stmt = null;
	ResultSet rs = null;
	//System.out.println("max_customers: "+max_customers);
	try {
		// Initialize the DB
		if((m_connection == null) || m_connection.isClosed()) connect();
		
		// createCSV
		stmt = m_connection.createStatement();
		rs = stmt.executeQuery((String)m_sqlBlocksMap.get(GetCustomerPhoneNumbers));
		for (int i = 0; (i < max_customers) && rs.next(); i++) {
			//System.out.println(i+": "+rs.getString(1)+", "+rs.getString(2)+", "+rs.getInt(3));
			//if (rs.getString(1).equals("240-404-3136"))
			if (rs.getString(1).equals("201-200-0101") || rs.getString(1).equals("240-404-3136"))
				i--;
			else if (!createCSV(formatPhoneNum(rs.getString(1)), rs.getString(2), rs.getInt(3)))
				i--;
		}

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

  public boolean createCSV(String cust_phone, String dslam_name, int dslam_id) {
	String IPTV_VALUES = "packets,10000,discount,100,underr,50,overr,200";
	String LAN_VALUES = "bytes_s,1110,bytes_r,220,pkts_s,230,pkts_r,140,cells_s,330,cells_r,440,wlan_st,GOOD,wlan_mbr,30000";
	String WAN_VALUES = "rblocks,430,tblocks,130,celldel,10,linkret,3,initerr,4,initto,6,lof,0,errsec,100,serrsec,51,fecerr,1,atucfe,14,hecerr,155,atuchec,41,crcerr,3,atuccrc,3,datapath,interleaved,ucurrrate,3600,dcurrrate,128,upmaxrate,1200,dnmaxrate,3600,upnm,10,dnnm,11,upattn,16,dnattn,32,uppwr,30,dnpwr,31,totstart,3334,showstart,111";

	try {
		if (new File(m_csv_target+cust_phone+".csv").exists())
			return false;
	
		FileWriter outFile = new FileWriter(m_csv_target+cust_phone+".csv");
		BufferedWriter out = new BufferedWriter(outFile);
		Calendar cal = Calendar.getInstance();
		/*String TIMESTMP = ((Integer)cal.get(Calendar.YEAR)).toString()
				  +((Integer)cal.get(Calendar.MONTH)).toString()
                                  +((Integer)cal.get(Calendar.DAY_OF_MONTH)).toString()
				  +((Integer)cal.get(Calendar.HOUR_OF_DAY)).toString()
                                  +((Integer)cal.get(Calendar.MINUTE)).toString()+"  "
				  +cal.getTimeZone().getDisplayName(true, TimeZone.SHORT);*/

		/*String month = ((Integer)cal.get(Calendar.MONTH)).toString();
		String day = ((Integer)cal.get(Calendar.DAY_OF_MONTH)).toString();
		String hour = ((Integer)cal.get(Calendar.HOUR_OF_DAY)).toString();
		String minute = ((Integer)cal.get(Calendar.MINUTE)).toString();*/

		String month = (new Integer(cal.get(Calendar.MONTH))).toString();
		String day = (new Integer(cal.get(Calendar.DAY_OF_MONTH))).toString();
		String hour = (new Integer(cal.get(Calendar.HOUR_OF_DAY))).toString();
		String minute = (new Integer(cal.get(Calendar.MINUTE))).toString();
		String second = (new Integer(cal.get(Calendar.SECOND))).toString();
		if (month.length() < 2) month = "0"+month;
		if (day.length() < 2) day = "0"+day;
		if (hour.length() < 2) hour = "0"+hour;
		if (minute.length() < 2) minute = "0"+minute;
		if (second.length() < 2) second = "0"+second;

		//String TIMESTMP = ((Integer)cal.get(Calendar.YEAR)).toString()+month+day+hour+minute
		//Integer TZ_OFFSET = new Integer((new Integer(cal.get(Calendar.ZONE_OFFSET))).intValue / -3600000);
		//Integer TZ_OFFSET = new Integer(cal.get(Calendar.ZONE_OFFSET) / -3600000);
		String TIMESTMP = (new Integer(cal.get(Calendar.YEAR))).toString()+month+day+hour+minute
				  +" "+cal.getTimeZone().getDisplayName(false, TimeZone.SHORT)
				  +(new Integer(cal.get(Calendar.ZONE_OFFSET) / -3600000)).toString()
				  +cal.getTimeZone().getDisplayName(true, TimeZone.SHORT);
				  //+TZ_OFFSET.toString()

		//
		int kpi_weight = (new Random()).nextInt(100);
	
		// Write the file header
		out.write(CSV_HEADER+"\n");
		out.write("TIMESTAMP: "+TIMESTMP+"\n\n");

		// Write the data headers
		out.write("I/IPTV Stats,customer_id,timestamp,keys_and_values\n");
		out.write("L/LAN Stats,customer_id,timestamp,keys_and_values\n");
		out.write("W/WAN Stats,customer_id,timestamp,keys_and_values\n");
		
		/*TIMESTMP = ((Integer)cal.get(Calendar.YEAR)).toString()+"/"
			   +((Integer)cal.get(Calendar.MONTH)).toString()+"/"
			   +((Integer)cal.get(Calendar.DAY_OF_MONTH)).toString()+"/ "
			   +((Integer)cal.get(Calendar.HOUR_OF_DAY)).toString()+":"
			   +((Integer)cal.get(Calendar.MINUTE)).toString()+":"
			   +((Integer)cal.get(Calendar.SECOND)).toString();*/
		TIMESTMP = (new Integer(cal.get(Calendar.YEAR))).toString()+"/"+month+"/"+day+" "+hour+":"+minute+":"+second;

		out.write("I,"+cust_phone+","+TIMESTMP+","+IPTV_VALUES+",kpithv,"+getRandStatus()+",kpi_weight,"+kpi_weight+"\n");
		out.write("L,"+cust_phone+","+TIMESTMP+","+LAN_VALUES+",kpithv,"+getRandStatus()+",kpi_weight,"+kpi_weight+"\n");
		out.write("W,"+cust_phone+","+TIMESTMP+","+WAN_VALUES+",kpithv,"+getRandStatus()+",kpi_weight,"+kpi_weight+"\n\n");

		out.write(CSV_END_TAG+"\n\n");
		out.write("DSLAM:"+dslam_name+","+dslam_id+"\n");
		
		out.close();
	}
	catch (IOException ioex) {
		ioex.printStackTrace(System.err);
                System.exit(-1);
	}

	return true;
  }
}
