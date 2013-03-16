import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;

public class EquipmentData implements Runnable {
	private static String m_sqlFile = "EquipmentData.sql";
	private static Hashtable m_sqlBlocksMap = null;
	private static Connection m_db = null;
	private static Hashtable m_RG_Types = new Hashtable();
	private static Hashtable m_STB_Types = new Hashtable();

	// SQL Block Strings
	private static final String GetCusts = "GetCusts:";
	private static final String GetCompTypeIds = "GetCompTypeIds:";
	private static final String AddEquipment = "AddEquipment:";
	private static final String Clear = "Clear:";


	private static void connect() throws SQLException, ClassNotFoundException {
		final String username = "tdce";
		final String passwd = "linesize";

		try { Class.forName("oracle.jdbc.driver.OracleDriver"); }
		catch (ClassNotFoundException ex) { throw ex; }
		try { m_db = DriverManager.getConnection("jdbc:oracle:thin:@zinc:1521:tdce", username, passwd); }
                catch (SQLException ex) { throw ex; }
	}

 	/** retrieve SQL query from file */
	private Hashtable createSQLBlocksMapFromFile(String sqlFilePath) throws Exception {
		Hashtable sqlHt = new Hashtable();

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

	private static void clear() {
		ArrayList list = new ArrayList();
		try {
			if((m_db == null) || m_db.isClosed()) connect();
			m_db.createStatement().executeQuery((String)m_sqlBlocksMap.get(Clear));
			m_db.commit();
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

	private static ArrayList[] getCusts() {
		ArrayList[] list = new ArrayList[2];
		list[0] = new ArrayList();
		list[1] = new ArrayList();
		try {
			// Initialize the DB
			if((m_db == null) || m_db.isClosed()) connect();
			
			Statement stmt = m_db.createStatement();
			ResultSet rs = stmt.executeQuery((String)m_sqlBlocksMap.get(GetCusts));
			while (rs.next()) {
				list[0].add(rs.getString(1));
				list[1].add(rs.getString(2));
			}
		}
		catch (ClassNotFoundException ex) {
			ex.printStackTrace(System.err);
                	System.exit(-1);
		}
		catch (SQLException sqlex) {
			sqlex.printStackTrace(System.err);
			System.exit(-1);
		}

		return list;
	}

	private static void addDeviceType(Hashtable deviceInfo, ArrayList custs, String compIDs, 
					  String cpeSuffix, ArrayList parentCPEs, double distribution) {
		// 
		int marker = ((int)Math.floor(custs.size() * distribution));

		try {
			// Initialize the DB
			if((m_db == null) || m_db.isClosed()) connect();
		
			String query = (String)m_sqlBlocksMap.get(AddEquipment)+"("+compIDs+") AND name = ?";
			PreparedStatement stmt = m_db.prepareStatement(query);

			Hashtable device = null;
			Iterator it = deviceInfo.keySet().iterator();
			for (int i = 0, device_count = -1; i < custs.size(); i++) {
				if (((i <= 0) || (i > marker)) && it.hasNext())
                                        device = (Hashtable)deviceInfo.get(it.next());
		
				//System.out.println(i+": ("+custs.get(i)+")["+compIDs+"] -> "+(String)device.get("VENDOR")+" "+(String)device.get("MODEL")+" ("+(String)device.get("VERSION")+") {"+(String)custs.get(i)+cpeSuffix+", "+parentCPEs.get(i)+"}");
			
				// 
				stmt.setString(1, (String)device.get("VENDOR")); 	// Vendor
				stmt.setString(2, (String)device.get("MODEL"));		// Model
				stmt.setString(3, (String)device.get("VERSION"));	// Version
				stmt.setString(4, (String)custs.get(i)+cpeSuffix);  	// CPE ID
				stmt.setString(5, (String)parentCPEs.get(i));		// Parent CPE ID
				stmt.setString(6, (String)custs.get(i));		// Customer
				stmt.executeQuery();
			}

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

	public void run() { 
		// Get the customers	
		ArrayList[] custs = getCusts();

		// Get the component type ids
		String compType_RGs = new String();
		String compType_STBs = new String();
		String name = null;
		try {
			// Initialize the DB
			if((m_db == null) || m_db.isClosed()) connect();

			Statement stmt = m_db.createStatement();
			ResultSet rs = stmt.executeQuery((String)m_sqlBlocksMap.get(GetCompTypeIds));
                        while (rs.next()) {
				name = (String)rs.getString(2);

				if (name.matches(".*RG.*"))
					compType_RGs += ((compType_RGs.length() > 0 ) ? "," : "") + rs.getString(1);
				else if (name.matches(".*STB.*")) 
					compType_STBs += ((compType_STBs.length() > 0 ) ? "," : "") + rs.getString(1);
			}
			
		}
		catch (ClassNotFoundException ex) {
			ex.printStackTrace(System.err);
			System.exit(-1);
		}
                catch (SQLException sqlex) {
			sqlex.printStackTrace(System.err);
			System.exit(-1);
		}

		// Develop list of Parent CPE IDs for STB
		ArrayList stbParents = new ArrayList();
		for (int i = 0; i < custs[0].size(); i++)
			stbParents.add((String)custs[0].get(i)+"_1");
		//System.out.println(stbParents);
		
		// 
		addDeviceType(m_RG_Types, custs[0], compType_RGs, "_1", custs[1], 0.25);
		addDeviceType(m_STB_Types, custs[0], compType_STBs, "_2", stbParents, 0.42);
	}

	public static void main(String[] args) {
		EquipmentData me = new EquipmentData();

		try {
			me.m_sqlBlocksMap = me.createSQLBlocksMapFromFile(me.m_sqlFile);
		} catch (Exception ex) {
			ex.printStackTrace(System.err);
			System.exit(-1);
		}

		if ((args.length > 0) && args[0].equals("-c")) {
			me.clear();
		} else {
			// RGs
			Hashtable tmp = new Hashtable();
			tmp.put("VENDOR", "Thomson");
			tmp.put("MODEL", "SpeedTouch 585i");
			tmp.put("VERSION", "13.4");
			me.m_RG_Types.put("RG_1", tmp);

			tmp = new Hashtable();
			tmp.put("VENDOR", "Philips");
			tmp.put("MODEL", "PH61227");
			tmp.put("VERSION", "24.1");
			me.m_RG_Types.put("RG_2", tmp);

		
			// STBs
			tmp = new Hashtable();
			tmp.put("VENDOR", "Thomson");
			tmp.put("MODEL", "DTI1500");
			tmp.put("VERSION", "5.63");
			me.m_STB_Types.put("STB_1", tmp);

			tmp = new Hashtable();
			tmp.put("VENDOR", "Philips");
			tmp.put("MODEL", "DTR7200");
			tmp.put("VERSION", "7.34b");
			me.m_STB_Types.put("STB_2", tmp);

			me.run();
		}
	}

}
