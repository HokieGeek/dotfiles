package com.jdsu.tools.KPIAggregator;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import com.jdsu.beans.DataSet;
import com.jdsu.interfaces.Component;
import com.jdsu.interfaceImpl.ComponentImpl;
import com.jdsu.helpers.CbtOpsHelper;

public class KPIAggregator {
	// Member Variables
  	String m_sqlFile = "KPIAggregator.sql";
  	protected Hashtable m_sqlBlocksMap = null;
	private Connection m_db = null;
	private ArrayList m_status = new ArrayList();
	private ArrayList m_NELevels = new ArrayList();
	private ArrayList m_NECompTypes = new ArrayList();
	private Hashtable m_virtualTypeIds = null;
	private Hashtable m_vneSQL = new Hashtable();
	private int m_KPICompTypeID = 0;

	private static final String KPI_COLUMN_NAME = "KPI Threshold Violation";

	// SQL Block Strings
	private static final String GetCustomers = "GetCustomers:";
	private static final String GetNECompTypes = "GetNECompTypes:";

	private static final String AddVirtualType = "AddVirtualType:";
	private static final String AddVirtualTypeVersion = "AddVirtualTypeVersion:";
	private static final String RemoveVirtualType = "RemoveVirtualType:";
	private static final String RemoveVirtualTypeVersion = "RemoveVirtualTypeVersion:";
	private static final String GetCurrNETypeSequence = "GetCurrNETypeSequence:";
	private static final String GetVirtualTypeID = "GetVirtualTypeID:";

	private static final String AddVirtualNERegion = "AddVirtualNERegion:";
	private static final String AddVirtualNEState = "AddVirtualNEState:";
	private static final String AddVirtualNEDistrict = "AddVirtualNEDistrict:";
	private static final String AddVirtualNELata = "AddVirtualNELata:";
	private static final String AddVirtualNELocation = "AddVirtualNELocation:";
	private static final String AddKPIValues = "AddKPIValues:";

	private static final String GetCompTypeKPIID = "GetCompTypeKPIID:";
	private static final String GetCurrNESequence = "GetCurrNESequence:";
	private static final String GetDSLAMID = "GetDSLAMID:";

	/**
    	  *
    	  */
	private void connect() throws SQLException, ClassNotFoundException {
		final String username = "tdce";
		final String passwd = "linesize";

     		try {
       			Class.forName("oracle.jdbc.driver.OracleDriver");
		}
     		catch (ClassNotFoundException ex) { throw ex; }

     		try {
			m_db = DriverManager.getConnection(
           			"jdbc:oracle:thin:@zinc:1521:tdce", username, passwd);
     		}
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

	public void GetVirtualTypeIDs() {
                PreparedStatement pStmt = null;
                ResultSet rs = null;

                try {
			// Initialize DB
			if((m_db == null) || m_db.isClosed()) connect();

			pStmt = m_db.prepareStatement((String)m_sqlBlocksMap.get(GetVirtualTypeID));

                        for (int i = 0; i < m_NELevels.size(); i++) {
                                pStmt.setString(1, (String)m_NELevels.get(i));
                                rs = pStmt.executeQuery();
                                if (rs.next()) {
                        		if (m_virtualTypeIds == null) 
						m_virtualTypeIds = new Hashtable();
                                        m_virtualTypeIds.put((String)m_NELevels.get(i), rs.getInt(1));
				}
                        }

			if (m_virtualTypeIds == null)
				setupVirtualNETypes();
                }
		catch (ClassNotFoundException ex) {
			ex.printStackTrace(System.err);
			//System.exit(-1);
		}
                catch (SQLException sqlex) {
			sqlex.printStackTrace(System.err);
			//System.exit(-1);
		}
	}

	public void setupVirtualNETypes() {
		String sql = null;
		PreparedStatement pStmt = null;
		Statement stmt = null;
		ResultSet rs = null;

		int VirtualTypeID = 0;

		try {
                        // Initialize DB
			if((m_db == null) || m_db.isClosed()) connect();

			// Add the Virtual NE Types
			String szAddVirtualType = (String)m_sqlBlocksMap.get(AddVirtualType);
			String szAddVirtualTypeVersion = (String)m_sqlBlocksMap.get(AddVirtualTypeVersion);
			PreparedStatement addVirtualType = null;
			PreparedStatement addVirtualTypeVersion = null;
			stmt = m_db.createStatement();

			for(int i = 0; i < m_NELevels.size(); i++) {
				//id = VirtualTypeBaseID+i;
				System.out.println("	 ["+m_NELevels.get(i)+"]");

				// Add the virtual type
				addVirtualType = m_db.prepareStatement(szAddVirtualType);
				addVirtualType.setString(1, (String)m_NELevels.get(i));

				System.out.println("	  Adding ne_type");
				addVirtualType.executeQuery();

				// Determine what the id for the type is
				rs = stmt.executeQuery((String)m_sqlBlocksMap.get(GetCurrNETypeSequence));
				if (rs.next())
                                        VirtualTypeID = rs.getInt(1);

					// Add the corresponding virtual type version
					addVirtualTypeVersion = m_db.prepareStatement(szAddVirtualTypeVersion);
					addVirtualTypeVersion.setInt(1, VirtualTypeID);

				System.out.println("	  Adding ne_type_version");
                       		addVirtualTypeVersion.executeQuery();
                    	}

			// Populate the member variable that holds these ids
			GetVirtualTypeIDs();

			m_db.close();
		}
                catch (ClassNotFoundException ex) {
			ex.printStackTrace(System.err);
			 //System.exit(-1);
		}
                catch (SQLException sqlex) {
			sqlex.printStackTrace(System.err);
			 //System.exit(-1);
		}
	}

	private Hashtable addLevelKPI(Hashtable data, String[] levels, Hashtable counts, String countKey) {
		Hashtable level = null;
		Integer kpiStatus = null;
		String statusType = null;
                Hashtable kpiCounts = null;
		Hashtable levelKPI = null;

		Hashtable top = data;

		//System.out.println("  addLevelKPI("+data+","+levels.length+","+counts+","+countKey+")");
		//System.out.println("     addLevelKPI("+levels.length+","+counts+","+countKey+")");

		for (int i = 0; i < levels.length; i++) {

			levelKPI = (Hashtable)counts.clone();
	
			//System.out.println(") "+i);
			if (top.containsKey(levels[i])) {
				level = (Hashtable)top.get(levels[i]);
				//System.out.println(")) "+levels[i]);

				if (level.containsKey("KPI"))
					kpiCounts = (Hashtable)level.get("KPI");
				else
					kpiCounts = new Hashtable();	


				//System.out.println("))) kpiCounts = "+kpiCounts);

				if (kpiCounts.containsKey(countKey)) {
                                	levelKPI = (Hashtable)kpiCounts.get(countKey);
					//System.out.println(")))) levelKPI = "+levelKPI);
	
				 	for (Iterator statusTypes = counts.keySet().iterator(); statusTypes.hasNext(); ) {
                                        	statusType = (String)statusTypes.next();
						//System.out.print("))))) "+statusType+": ");

						if (levelKPI.containsKey(statusType))
                                                	kpiStatus = (Integer)levelKPI.get(statusType);
				        	else
						        kpiStatus = new Integer(0);

						//System.out.println(kpiStatus);

                                		levelKPI.put(statusType, kpiStatus+((Integer)counts.get(statusType)));
					}
					//System.out.println(")))) levelKPI = "+levelKPI);
				}
			} else {
                        	kpiCounts = new Hashtable();
				level = new Hashtable();
			}

			//System.out.println("))))* levelKPI = "+levelKPI);

			kpiCounts.put(countKey, levelKPI);
			level.put("KPI", kpiCounts);
			top.put(levels[i], level);
			top = level;
		}
		//System.out.println("**** DONE ****");

		return top;
	}

	private void insertKPIValues(String levelType, ArrayList levels, String vneSql, Hashtable counts, Integer neCount) {
		Statement stmt = null;
		PreparedStatement addVirtualNE = null;
		PreparedStatement addKPIValues = null;
		ResultSet rs = null;

		int pos = 0;
		int pos_base = 3;
		int virtualNEID = 0;
		Hashtable typeCounts = null;

		//System.out.println("  insertKPIValues("+levelType+","+levels+","+vneSql+","+counts+","+neCount+")");

		try {
			// Initialize
			if ((m_db == null) || m_db.isClosed()) connect();
			if (m_db.isReadOnly()) {
				System.out.println("Error: DB connection is Read Only");
				return;
			}


			if (vneSql != null) {
	
				// Add the Virtual NE
				addVirtualNE = m_db.prepareStatement((String)m_sqlBlocksMap.get(vneSql));
				//addVirtualNE.setString(1, "AFP_"+levelType+"_"+neCount.toString()); // NAME
				addVirtualNE.setString(1, levelType+"_"+neCount.toString()); // NAME
				addVirtualNE.setInt(2, ((Integer)m_virtualTypeIds.get(levelType)).intValue()); // NE_TYPE_VERSION_ID
				for (int i = 0; i < levels.size(); i++) {
					addVirtualNE.setString(pos_base, (String)levels.get(i));
					pos_base++;
				}
				rs = addVirtualNE.executeQuery();
				if (rs != null) rs.close();
				System.out.print("Added Virtual NE #");

				// Determine the ID of the NE just added
				stmt = m_db.createStatement();
				if (stmt != null) rs = stmt.executeQuery((String)m_sqlBlocksMap.get(GetCurrNESequence));
			} else {
				// Determine the ID of the NE just added
				PreparedStatement getNEID = m_db.prepareStatement((String)m_sqlBlocksMap.get(GetDSLAMID));
				String tmp = null;
				for (int i = 1; (getNEID != null) && (i <= levels.size()); i++)
					getNEID.setString(i, (String)levels.get(i-1));
				if (getNEID != null) rs = getNEID.executeQuery();
				System.out.print("Found Virtual NE #");
				
			}
			if ((rs != null) && rs.next()) {
				virtualNEID = rs.getInt(1);
				System.out.println(virtualNEID);
			}	
			if (rs != null) rs.close();
			//System.out.println(virtualNEID);

			// Add the KPI values to the NE_COMP table
			addKPIValues = m_db.prepareStatement((String)m_sqlBlocksMap.get(AddKPIValues));
			//addKPIValues.setString(1, "AFP_"+levelType+"_"+neCount.toString()); // NAME
			addKPIValues.setString(1, levelType+"_"+neCount.toString()); // NAME
			addKPIValues.setInt(2, m_KPICompTypeID); // NE_COMP_TYPE_ID
			addKPIValues.setInt(3, virtualNEID); // NE_ID (Just created)

			pos_base = 4;
			int i = 0;
			for (Iterator compTypes = counts.keySet().iterator(); compTypes.hasNext(); i++) {
				typeCounts = (Hashtable)counts.get(compTypes.next());
				pos = pos_base+(m_status.size()*i);
				for (int j = 0; j < m_status.size(); j++) 
					addKPIValues.setInt((pos+j), ((Integer)typeCounts.get((String)m_status.get(j))).intValue());
			}
			addKPIValues.executeQuery();
			System.out.println("Added KPI Values to NE_COMP table");
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

	public Integer parseKPIData(Hashtable data, int level, int count, ArrayList lvls)  {
		String levelType = null;
		String currLevel = null;
		ArrayList vneHierarchy = (lvls == null) ? new ArrayList() : (ArrayList)lvls.clone();

		if (level >= m_NELevels.size()) return count;

		for (Iterator levels = data.keySet().iterator(); levels.hasNext(); ) {
			levelType = (String)m_NELevels.get(level);
			currLevel = (String)levels.next();
			
			//
			if (currLevel.equals("KPI")) continue;
		
			// 
			if (level < vneHierarchy.size())
                        	vneHierarchy.remove(vneHierarchy.size()-1);
                        vneHierarchy.add(currLevel);

                        /*for(int sp = 0; sp < level; sp++)
                        	System.out.print(" ");
			System.out.println(currLevel);*/
			//System.out.println((Hashtable)((Hashtable)data.get(currLevel)).get("KPI"));

                        insertKPIValues(levelType, vneHierarchy, (String)m_vneSQL.get(levelType),
					  (Hashtable)((Hashtable)data.get(currLevel)).get("KPI"), count);
			count = parseKPIData((Hashtable)data.get(currLevel), level+1, count+1, vneHierarchy);
		}

                return count;
	}


	public void storeAllKPIValues() {
		// Local Variables
		Hashtable kpiData = new Hashtable();
		Hashtable kpiCount = new Hashtable();
		CbtOpsHelper cbtopsHelper = new CbtOpsHelper();
		Statement stmt = null;
		ResultSet custList = null;
		DataSet custData = null;
		List<Map<String, Object>> custInfoList = null;
		Map<String, Object> customer = null;
		String currNECompType = null;

		Integer neCount = 0;
		String levelType = null;
		String[] levels = new String[m_NELevels.size()];
		Hashtable level = null;

		try {
			// Initialize
			if ((m_db == null) || m_db.isClosed()) connect();
			if (m_db.isReadOnly()) {
				System.out.println("Error: DB connection is Read Only");
				return;
			}

			// Get the list of dslams
			stmt = m_db.createStatement();
			custList = stmt.executeQuery((String)m_sqlBlocksMap.get(GetCustomers));

			// Get KPI information for all customers per dslam
			while (custList.next()) {
				//System.out.println(">> "+custList.getString(1));
				custData = cbtopsHelper.getCustomerData(custList.getString(1), null); //

			   	// Retrieve KPI info for each comp type
			   	for (int i = 0; i < m_NECompTypes.size(); i++) {
			       		currNECompType = (String)m_NECompTypes.get(i);
					//System.out.print(">>> "+currNECompType+": ");

					custInfoList = (List<Map<String, Object>>)custData.get(currNECompType);
					customer = (Map<String, Object>)custInfoList.get(0);

			        	// If there is data, then count all the values for the dslam
					for (int j = 0; j < m_status.size(); j++)
						kpiCount.put((String)m_status.get(j), new Integer(0));

					//System.out.println(" "+customer.get(KPI_COLUMN_NAME));	
					kpiCount.put(((String)customer.get(KPI_COLUMN_NAME)).toLowerCase(), new Integer(1));

					// Add counted values to each level that the dslam is in
					/////////////////
					for (int j = 0; j < m_NELevels.size(); j++) 
						levels[j] = custList.getString(j+2);

					//System.out.println(">>>> "+kpiCount);
					addLevelKPI(kpiData, levels, kpiCount, currNECompType);
				}
			}
			custList.close();
			stmt.close();

			System.out.println("============================================================");
			for (Iterator regions = kpiData.keySet().iterator(); regions.hasNext(); ) {
				String region = (String)regions.next();
				System.out.println(region+":: "+((Hashtable)kpiData.get(region)).get("KPI"));
				System.out.println();
			}
			System.out.println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
			//System.out.println(((Hashtable)kpiData.get(kpiData.keySet().iterator().next())).get("KPI"));
			System.out.println(kpiData);
			System.out.println("============================================================");

			// Take the parsed data and add it to the tables

			//
			neCount = parseKPIData(kpiData, 0, 0, null);
			//System.out.println("neCount = "+neCount);

			// Done
			m_db.close();
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

	KPIAggregator() throws Exception {
		// Populate the sql blocks hashtable
		m_sqlBlocksMap = createSQLBlocksMapFromFile(m_sqlFile);

		// Initialize the allowed statuses
		m_status.add("good");
		m_status.add("fair");
		m_status.add("poor");
		m_status.add("unknown");

		// Initialize the levels
		m_NELevels.add("REGION");
		m_NELevels.add("STATE");
		//m_NELevels.add("DISTRICT");
		m_NELevels.add("LATA");
		m_NELevels.add("LOCATION");
		m_NELevels.add("DSLAM");

		String lvl = null;
		for (int i = 0; i < m_NELevels.size(); i++) {
			lvl = (String)m_NELevels.get(i);  
			if (lvl.equals("REGION")) m_vneSQL.put(lvl, AddVirtualNERegion);
			else if (lvl.equals("STATE")) m_vneSQL.put(lvl, AddVirtualNEState);
			else if (lvl.equals("DISTRICT")) m_vneSQL.put(lvl, AddVirtualNEDistrict);
			else if (lvl.equals("LATA")) m_vneSQL.put(lvl, AddVirtualNELata);
			else if (lvl.equals("LOCATION")) m_vneSQL.put(lvl, AddVirtualNELocation);
		}

		Statement stmt = null;
		ResultSet rs = null;
		try {
			// Initialize
			if ((m_db == null) || m_db.isClosed()) connect();
			if (m_db.isReadOnly()) 
				System.out.println("Error: DB connection is Read Only");
			stmt = m_db.createStatement();

			// Determine what the KPI Comp Type ID is
			rs = stmt.executeQuery((String)m_sqlBlocksMap.get(GetCompTypeKPIID));
			if (rs.next())
				m_KPICompTypeID = rs.getInt(1);
			rs.close();

			// Retrieve the names of the status comp types we want
			rs = stmt.executeQuery((String)m_sqlBlocksMap.get(GetNECompTypes));
			while (rs.next())
				m_NECompTypes.add(rs.getString(1));
			rs.close();

			stmt.close();
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
}
