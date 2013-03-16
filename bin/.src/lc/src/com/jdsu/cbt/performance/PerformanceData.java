package com.jdsu.cbt.performance;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

public class PerformanceData {

	private String name;
	private static  Map data;

	//Avoid null pointer exception 
	private KeyPerformanceIndicator loopKpi = new KeyPerformanceIndicator();
	private KeyPerformanceIndicator lanKpi = new KeyPerformanceIndicator();
	private KeyPerformanceIndicator iptvKpi = new KeyPerformanceIndicator();
	private KeyPerformanceIndicator voipKpi = new KeyPerformanceIndicator();
	
	public final static String regions[]= {"West", "East", "North", "South"}; 
	public final static String districts[]= {"New Castle",  "Manchester", "London"}; 
	public final static String wirecenters[]= {"WC101", "Salem Center", "Dumfries Center"}; 
	
	public final static String dslamIds[]=new String[]{"NYCMNY42-11110801C", "NYCMNY42-11110801C", "NYCMNY42-1111080F3", 
		"NYCMNY42-19800801B", "NYCMNY42-13210801F", "NYCMNY42-111108AB0", "NYCMNY42-1111087F3", "AEDMNY42-111108679"
	};
	
	public final static String customers[]= {"754-789-0098", "345-876-3214", "990-654-3245"}; 

	public KeyPerformanceIndicator getIptvKpi() {
		return iptvKpi;
	}



	public void setIptvKpi(KeyPerformanceIndicator iptvKpi) {
		this.iptvKpi = iptvKpi;
	}



	public KeyPerformanceIndicator getLanKpi() {
		return lanKpi;
	}



	public void setLanKpi(KeyPerformanceIndicator lanKpi) {
		this.lanKpi = lanKpi;
	}



	public KeyPerformanceIndicator getLoopKpi() {
		return loopKpi;
	}



	public void setLoopKpi(KeyPerformanceIndicator loopKpi) {
		this.loopKpi = loopKpi;
	}



	public String getName() {
		return name;
	}



	public void setName(String name) {
		this.name = name;
	}



	public KeyPerformanceIndicator getVoipKpi() {
		return voipKpi;
	}



	public void setVoipKpi(KeyPerformanceIndicator voipKpi) {
		this.voipKpi = voipKpi;
	}



	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}
	
	public static final Map getDataMap() { 
		if(data ==null) data = createData(); 			
		return data; 	
	}

	public static final Map  createData() { 
		data = new HashMap(); 
		data.put("0", createData("0")); // for all regions 
		data.put("1", createData("1")); // for all districts 
		data.put("2", createData("2")); // for all wire centers 
		data.put("3", createData("3")); // for all dslams 
		data.put("4", createData("4")); // for all customers 		
		return data;
		
	}
	
	public static final Map  createData(String level) { 
		Map  map = new LinkedHashMap(); 
		if ("0".equals(level)) {
			for (int i = 0; i < regions.length; i++) {
				map.put(regions[i], createWithName(regions[i], level));				
			}
		} else if ("1".equals(level)) {
			for (int i = 0; i < districts.length; i++) {
				map.put(districts[i], createWithName(districts[i], level));
			}
		} else if ("2".equals(level)) {
			for (int i = 0; i < wirecenters.length; i++) {
				map.put(wirecenters[i], createWithName(wirecenters[i], level));
			}
		} else if ("3".equals(level)) {
			for (int i = 0; i < dslamIds.length; i++) {
				PerformanceData kp = createWithName(dslamIds[i], level); 
			/*	if(i==1 || i==4)
						kp.getVoipKpi().setNotApplicable(true);*/
				map.put(dslamIds[i], kp);
			}
		} else if ("4".equals(level)) {
			for (int i = 0; i < customers.length; i++) {
				PerformanceData kp = createWithName(customers[i], level); 
			/*	if(i==0 ||  i==5)
						kp.getVoipKpi().setNotApplicable(true);*/
				map.put(customers[i],kp );
			}	
		}
		//No matter what level we are, create total before return 
		PerformanceData totalPki = createTotal(map); 
		map.put("Total", totalPki);
		
		return map; 
	} 
	
	public static PerformanceData createTotal(Map dataMap){
		PerformanceData totalPki = new  PerformanceData(); 
		Iterator keys = dataMap.keySet().iterator(); 
		while(keys.hasNext()) { 
			PerformanceData tempPki = (PerformanceData) dataMap.get(keys.next());
			totalPki.getLoopKpi().addStatusBad(tempPki.getLoopKpi().getStatusBad()); 
			totalPki.getLoopKpi().addStatusGood(tempPki.getLoopKpi().getStatusGood()); 
			totalPki.getLoopKpi().addStatusMarginal(tempPki.getLoopKpi().getStatusMarginal()); 
			totalPki.getLoopKpi().addStatusUnknown(tempPki.getLoopKpi().getStatusUnknown()); 
			totalPki.getLanKpi().addStatusBad(tempPki.getLanKpi().getStatusBad()); 
			totalPki.getLanKpi().addStatusGood(tempPki.getLanKpi().getStatusGood()); 
			totalPki.getLanKpi().addStatusMarginal(tempPki.getLanKpi().getStatusMarginal()); 
			totalPki.getLanKpi().addStatusUnknown(tempPki.getLanKpi().getStatusUnknown()); 
			totalPki.getIptvKpi().addStatusBad(tempPki.getIptvKpi().getStatusBad()); 
			totalPki.getIptvKpi().addStatusGood(tempPki.getIptvKpi().getStatusGood()); 
			totalPki.getIptvKpi().addStatusMarginal(tempPki.getIptvKpi().getStatusMarginal()); 
			totalPki.getIptvKpi().addStatusUnknown(tempPki.getIptvKpi().getStatusUnknown()); 
			totalPki.getVoipKpi().addStatusBad(tempPki.getVoipKpi().getStatusBad()); 
			totalPki.getVoipKpi().addStatusGood(tempPki.getVoipKpi().getStatusGood()); 
			totalPki.getVoipKpi().addStatusMarginal(tempPki.getVoipKpi().getStatusMarginal()); 
			totalPki.getVoipKpi().addStatusUnknown(tempPki.getVoipKpi().getStatusUnknown()); 
		}
		totalPki.setName("Total"); 
		
		return totalPki; 
	}
	
	public static PerformanceData createWithName(String name, String  level) { 
		PerformanceData pd = new PerformanceData(); 
		pd.setName(name);
		pd.setIptvKpi(KeyPerformanceIndicator.generateData(level));
		pd.setLoopKpi(KeyPerformanceIndicator.generateData(level));
		pd.setVoipKpi(KeyPerformanceIndicator.generateData(level));
		pd.setLanKpi(KeyPerformanceIndicator.generateData(level));
				
		return pd; 
	}

}
