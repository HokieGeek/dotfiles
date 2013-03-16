package com.jdsu.cbt.performance;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.apache.log4j.Logger;

public class DslamPerformanceData {
	
	private long statusGood;
	private long statusMarginal;
	private long statusBad;
	private long statusUnknown;
	private String name;
	private long studies;
	private long level;
	
	private String dslamId; 
	
	public final static String sampleDslamIds[]=new String[]{
		"NYCMNY42-11110801C", "NYCMNY42-11110801C", "NYCMNY42-1111080F3", 
		"NYCMNY42-19800801B", "NYCMNY42-13210801F", "NYCMNY42-111108AB0", 
		"NYCMNY42-1111087F3", "AEDMNY42-11110867D", "NYCMNY42-111108AEE", 
		"NYCMNY42-198004823", "NYCMNY42-1321080Q2", "NYCMNY42-111108AMC", 
		"NYCMNY42-111101289", "AEDMNY42-11110867E", "NYCMNY42-111108A99", 
	};
	
	
	
	final static Random random = new Random(); 
	final static Map<String, List> dataContainer= new HashMap<String, List>(); 
	
	public DslamPerformanceData() {
		super();
	}
	
	
	public long getLevel() {
		return level;
	}


	public void setLevel(long level) {
		this.level = level;
	}


	public long getTotal(){
		Logger logger = Logger.getLogger(this.getClass());
		logger.debug("Doing Total"); 
		return this.statusGood + this.statusMarginal + this.statusBad + this.statusUnknown; 
	}
	
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	
	
	
	public long getStatusBad() {
		return statusBad;
	}


	public void setStatusBad(long statusBad) {
		this.statusBad = statusBad;
	}


	public long getStatusGood() {
		return statusGood;
	}


	public void setStatusGood(long statusGood) {
		this.statusGood = statusGood;
	}


	public long getStatusMarginal() {
		return statusMarginal;
	}


	public void setStatusMarginal(long statusMarginal) {
		this.statusMarginal = statusMarginal;
	}


	public long getStatusUnknown() {
		return statusUnknown;
	}


	public void setStatusUnknown(long statusUnknown) {
		this.statusUnknown = statusUnknown;
	}


	public long getStudies() {
		return studies;
	}


	public void setStudies(long studies) {
		this.studies = studies;
	}

	
	public String getDslamId() {
		return dslamId;
	}


	public void setDslamId(String dslamId) {
		this.dslamId = dslamId;
	}


	public static final List<DslamPerformanceData> getPerformanceData(int level, String  timeRange){
		String key = level + "|" + timeRange; 
		List<DslamPerformanceData> dataList = dataContainer.get(key); 
		if(dataList ==null) { 
			dataList= createDataList(timeRange);
			dataContainer.put(key, dataList);
		}
		
		return dataList; 
	}
	
	public static final List<DslamPerformanceData> createDataList(String timeRange){
		List<DslamPerformanceData> list = new ArrayList<DslamPerformanceData>();
		//this is for regions 
		
		int multiple= 2; 
		int upperLimit=3; 
		if("1".equals(timeRange)){
			multiple= 7; 
			upperLimit =3; 
		} else if("2".equals(timeRange)){
			multiple= 30; 
			upperLimit =4; 
		} else if("3".equals(timeRange)){
			multiple= 90; 
			upperLimit =6; 
		} else if("4".equals(timeRange)){
			multiple= 180; 
			upperLimit =9; 
		} else if("5".equals(timeRange)){
			multiple= 365; 
			upperLimit =11; 
		}
		
		int statusGoodTotal =0; 
		int statusMarginalTotal =0; 
		int statusBadTotal =0; 
		int statusUnknownTotal =0; 
		int studiesTotal =0; 
		
		DslamPerformanceData pki = null; 
		
		for(int i=0 ;  i<upperLimit; i++) { 
			pki = new DslamPerformanceData(); 
			pki.setStatusGood(random.nextInt(2100*multiple));
			statusGoodTotal += pki.getStatusGood();
			
			pki.setStatusMarginal(random.nextInt(400*multiple));
			statusMarginalTotal += pki.getStatusMarginal();
			
			pki.setStatusBad(random.nextInt(200*multiple));
			statusBadTotal += pki.getStatusBad();
			
			pki.setStatusUnknown(random.nextInt(300*multiple));
			statusUnknownTotal += pki.getStatusUnknown();
			
			pki.setDslamId(sampleDslamIds[i]);
			//pki.setName("Region" + random.nextInt(200));
			pki.setStudies(random.nextInt(50*multiple));
			studiesTotal += pki.getStudies(); 
			
			list.add(pki);
		}
		
	
		return list; 
	}	
	
	public static final List<DslamPerformanceData> getPerformanceDataByHierarchyId(int id){
		List<DslamPerformanceData> list = new ArrayList<DslamPerformanceData>();
		//this is for regions 
		
		int statusGoodTotal =0; 
		int statusMarginalTotal =0; 
		int statusBadTotal =0; 
		int statusUnknownTotal =0; 
		int studiesTotal =0; 
		
		DslamPerformanceData pki = null; 
		
		for(int i=0 ; id==0 && i<4; i++) { 
			pki = new DslamPerformanceData(); 
			pki.setStatusGood(random.nextInt(2100));
			statusGoodTotal += pki.getStatusGood();
			
			pki.setStatusMarginal(random.nextInt(400));
			statusMarginalTotal += pki.getStatusMarginal();
			
			pki.setStatusBad(random.nextInt(200));
			statusBadTotal += pki.getStatusBad();
			
			pki.setStatusUnknown(random.nextInt(300));
			statusUnknownTotal += pki.getStatusUnknown();
			
			
			//pki.setName("Region" + random.nextInt(200));
			pki.setStudies(random.nextInt(50));
			studiesTotal += pki.getStudies(); 
			
			switch (i){ 
				case 0: pki.setName("South");
				break; 
				case 1: pki.setName("North");
				break; 
				case 2: pki.setName("East");
				break; 
				case 3: pki.setName("West");
			} 
			
				list.add(pki);
		}
		//add total 
		pki = new DslamPerformanceData(); 
		pki.setName("Total");
		pki.setStatusBad(statusBadTotal);
		pki.setStatusGood(statusGoodTotal);
		pki.setStatusMarginal(statusMarginalTotal);
		pki.setStatusUnknown(statusUnknownTotal);
		pki.setStudies(studiesTotal);
		list.add(pki);
	
		return list; 
	}	

}
