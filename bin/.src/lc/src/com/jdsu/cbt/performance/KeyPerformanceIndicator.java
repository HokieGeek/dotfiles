package com.jdsu.cbt.performance;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.apache.log4j.Logger;

public class KeyPerformanceIndicator {
	
	private long statusGood;
	private long statusMarginal;
	private long statusBad;
	private long statusUnknown;
	private String name;
	private long studies;
	private long level;
	private boolean NotApplicable;
	
	private String dslamId;
	
	public final static String regions[]= {"West", "East", "North", "South"}; 
	
	
	public final static String sampleDslamIds[]=new String[]{"NYCMNY42-11110801C", "NYCMNY42-11110801C", "NYCMNY42-1111080F3", 
		"NYCMNY42-19800801B", "NYCMNY42-13210801F", "NYCMNY42-111108AB0", "NYCMNY42-1111087F3", "AEDMNY42-11110867",
	};
	
	
	
	final static Random random = new Random(); 
	final static Map<String, List> dataContainer= new HashMap<String, List>(); 
	
	public KeyPerformanceIndicator() {
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

	public void addStatusBad(long statusBad) {
		this.statusBad += statusBad;
	}
	public void addStatusGood(long statusGood) {
		this.statusGood += statusGood;
	}
	public void addStatusMarginal(long statusMarginal) {
		this.statusMarginal += statusMarginal;
	}
	public void addStatusUnknown(long statusUnknown) {
		this.statusUnknown += statusUnknown;
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

	

	


	public static final List<KeyPerformanceIndicator> getPerformanceData(int level, String  timeRange){
		//String key = level + "|" + timeRange; 
		List<KeyPerformanceIndicator> dataList = null; //dataContainer.get(key); 
	//	if(dataList ==null) { 
			dataList= createDataList(level, timeRange);
			for(int i=0; level==0 && i<dataList.size(); i++) { 
				KeyPerformanceIndicator pki = dataList.get(i);
			    pki.setLevel(level+1); 
				switch (i){ 
				case 0: pki.setName("South");
				break; 
				case 1: pki.setName("North");
				break; 
				case 2: pki.setName("East");
				break; 
				case 3: pki.setName("West");
				break; 
				default: ; 
				}
			}
	
			for(int i=0; level==1 && i<dataList.size(); i++) { 
				KeyPerformanceIndicator pki = dataList.get(i);
				 pki.setLevel(level+1); 
				switch (i){ 
				case 0: pki.setName("New Castle");
				break; 
				case 1: pki.setName("Hampshire");
				break; 
				case 2: pki.setName("Winchester");
				break; 
				case 3: pki.setName("Bermingham");
				break; 
				default: ; 
				}
			}
	
			for(int i=0; level==2 && i<dataList.size(); i++) { 
				KeyPerformanceIndicator pki = dataList.get(i);
				 pki.setLevel(level+1); 
				switch (i){ 
				case 0: pki.setName("Wire Center 1");
				break; 
				case 1: pki.setName("Wire Center 2");
				break; 
				case 2: pki.setName("Wire Center 3");
				break; 
				case 3: pki.setName("Wire Center 4");
				break; 
				default: ; 
				}
			}
			
			for(int i=0; level==3 && i<dataList.size(); i++) { 
				KeyPerformanceIndicator pki = dataList.get(i);
				pki.setLevel(level+1);
				pki.setName(sampleDslamIds[i]);
			}
	
		
		return dataList; 
	}
	
	public static final List<KeyPerformanceIndicator> createDataList(int level, String timeRange){
		List<KeyPerformanceIndicator> list = new ArrayList<KeyPerformanceIndicator>();
		//this is for regions 
		
		int multiple= 2; 
		if("1".equals(timeRange)){
			multiple= 7; 
		} else if("2".equals(timeRange)){
			multiple= 30; 
		} else if("3".equals(timeRange)){
			multiple= 90; 
		} else if("4".equals(timeRange)){
			multiple= 180; 
		} else if("5".equals(timeRange)){
			multiple= 365; 
		}
		
		int statusGoodTotal =0; 
		int statusMarginalTotal =0; 
		int statusBadTotal =0; 
		int statusUnknownTotal =0; 
		int studiesTotal =0; 
		
		int upperLimit =4; 
		if(level==4)
			upperLimit =sampleDslamIds.length; 
		
		KeyPerformanceIndicator pki = null; 
		
		for(int i=0 ;  i<upperLimit; i++) { 
			pki = new KeyPerformanceIndicator(); 
			pki.setStatusGood(random.nextInt(2100*multiple));
			statusGoodTotal += pki.getStatusGood();
			
			pki.setStatusMarginal(random.nextInt(400*multiple));
			statusMarginalTotal += pki.getStatusMarginal();
			
			pki.setStatusBad(random.nextInt(200*multiple));
			statusBadTotal += pki.getStatusBad();
			
			pki.setStatusUnknown(random.nextInt(300*multiple));
			statusUnknownTotal += pki.getStatusUnknown();
			
			pki.setDslamId(sampleDslamIds[i]);
			
			pki.setStudies(random.nextInt(50*multiple));			
			studiesTotal += pki.getStudies(); 
			
				list.add(pki);
		}
		//add total 
		pki = new KeyPerformanceIndicator(); 
		pki.setName("Total");
		pki.setStatusBad(statusBadTotal);
		pki.setStatusGood(statusGoodTotal);
		pki.setStatusMarginal(statusMarginalTotal);
		pki.setStatusUnknown(statusUnknownTotal);
		pki.setStudies(studiesTotal);
		list.add(pki);
	
		return list; 
	}	
	
	//this is used now
	public static final KeyPerformanceIndicator generateData(String level){
		
		int multiple= 2; 
		if("4".equals(level)){
			multiple= 7; 
		} else if("3".equals(level)){
			multiple= 30; 
		} else if("2".equals(level)){
			multiple= 90; 
		} else if("1".equals(level)){
			multiple= 180; 
		} else if("0".equals(level)){
			multiple= 365; 
		}
		
		
		KeyPerformanceIndicator pki = new KeyPerformanceIndicator();
		pki.setStatusGood(random.nextInt(2100 * multiple));

		pki.setStatusMarginal(random.nextInt(400 * multiple));

		pki.setStatusBad(random.nextInt(200 * multiple));

		pki.setStatusUnknown(random.nextInt(300 * multiple));

		pki.setStudies(random.nextInt(50 * multiple));
			
		return pki; 
	}


	public boolean getNotApplicable() {
		return NotApplicable;
	}


	public void setNotApplicable(boolean notApplicable) {
		NotApplicable = notApplicable;
	}


	
}
