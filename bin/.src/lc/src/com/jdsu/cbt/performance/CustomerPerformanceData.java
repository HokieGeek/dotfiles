package com.jdsu.cbt.performance;

import java.util.ArrayList;
import java.util.List;

public class CustomerPerformanceData {
	
	private String phoneNumber; 
	private String Comments; 
	private String studyIndicator; 
	private String latestLoopMeasureTime; 
	private String latestLanMeasureTime; 
	private String latestIptvMeasureTime; 
	
	private String latestVoipMeasureStatus; 
	private String latestLoopMeasureStatus; 
	private String latestLanMeasureStatus; 
	private String latestIptvMeasureStatus;
	
	private boolean voipNotApplicable; 
	
	


	public boolean isVoipNotApplicable() {
		return voipNotApplicable;
	}


	public void setVoipNotApplicable(boolean voipNotApplicable) {
		this.voipNotApplicable = voipNotApplicable;
	}


	public String getComments() {
		return Comments;
	}
	
	
	public String getLatestVoipMeasureStatus() {
		return latestVoipMeasureStatus;
	}


	public void setLatestVoipMeasureStatus(String latestVoipMeasureStatus) {
		this.latestVoipMeasureStatus = latestVoipMeasureStatus;
	}


	public void setComments(String comments) {
		Comments = comments;
	}
	public String getLatestIptvMeasureStatus() {
		return latestIptvMeasureStatus;
	}
	public void setLatestIptvMeasureStatus(String latestIptvMeasureStatus) {
		this.latestIptvMeasureStatus = latestIptvMeasureStatus;
	}
	public String getLatestIptvMeasureTime() {
		return latestIptvMeasureTime;
	}
	public void setLatestIptvMeasureTime(String latestIptvMeasureTime) {
		this.latestIptvMeasureTime = latestIptvMeasureTime;
	}
	public String getLatestLanMeasureStatus() {
		return latestLanMeasureStatus;
	}
	public void setLatestLanMeasureStatus(String latestLanMeasureStatus) {
		this.latestLanMeasureStatus = latestLanMeasureStatus;
	}
	public String getLatestLanMeasureTime() {
		return latestLanMeasureTime;
	}
	public void setLatestLanMeasureTime(String latestLanMeasureTime) {
		this.latestLanMeasureTime = latestLanMeasureTime;
	}
	public String getLatestLoopMeasureStatus() {
		return latestLoopMeasureStatus;
	}
	public void setLatestLoopMeasureStatus(String latestLoopMeasureStatus) {
		this.latestLoopMeasureStatus = latestLoopMeasureStatus;
	}
	public String getLatestLoopMeasureTime() {
		return latestLoopMeasureTime;
	}
	public void setLatestLoopMeasureTime(String latestLoopMeasureTime) {
		this.latestLoopMeasureTime = latestLoopMeasureTime;
	}
	public String getPhoneNumber() {
		return phoneNumber;
	}
	public void setPhoneNumber(String phoneNumber) {
		this.phoneNumber = phoneNumber;
	}
	public String getStudyIndicator() {
		return studyIndicator;
	}
	public void setStudyIndicator(String studyIndicator) {
		this.studyIndicator = studyIndicator;
	} 
	
	
	public static List<CustomerPerformanceData> getCriticalCustomers(String timeRange){ 
		ArrayList<CustomerPerformanceData> list = new ArrayList<CustomerPerformanceData>(); 
		
		CustomerPerformanceData data = new CustomerPerformanceData();
		data = new CustomerPerformanceData();
		data.setPhoneNumber("111-111-1111");
		data.setStudyIndicator("N");
		data.setComments("Looks like a wiring problem"); 
		data.setLatestLoopMeasureTime("05/03 5:16pm"); 
		data.setLatestLanMeasureTime("05/03 5:16pm"); 
		data.setLatestIptvMeasureTime("05/03 5:12pm"); 
		data.setLatestLoopMeasureStatus("statusBad");
		data.setLatestLanMeasureStatus("statusBad");
		data.setLatestIptvMeasureStatus("statusGood");
		data.setLatestVoipMeasureStatus("statusMarginal");
		
		list.add(data); 
		
		data = new CustomerPerformanceData();
		data.setPhoneNumber("222-222-2222");
		data.setStudyIndicator("N");
		data.setComments("Looks like a wiring problem"); 
		data.setLatestLoopMeasureTime("05/03 5:16pm"); 
		data.setLatestLanMeasureTime("05/03 5:16pm"); 
		data.setLatestIptvMeasureTime("05/03 5:12pm"); 
		data.setLatestLoopMeasureStatus("statusMarginal");
		data.setLatestLanMeasureStatus("statusBad");
		data.setLatestIptvMeasureStatus("statusBad");
		//data.setLatestVoipMeasureStatus("statusUnknown");
		data.setVoipNotApplicable(true);
		list.add(data);
		
	//	if("1".equals(timeRange)) return list; 
		
		
		data = new CustomerPerformanceData();
		data.setPhoneNumber("123-456-7890");
		data.setStudyIndicator("Y");
		data.setComments("problem"); 
		data.setLatestLoopMeasureTime("02/03 5:34pm"); 
		data.setLatestLanMeasureTime("02/03 5:34pm"); 
		data.setLatestIptvMeasureTime("02/03 5:34pm"); 
		data.setLatestLoopMeasureStatus("statusBad");
		data.setLatestLanMeasureStatus("statusGood");
		data.setLatestIptvMeasureStatus("statusUnknown");
		data.setLatestVoipMeasureStatus("statusUnknown");		
		list.add(data); 	
		
		data.setPhoneNumber("240-213-1343");
		data.setStudyIndicator("N");
		data.setComments("No Yellow or Red KPI THV's"); 
		data.setLatestLoopMeasureTime("06/03 4:14pm"); 
		data.setLatestLanMeasureTime("06/03 4:14pm"); 
		data.setLatestIptvMeasureTime("06/03 4:14pm"); 
		data.setLatestLoopMeasureStatus("statusGood");
		data.setLatestLanMeasureStatus("statusGood");
		data.setLatestVoipMeasureStatus("statusGood");
		data.setLatestIptvMeasureStatus("statusGood");
		
		list.add(data); 
		
		data = new CustomerPerformanceData();
		data.setPhoneNumber("240-213-2342");
		data.setStudyIndicator("N");
		data.setComments("Looks like a wiring problem"); 
		data.setLatestLoopMeasureTime("06/03 5:16pm"); 
		data.setLatestLanMeasureTime("06/03 5:16pm"); 
		data.setLatestIptvMeasureTime("06/03 5:12pm"); 
		data.setLatestLoopMeasureStatus("statusBad");
		data.setLatestLanMeasureStatus("statusBad");
		data.setLatestIptvMeasureStatus("statusBad");
		data.setLatestVoipMeasureStatus("statusBad");
		
		list.add(data); 
		
		data = new CustomerPerformanceData();
		data.setPhoneNumber("240-213-3344");
		data.setStudyIndicator("Y");
		data.setComments("Loop issue on down"); 
		data.setLatestLoopMeasureTime("06/03 7:08pm"); 
		data.setLatestLanMeasureTime("06/03 7:09pm"); 
		data.setLatestIptvMeasureTime("06/03 7:08pm"); 
		data.setLatestLoopMeasureStatus("statusBad");
		data.setLatestLanMeasureStatus("statusBad");
		data.setLatestIptvMeasureStatus("statusBad");
		data.setLatestVoipMeasureStatus("statusUnknown");
		
		list.add(data); 
		
	//	if("0".equals(timeRange)) return list; 
		
		
	
		
		data = new CustomerPerformanceData();
		data.setPhoneNumber("240-213-9921");
		data.setStudyIndicator("N");
		data.setComments("Could not get IPTV"); 
		data.setLatestLoopMeasureTime("01/03 5:08pm"); 
		data.setLatestLanMeasureTime("01/03 5:08pm"); 
		data.setLatestIptvMeasureTime("01/03 5:09pm"); 
		data.setLatestLoopMeasureStatus("statusMarginal");
		data.setLatestLanMeasureStatus("statusMarginal");
		data.setLatestIptvMeasureStatus("statusBad");
		data.setLatestVoipMeasureStatus("statusUnknown");
		
		list.add(data);
	
		data.setPhoneNumber("240-213-3456");
		data.setStudyIndicator("N");
		data.setComments("No Yellow or Red KPI THV's"); 
		data.setLatestLoopMeasureTime("06/03 14:14pm"); 
		data.setLatestLanMeasureTime("06/03 14:14pm"); 
		data.setLatestIptvMeasureTime("06/03 14:14pm"); 
		data.setLatestLoopMeasureStatus("statusGood");
		data.setLatestLanMeasureStatus("statusGood");
		data.setLatestIptvMeasureStatus("statusMarginal");
		data.setLatestVoipMeasureStatus("statusUnknown");
		
		list.add(data); 
		
		data = new CustomerPerformanceData();
		data.setPhoneNumber("240-213-1111");
		data.setStudyIndicator("N");
		data.setComments("Looks like a wiring problem"); 
		data.setLatestLoopMeasureTime("06/03 15:16pm"); 
		data.setLatestLanMeasureTime("06/03 15:16pm"); 
		data.setLatestIptvMeasureTime("06/03 15:12pm"); 
		data.setLatestLoopMeasureStatus("statusBad");
		data.setLatestLanMeasureStatus("statusBad");
		data.setLatestIptvMeasureStatus("statusBad");
		//data.setLatestVoipMeasureStatus("statusUnknown");
		data.setVoipNotApplicable(true);
		
		list.add(data); 
		
		data = new CustomerPerformanceData();
		data.setPhoneNumber("240-213-3333");
		data.setStudyIndicator("Y");
		data.setComments("Loop issue on down"); 
		data.setLatestLoopMeasureTime("06/03 17:08pm"); 
		data.setLatestLanMeasureTime("06/03 17:09pm"); 
		data.setLatestIptvMeasureTime("06/03 17:08pm"); 
		data.setLatestLoopMeasureStatus("statusBad");
		data.setLatestLanMeasureStatus("statusBad");
		data.setLatestIptvMeasureStatus("statusBad");
		data.setLatestVoipMeasureStatus("statusUnknown");
		
		list.add(data); 
		
		return list; 
	}
	
	
	

}
