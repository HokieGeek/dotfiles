package com.jdsu.beans;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Random;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class DataSubSet 
{
	
	/** Logger for this class and subclasses */
    protected static final Log logger = LogFactory.getLog(DataSubSet.class);
	/**The following are the required data subset to be displayed  **/
	 private static String [] lanDataSubSet ={"bytes_s","bytes_r","pkts_s","pkts_r","cells_s","cells_r","wlan_st","wlan_mbr"};
	 private static String [] wanDataSubSet ={"rblocks","tblocks","celldel","linkret","initerr","initto","lof","errsec",
				  							"serrsec","fecerr","atucfe","hecerr","atuchec","crcerr","atuccrc","datapath",
				  							"ucurrrate","dcurrrate","upmaxrate","dnmaxrate","upnm","dnnm","upattn","dnattn",
				  							"uppwr","dnpwr","testkey","totstart","showstart"};
	 private static String [] ipTVDataSubSet={"packets","discount","underr","overr"};
	
    /**
     * 
     */
    private static String wanStatus;
    private static String lanStatus;
    private static String ipTVStatus;
	 
   
	 /**
	 * @return
	 */
	public static String getWanStatus() {
		return DataSubSet.wanStatus;
	}
	/**
	 * @param wanStatus
	 */
	public static void setWanStatus(String wanStatus) {
		DataSubSet.wanStatus = wanStatus;
	}
	/**
	 * @return
	 */
	public static String getLanStatus() {
		return DataSubSet.lanStatus;
	}
	/**
	 * @param lanStatus
	 */
	public static void setLanStatus(String lanStatus) {
		DataSubSet.lanStatus = lanStatus;
	}
	/**
	 * @return
	 */
	public static String getIpTVStatus() {
		return DataSubSet.ipTVStatus;
	}
	/**
	 * @param ipTVStatus
	 */
	public static void setIpTVStatus(String ipTVStatus) {
		DataSubSet.ipTVStatus = ipTVStatus;
	}
	/**
	 * @param customerId
	 * @param date
	 * @param isRealTime
	 * @return
	 */
	public static List<DataItem> getDataSubSet(String customerId,Date date,boolean isRealTime,int aspectValue)
    {
		
	    DataSet dataset = null;
	    List<DataItem> dataList=new ArrayList<DataItem>();
        Random gen = new Random();
        String [] currentDataSubSet = null;
        switch(aspectValue)
        {
        case 0:
            currentDataSubSet = getLanDataSubSet();
            break;
        case 1:
            currentDataSubSet = getWanDataSubSet();
            break;
        case 2:
            currentDataSubSet = getIpTVDataSubSet();
            break;
        default:
            currentDataSubSet = getLanDataSubSet();
                
        }
	    //Check if the request is for real time data
//	    if(isRealTime) 
//		{
//	        dataset = new CbtOpsHelper().collectCustomerData(customerId, date);
//		}
//	    else
//	    {
//	        dataset = new CbtOpsHelper().getCustomerData(customerId,date);
//	    }
//	    
//		
//		{
//		    if(dataset[0]!=null) 
//		     setLanStatus((String)dataset[0].get("kpithv"));
//		    if(dataset[1]!=null) 
//	         setWanStatus((String)dataset[1].get("kpithv"));
//		    if(dataset[2]!=null) 
//	         setIpTVStatus((String)dataset[2].get("kpithv"));
//	         
//	        if(dataset[aspectValue]!=null)
//	        {
//	            for(int i=0;i<currentDataSubSet.length;i++)
//	             {
//	                 if(dataset[aspectValue].get(currentDataSubSet[i])!=null)
//	                   {
//	                       Integer r = gen.nextInt(3);
//	                       DataItem item =(DataItem) dataset[aspectValue].get(currentDataSubSet[i]);
//	                       item.setStatus(r.toString());
//	                       dataList.add(item);   
//	                   }
//	             }
//	        }
//		    
//	         
//		}
	    
		return null;
    	
	}
	/**
	 * @param wanDataSubSet
	 */
	public static void setWanDataSubSet(String[] wanDataSubSet)
	{
		DataSubSet.wanDataSubSet = wanDataSubSet;
	}
	
	/**
	 * @param lanDataSubSet
	 */
	public static void setLanDataSubSet(String[] lanDataSubSet)
	{
		DataSubSet.lanDataSubSet = lanDataSubSet;
	}
	
	/**
	 * @param ipTVDataSubSet
	 */
	public static void setIpTVDataSubSet(String[] ipTVDataSubSet)
	{
		DataSubSet.ipTVDataSubSet = ipTVDataSubSet;
	}
	/**
	 * @return
	 */
	public static String[] getLanDataSubSet() {
		return lanDataSubSet;
	}
	/**
	 * @return
	 */
	public static String[] getWanDataSubSet() {
		return wanDataSubSet;
	}
	/**
	 * @return
	 */
	public static String[] getIpTVDataSubSet() {
		return ipTVDataSubSet;
	}
		
  
}
