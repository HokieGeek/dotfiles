package com.jdsu.beans;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.jdsu.helpers.CbtOpsHelper;

public class HierarchyData
{
    protected static final Log logger = LogFactory.getLog(HierarchyData.class);

    public static List<Map<String, Object>> getHierarchy(List neList)
    {
        // Initialize row totals
        long[] wanTotals = new long[4];
        long[] lanTotals = new long[4];
        long[] iptvTotals = new long[4];
        
        
        
//        for (int i =0; i< neList.size(); i++)
//	   	 {
//	   	     Map curr = (Map) neList.get(i);
//	   	     Map<String, String> tmap = new TreeMap<String, String>();
//	   	     Set a = curr.keySet();
//	   	     Iterator itr = a.iterator();
//	   	     int j=0;
//	   	     while (itr.hasNext())
//	   	     {
//	   	    	 String key = (String) itr.next();
//	   	         Object val = curr.get(key);
//	   	         if (val!= null)
//	   	         {
//	   	             tmap.put(key.toLowerCase(), curr.get(key).toString());
//	   	         }
//	   	     }
//	   	  
//	   	     neList.set(i, tmap);
//	   	     
//	   	 }

        long wan_rowTotal = 0, lan_rowTotal = 0, iptv_rowTotal = 0;
       
        for (int i =0; i< neList.size(); i++) {
        	Map<String, Object> mymap = (Map) neList.get(i);
        	
        	wanTotals[0] +=((BigDecimal)(mymap.get("wan_poor"))).longValue();
        	wanTotals[1] += ((BigDecimal)(mymap.get("wan_fair"))).longValue();
        	wanTotals[2] += ((BigDecimal)(mymap.get("wan_good"))).longValue();
        	wanTotals[3] += ((BigDecimal)(mymap.get("wan_unknown"))).longValue();
        	
        	lanTotals[0] += ((BigDecimal)(mymap.get("lan_poor"))).longValue();
        	lanTotals[1] += ((BigDecimal)(mymap.get("lan_fair"))).longValue();
        	lanTotals[2] += ((BigDecimal)(mymap.get("lan_good"))).longValue();
        	lanTotals[3] += ((BigDecimal)(mymap.get("lan_unknown"))).longValue();
        	
        	iptvTotals[0] += ((BigDecimal)(mymap.get("iptv_poor"))).longValue();
        	iptvTotals[1] += ((BigDecimal)(mymap.get("iptv_fair"))).longValue();
        	iptvTotals[2] += ((BigDecimal)(mymap.get("iptv_good"))).longValue();
        	iptvTotals[3] += ((BigDecimal)(mymap.get("iptv_unknown"))).longValue();
                        
 	        wan_rowTotal =((BigDecimal)mymap.get("wan_poor")).longValue() + ((BigDecimal)mymap.get("wan_fair")).longValue()
	                  + ((BigDecimal)mymap.get("wan_good")).longValue() + ((BigDecimal)mymap.get("wan_unknown")).longValue();
            lan_rowTotal = ((BigDecimal)(mymap.get("lan_poor"))).longValue() + ((BigDecimal)(mymap.get("lan_fair"))).longValue()
                      + ((BigDecimal)(mymap.get("lan_good"))).longValue() + ((BigDecimal)(mymap.get("lan_unknown"))).longValue();
            iptv_rowTotal = ((BigDecimal)(mymap.get("iptv_poor"))).longValue() + ((BigDecimal)(mymap.get("iptv_fair"))).longValue()
                       + ((BigDecimal)(mymap.get("iptv_good"))).longValue() +((BigDecimal)(mymap.get("iptv_unknown"))).longValue();

            // Add row totals to Map
            mymap.put("wan_total",new BigDecimal( wan_rowTotal));
            mymap.put("lan_total",new BigDecimal( lan_rowTotal));
            mymap.put("iptv_total",new BigDecimal(iptv_rowTotal));

            neList.set(i, mymap);

        
    }

        // Create map element for column totals - the last row in display table
        Map<String, Object> mymap = new TreeMap<String, Object>();
        mymap.put("id", "total");
        mymap.put("wan_poor", new BigDecimal(wanTotals[0]));
        mymap.put("wan_fair",new BigDecimal(wanTotals[1]));
        mymap.put("wan_good",new BigDecimal(wanTotals[2]));
        mymap.put("wan_unknown", new BigDecimal(wanTotals[3]));
        mymap.put("wan_total", new BigDecimal(wanTotals[0] + wanTotals[1] + wanTotals[2] + wanTotals[3]));
        mymap.put("lan_poor", new BigDecimal(lanTotals[0]));
        mymap.put("lan_fair",new BigDecimal(lanTotals[1]));
        mymap.put("lan_good",new BigDecimal(lanTotals[2]));
        mymap.put("lan_unknown",new BigDecimal(lanTotals[3]));
        mymap.put("lan_total", new BigDecimal(lanTotals[0] + lanTotals[1] + lanTotals[2] + lanTotals[3]));
        mymap.put("iptv_poor",new BigDecimal(iptvTotals[0]));
        mymap.put("iptv_fair", new BigDecimal(iptvTotals[1]));
        mymap.put("iptv_good",new BigDecimal(iptvTotals[2]));
        mymap.put("iptv_unknown",new BigDecimal(iptvTotals[3]));
        mymap.put("iptv_total", new BigDecimal(iptvTotals[0] + iptvTotals[1] + iptvTotals[2] + iptvTotals[3]));
        
        neList.add(mymap);
        
        return neList;

    }

    public static List<Map<String, Object>> getCustomers(String dslam)
    {
        DataSet custStats = new CbtOpsHelper().getDlamCustStats(dslam);

        List dslamCusts = (List) custStats.get("DSLAMCUST");
        logger.info("list size in hierarchy data..."+dslamCusts.size()); 
        for (int i = 0; i < dslamCusts.size(); i++)
        {
            Map<String, Object> curCust = (Map) dslamCusts.get(i);
                        
            for (int j = 0; j < CbtOpsHelper.COMPONENT_TYPES.length; j++)
            {
            	switch (j)
                {
                case 0:
                    curCust.put("CUSTOMER", curCust.get(
                            CbtOpsHelper.COMPONENT_TYPES[j]));
                    break;
                case 1:
                    curCust.put("LAN", curCust.get(
                            CbtOpsHelper.COMPONENT_TYPES[j]));
                    break;
                case 2:
                    curCust.put("WAN", curCust.get(
                            CbtOpsHelper.COMPONENT_TYPES[j]));
                    break;
                case 3:
                    curCust.put("IPTV", curCust.get(
                            CbtOpsHelper.COMPONENT_TYPES[j]));
                    break;
                }
            }
            
            dslamCusts.set(i,curCust);
        }
        return dslamCusts;
    }

}
