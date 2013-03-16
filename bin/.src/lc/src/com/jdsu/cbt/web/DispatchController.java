package com.jdsu.cbt.web;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jfree.chart.JFreeChart;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.multiaction.MultiActionController;

import com.jdsu.beans.DataSet;
import com.jdsu.beans.HierarchyData;
import com.jdsu.cbt.charts.ChartHierarchyData;
import com.jdsu.helpers.CbtOpsHelper;

public class DispatchController extends MultiActionController
{
    /** Logger for this class and subclasses */
    protected final Log logger = LogFactory.getLog(getClass());

    // DB columns that are for application use, and that
    // don't need to be displayed.
    // FIXME: Add new ones here
    private final String[] appUseOnly = {"CUSTOMER",
            "REGION",
            "LATA",
            "STATE",
            "DISTRICT",
            "LOCATION",
            "DSLAM",
            "COMP_TYPE",
            "COLLECTION_DATE",
            "Timestamp"};
    
    private final String[] nonStatus = {"Customer ID",
            "Data Path",
            "COLLECTION_DATE",
            "COMPONENT_ID",
            "Timestamp"
            };
    
    private static Set<String> dontDisplay = null;
    private static Set<String> noUseStatus = null;
    
    public void init(){

    	logger.info("Initializing DispatchController...");	
    	ServletContext sc = getServletContext();
    	if(sc.getAttribute("hierarchy") ==null) { 
    	 	Map hierarchyMap = new TreeMap(); 
    	 	hierarchyMap.put("0", "All Regions"); 
    	 	hierarchyMap.put("1", "Region"); 
    	 	hierarchyMap.put("2", "District"); 
            hierarchyMap.put("3", "Area"); 
    	 	hierarchyMap.put("4", "Wire Center"); 
    	 	hierarchyMap.put("5", "DSLAM");
    	 	hierarchyMap.put("6", "Customer"); 
    	 	sc.setAttribute("hierarchy", hierarchyMap);
    	} 
    	dontDisplay = new HashSet<String>();
        for (int i =0;i<appUseOnly.length;i++)
        {
            dontDisplay.add(appUseOnly[i]);
        }

        noUseStatus = new HashSet<String>();
        for (int i =0;i<nonStatus.length;i++)
        {
            noUseStatus.add(nonStatus[i]);
        }

   }

    public ModelAndView customer(HttpServletRequest request,
            HttpServletResponse response) throws Exception
    {

        String aspect = request.getParameter("aspect");
        aspect = (aspect != null) ? aspect : "2";
        int aspectValue = new Integer(aspect).intValue();
        String customerId = request.getParameter("entityName");
        String timeRange = request.getParameter("timeRange");
        String date = request.getParameter("histDate");
        Date histDate = null;
        if (date != null && !("".equals(date)))
        {
            histDate = new SimpleDateFormat("MM/dd/yy").parse(date);
        }
           
        boolean isRealTime = timeRange.equals("real");
        
        ModelAndView mav = fetchCustomer(customerId,histDate,isRealTime,aspectValue, timeRange);
        return mav;
        
    }
    
    private ModelAndView fetchCustomer(String customerId, Date histDate, 
            boolean isRealTime, int aspectValue, String timeRange)
    {
        ModelAndView mav = new ModelAndView("customerView");

        DataSet dataSet = new CbtOpsHelper().getCustomerData(customerId,histDate);
        
        //---- Get all three stats for now---
        String lanStatus = "", wanStatus = "", iptvStatus = "";
        List<Map<String, Object>> mapLan = (List<Map<String, Object>>) dataSet.get(CbtOpsHelper.COMPONENT_TYPES[1]);
        List<Map<String, Object>> mapWan = (List<Map<String, Object>>) dataSet.get(CbtOpsHelper.COMPONENT_TYPES[2]);
        List<Map<String, Object>> mapIptv = (List<Map<String, Object>>) dataSet.get(CbtOpsHelper.COMPONENT_TYPES[3]);
        
        List<Map<String, Object>> mapList = (List<Map<String, Object>>) dataSet.get(CbtOpsHelper.COMPONENT_TYPES[aspectValue]);
        Random gen = new Random();
        Map<String, Object> curr = null;
        List<Map<String, String>> itemList = new ArrayList<Map<String, String>>();
        if (!mapList.isEmpty())
        {
            curr = (Map) mapList.get(0);
            Set<String> a = curr.keySet();
            Iterator<String> itr = a.iterator();
            Map<String, String> newMap = new TreeMap<String, String>();                       
            newMap.put("name", "Timestamp");
            DateFormat df = new SimpleDateFormat("MMM d, ''yy h:mm a");
            if (curr.containsKey("Timestamp"))
            {
                newMap.put("value", df.format(curr.get("Timestamp")));
            }
            else
            {
                newMap.put("value", df.format((Date)dataSet.get("DS TIME")));
            }
            newMap.put("status", "");
            itemList.add(newMap);

            while (itr.hasNext())
            {
                String key = (String) itr.next();
                if (dontDisplay.contains(key)) continue;
                newMap = new TreeMap<String, String>();                       
                newMap.put("name", key);
                Object val = curr.get(key);
                if (val!=null)
                {
                    newMap.put("value", val.toString());
                }
                else
                {
                    newMap.put("value", "");
                }
                if (noUseStatus.contains(key))
                {
                    newMap.put("status", "");
                }
                else
                {
                    if (key.equals("KPI Weight"))
                    {
                        // get same status as kpithv
                        val = curr.get("KPI Threshold Violation");
                    }
                    if (val==null)
                    {
                        newMap.put("status", "3");
                    }
                    else if (val.toString().equalsIgnoreCase("good"))
                    {
                        newMap.put("status", "0");
                    }
                    else if (val.toString().equalsIgnoreCase("fair"))
                    {
                        newMap.put("status", "2");
                    }
                    else if (val.toString().equalsIgnoreCase("poor"))
                    {
                        newMap.put("status", "1");
                    }
                    else
                    {
                        newMap.put("status", String.valueOf(gen.nextInt(3))); 
                    }
                }
                itemList.add(newMap);
                
            }
            
            //---Get kpithv values for three stats---
            if (mapLan!=null && mapWan!=null && mapIptv!=null && !mapLan.isEmpty() && !mapWan.isEmpty() && !mapIptv.isEmpty()) {
	            if(mapLan.get(0).containsKey("KPI Threshold Violation") && mapWan.get(0).containsKey("KPI Threshold Violation") && mapIptv.get(0).containsKey("KPI Threshold Violation"))
	            {	
	            	if (mapLan.get(0).get("KPI Threshold Violation")!=null)
	            		lanStatus = mapLan.get(0).get("KPI Threshold Violation").toString();
	            	else lanStatus = "unknown";
	            	if (mapWan.get(0).get("KPI Threshold Violation")!=null)
	            		wanStatus = mapWan.get(0).get("KPI Threshold Violation").toString();
	            	else wanStatus = "unknown";
	            	if (mapIptv.get(0).get("KPI Threshold Violation")!=null)
	            		iptvStatus = mapIptv.get(0).get("KPI Threshold Violation").toString();
	            	else iptvStatus = "unknown";
	            }
            }    
           
        }
        if(itemList.isEmpty())
        {
            mav.addObject("message.nodata",customerId);
        }
        
                     
        //get the Measurement timestamps to populate the History dropdown
        Date [] historyRange = new CbtOpsHelper().
                           getMsmtTimestamps(CbtOpsHelper.COMPONENT_TYPES[aspectValue], customerId);
        mav.addObject("historyRange", historyRange);
        logger.info("wanDataList..size" + itemList.size());
        Object[] args = getCriticalMesg(lanStatus, wanStatus, iptvStatus);
       
        // For crumb trail
        if (timeRange.toLowerCase().equals("latest"))
        {
            if (!mapList.isEmpty())
            {
                mav.addObject("hierarchy",crumbTrail(mapList.get(0)));
            }
        }
        
        mav.addObject("timeRange", timeRange);
        mav.addObject("entityName", customerId);
        mav.addObject("aspect", Integer.toString(aspectValue));
        mav.addObject("lanStatus", lanStatus);
        mav.addObject("wanStatus", wanStatus);
        mav.addObject("ipTVStatus", iptvStatus);
        mav.addObject("statArgs", args);
        mav.addObject("message.thvViolation", "criticalMessage.thvViolation"
                + args.length);
        mav.addObject("currentLevel", "4");
        mav.addObject("pkiData", itemList);
        mav.addObject("histDate", histDate);
        logger.info("returning customer model and view..");

        return mav;
    }

    public Object[] getCriticalMesg(String lan, String wan, String iptv)
    {
        List<String> argList = new ArrayList<String>();

        if (lan.equals("poor"))
        {
            argList.add("RG LAN");
        }
        if (wan.equals("poor"))
        {
            argList.add("RG WAN");
        }
        if (iptv.equals("poor"))
        {
            argList.add("IPTV");
        }

        return (Object[]) argList.toArray();
    }
    
    
    public ModelAndView hierarchy(HttpServletRequest request,
            HttpServletResponse response) throws Exception
    {
    	ModelAndView mav = null;
    	String level = request.getParameter("level");
        String entityName = request.getParameter("entityName");
        
        HttpSession ss = request.getSession();
        DataSet hierarchy = (DataSet) ss.getAttribute("hierarchy");
        
        int l = Integer.valueOf(level).intValue()-1;
        if (l>=0)
        {
            hierarchy.add(String.valueOf(l),entityName);
        }
        
        logger.debug("Level: "+level+"     EntityName: "+entityName);
        
        mav = fetchHierarchy(level, hierarchy);
        mav.addObject("entityName", entityName);
        return mav;
    }
        
        
    private ModelAndView fetchHierarchy(String level,DataSet hierarchy)
    {
        ModelAndView mav = new ModelAndView("navigation");
        List<Map<String,Object>> nwElements = null;
        
        DataSet regStats = new CbtOpsHelper().getComponent(level, hierarchy);
        List neList = (List) regStats.get("NE");
        nwElements = HierarchyData.getHierarchy(neList);

        mav.addObject("REGSTATS", nwElements);
        mav.addObject("listSize", nwElements.size());
        
        List<JFreeChart> charts = ChartHierarchyData.createCharts(level,nwElements);
        // Save charts in application scope
		ServletContext sc = getServletContext();
		sc.setAttribute("wanChart"+level, charts.get(0));
		sc.setAttribute("lanChart"+level, charts.get(1));
		sc.setAttribute("iptvChart"+level, charts.get(2));
        if (nwElements.size()>0)
        {
            mav.addObject("hierarchy",crumbTrail(nwElements.get(0)));
        }
                 		
        return mav;
    }
    

    public ModelAndView dslamCustomers(HttpServletRequest request,
            HttpServletResponse response) throws Exception
    {
        String entityName = request.getParameter("entityName");
        return fetchDslams(entityName);
        
    }
    
    
    private ModelAndView fetchDslams(String entityName)
    {
        ModelAndView mav = new ModelAndView("dslamView");

        List<Map<String,Object>> custStats = HierarchyData.getCustomers(entityName);

        mav.addObject("CUSTSTATS", custStats);
        mav.addObject("entityName",entityName);
        
        List<JFreeChart> charts = ChartHierarchyData.createCharts("5",custStats);
        // Save charts in application scope
 	if (charts != null && charts.size() >= 2) {
	   ServletContext sc = getServletContext();
	   sc.setAttribute("wanChart5", charts.get(0));
	   sc.setAttribute("lanChart5", charts.get(1));
	   sc.setAttribute("iptvChart5", charts.get(2));
        }

        if (custStats != null && !custStats.isEmpty())
        {
            mav.addObject("hierarchy",crumbTrail(custStats.get(0)));
        }
        
        return mav;
    }

    
    public ModelAndView searchController(HttpServletRequest request,
            HttpServletResponse response) throws Exception
    {
        String phoneNumber = request.getParameter("phoneNumber");
        String dslam = request.getParameter("dslam");
        String wireCenter = request.getParameter("wireCenter");
        String timeRange = request.getParameter("timeRange");
        ModelAndView mav = null;
        if (phoneNumber!=null && phoneNumber.length()>0)
        {
            phoneNumber = phoneNumber.trim();  
            mav = fetchCustomer(phoneNumber.toUpperCase(),null,false,2,timeRange);
            mav.addObject("aspect", "2");
            
        }
        else if (dslam!=null && dslam.length()>0)
        {
        	dslam = dslam.trim();
            mav = fetchDslams(dslam.toUpperCase());
            
        }
        else if (wireCenter!=null && wireCenter.length()>0)
        {
            wireCenter = wireCenter.trim().toUpperCase();
        	DataSet wc = new DataSet();
            wc.add("3", wireCenter);
            mav = fetchHierarchy("4",wc);
            mav.addObject("entityName",wireCenter);
            mav.addObject("level","4");
        }
        else
        {
            // dummy call customer with null
            mav = fetchCustomer("  ",null,false,2,timeRange);
            mav.addObject("aspect", "2");
            
        }
        if (mav!=null){
        	mav.addObject("search","true");
        }	
        return mav;
    }

    private DataSet crumbTrail(Map curr)
    {
        // For crumb trail 
        DataSet ds = new DataSet();
        if (curr.get("REGION") != null) ds.add("0", curr.get("REGION"));
        if (curr.get("STATE") != null) ds.add("1", curr.get("STATE"));
        if (curr.get("LATA") != null) ds.add("2", curr.get("LATA"));
        if (curr.get("LOCATION") != null) ds.add("3", curr.get("LOCATION"));
        if (curr.get("DSLAM") != null)
            ds.add("4", curr.get("DSLAM"));
        return ds;
    }
    
}
