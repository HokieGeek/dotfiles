package com.jdsu.cbt.web;

import java.util.List;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jfree.chart.JFreeChart;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.multiaction.MultiActionController;

import com.jdsu.beans.DataSet;
import com.jdsu.beans.HierarchyData;
import com.jdsu.cbt.charts.ChartHierarchyData;
import com.jdsu.helpers.CbtOpsHelper;

/**
 * @author kal50389
 * This class acts as a controller for all Critical Report requests 
 * like Critical Customer View, Critical DSLAM view etc.
 * 
 *
 */
public class CriticalReportController extends MultiActionController
{
    /** Logger for this class and subclasses */
    protected final Log logger = LogFactory.getLog(getClass());
    
    public ModelAndView getCriticalCustomer(HttpServletRequest request,
    HttpServletResponse response) throws Exception
    {
        
        final String fName = "getCriticalCustomer()";
        logger.info(fName);
        String level = request.getParameter("level");
        String entityName = request.getParameter("entityName");
        ModelAndView mav = new ModelAndView("dslamView");
        mav.addObject("critical", "true");
        DataSet critCusts =  new CbtOpsHelper().peformOperation("CriticalCustomer", null);
        List<Map<String,Object>> custStats = (List<Map<String, Object>>)critCusts.get("CRITICALCUSTOMER");
        logger.info("customer list size.."+custStats.size());
//        List<JFreeChart> charts = ChartHierarchyData.createCharts(level,custStats);
//        // Save charts in application scope
//        ServletContext sc = getServletContext();
//        if(charts!=null && charts.size()>0)
//        {
//            sc.setAttribute("wanChart"+level, charts.get(0));
//            sc.setAttribute("lanChart"+level, charts.get(1));
//            sc.setAttribute("iptvChart"+level, charts.get(2));
//        }
        mav.addObject("level", level);
        mav.addObject("entityName", entityName);
        mav.addObject("CUSTSTATS", custStats);
        if (!custStats.isEmpty())
        {
            mav.addObject("hierarchy",crumbTrail(custStats.get(0)));
        }
        
        return mav;
    }
    
    /**
     * This method returns the critical dslams for all regions
     * @param request
     * @param response
     * @return
     * @throws Exception
     */
    public ModelAndView getCriticalDslam(HttpServletRequest request,
            HttpServletResponse response) throws Exception
    {
        
        final String fName = "getCriticalDslam()";
        logger.info(fName);
        ModelAndView mav = new ModelAndView("navigation");
        String level = request.getParameter("level");
        String entityName = request.getParameter("entityName");
        
        DataSet critDslams = new CbtOpsHelper().peformOperation("CriticalDSLAMS", null);
        List<Map<String,Object>> nwElements = (List<Map<String, Object>>)critDslams.get("CRITICALDSLAMS");
        nwElements = HierarchyData.getHierarchy(nwElements);
        mav.addObject("REGSTATS", nwElements);
        mav.addObject("listSize", nwElements.size());
        List<JFreeChart> charts = ChartHierarchyData.createCharts(level,nwElements);
        // Save charts in application scope
        ServletContext sc = getServletContext();
        if(charts!=null && charts.size()>0)
        {
            sc.setAttribute("wanChart"+level, charts.get(0));
            sc.setAttribute("lanChart"+level, charts.get(1));
            sc.setAttribute("iptvChart"+level, charts.get(2));
        }
       
        mav.addObject("level", level);
        mav.addObject("entityName", entityName);
        mav.addObject("critical", "true");
        
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
