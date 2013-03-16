package com.jdsu.cbt.web;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.TreeMap;
import java.util.Random;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jfree.chart.JFreeChart;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.multiaction.MultiActionController;

import com.jdsu.cbt.charts.ChartTrendingData;

public class TrendingReportController extends MultiActionController 
{
	protected final Log logger = LogFactory.getLog(getClass());
	
	public ModelAndView trendingData(HttpServletRequest request, HttpServletResponse response)
		                throws Exception {
		
		ModelAndView mav = new ModelAndView("oneYearTrending");
		
		//Call interface and get List<Map<String,Object> here
		List<Map<String,Object>> tstats = new ArrayList<Map<String,Object>>();
		for(int i=0;i<12;i++){
			Map<String,Object> mymap = new TreeMap<String,Object>();
			mymap.put("poor", Integer.toString(new Random().nextInt(50)));
			mymap.put("fair", Integer.toString(new Random().nextInt(50)));
			mymap.put("good", Integer.toString(new Random().nextInt(50)));
			mymap.put("unknown", Integer.toString(new Random().nextInt(50)));
			
			tstats.add(mymap);
		}
		
		List<JFreeChart> charts = ChartTrendingData.createCharts(null,tstats);
	    HttpSession ss = request.getSession();
	    ss.setAttribute("trending", charts.get(0));
	
 		return mav; 
	}
	
}

