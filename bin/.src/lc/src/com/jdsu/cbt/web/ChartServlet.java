package com.jdsu.cbt.web;

import java.io.*;

import javax.servlet.http.*;
import javax.servlet.ServletException;
import javax.servlet.ServletContext;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.plot.PiePlot;
import org.jfree.chart.labels.PieToolTipGenerator;
import org.jfree.chart.labels.StandardPieToolTipGenerator;
import org.jfree.chart.ChartRenderingInfo;
import org.jfree.chart.entity.StandardEntityCollection;
import org.jfree.chart.imagemap.ImageMapUtilities;

public class ChartServlet extends HttpServlet {
	
	protected final Log logger = LogFactory.getLog(getClass());
	
	public void init(){
		logger.info("Initializing chartservlet...");
	}
	
	/**
     * This method gets the charts from the session and stores them as images for JSP access
     * @param request  HttpServletRequest type
     * @param respose  HttpServletResponse type
     * @return
     * @throws ServletException, IOException
     */
	public void service(HttpServletRequest request, HttpServletResponse response)
           throws ServletException, IOException {
      String type = request.getParameter("type");
      String chartName = request.getParameter("chart");
      ServletContext sc = getServletContext();
      HttpSession ss = request.getSession();
      int width=0,height=0;
      JFreeChart chart = null;
      
      //If request is coming from hierarchy level (Pie Charts)
      if(type!=null && type.equals("hierarchy")){
	      String level = request.getParameter("level");
		  		  
		  chart = (JFreeChart) sc.getAttribute(chartName+level);
		  PiePlot plot = (PiePlot) chart.getPlot();
		  PieToolTipGenerator generator = new StandardPieToolTipGenerator();
		  plot.setToolTipGenerator(generator);
		  width=150;
		  height=150;
		  //logger.info("Plot..."+plot.toString()+" "+plot.getToolTipGenerator().toString());
		  
	  }
	
      //If request is coming from trending reports (Stacked Bar Charts)
	  if(type!=null && type.equals("trendingdata")){
	   	 //Get chart from context
	     chart = (JFreeChart) ss.getAttribute(chartName);
	     width=500;
	     height=500;
	  }
	  
	  ChartRenderingInfo info = new ChartRenderingInfo(new StandardEntityCollection());
	  //Populate info here
	  //logger.info("Inside ChartServlet.."+chartName+" "+((JFreeChart) sc.getAttribute(chartName)).toString()+" "+info.getEntityCollection().toString());
      OutputStream outStream = response.getOutputStream();  
	  
	  response.setContentType("image/png");
	  response.setHeader("Cache-Control", "no-store, no-cache, must-revalidate, post-check=0, pre-check=0");
	  response.setHeader("Pragma", "no-cache");
	  ChartUtilities.writeChartAsPNG(outStream, chart, width, height, info);
	  PrintWriter pw = new PrintWriter(outStream);
	  ImageMapUtilities.writeImageMap(pw, "chart", info);
	  
	  outStream.flush();
	  outStream.close();
	  pw.close();
	}
 
	public void destroy(){
		
	}
}