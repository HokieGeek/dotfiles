package com.jdsu.cbt.charts;

import java.awt.Color;
import java.awt.Font;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.title.TextTitle;
import org.jfree.data.category.CategoryDataset;
import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.renderer.category.CategoryItemRenderer;

public class ChartTrendingData {
	
	protected static final Log logger = LogFactory.getLog(ChartTrendingData.class);
	
	/**
     * This method returns dataset created from the input List
     * @param level  The hierarchy level where chart is created
     * @param statTotals  List of maps that contain the chart data 
     * @return dataset  DefaultCategoryDataset type 
     */
    public static DefaultCategoryDataset createDataset(String level, List<Map<String,Object>> statTotals) {
		
    	String[] sections = {"SectionPoor","SectionFair","SectionGood","SectionUnknown"};
    	String[] months = {"Jan","Feb","Mar","Apr","May",
    			          "Jun","Jul","Aug","Sep","Oct","Nov","Dec"};

	   	DefaultCategoryDataset dataset = new DefaultCategoryDataset();
		
    	for (int i=0;i<statTotals.size();i++){ //this will be 12
    	  Map<String,Object> mymap = (Map<String,Object>) statTotals.get(i); 
    	  Set<String> curr = mymap.keySet();
    	  Iterator<String> itr = curr.iterator();
    	  int j=0;
    	  while(itr.hasNext()){
			dataset.addValue(Integer.parseInt((String)mymap.get(itr.next())),sections[j], months[i]);
			j++;
		  }
    	}
		return dataset;        
    }
	  
    /**
     * This method contructs Bar chart from the input dataset
     * @param dataset  DefaultCategoryDataset type - created in createDataset(...) 
     * @return chart  JFreeChart type
     */
	public static JFreeChart createBarChart(DefaultCategoryDataset dataset) {
	
		JFreeChart chart = ChartFactory.createStackedBarChart(
	        	"",  // chart title
	        	"", //domain axis label
	        	"", //range axis label
	            dataset, // data
	            PlotOrientation.VERTICAL,
	            false,   // include legend
	            false,
	            false
	    );
	        
		chart.setBorderVisible(false);
		
		CategoryPlot plot = (CategoryPlot) chart.getPlot();
		plot.setNoDataMessage("No data available");
		//plot.setSectionOutlinesVisible(false);
		
		plot.setOutlineStroke(null);
		plot.setOutlinePaint(null);
		
		chart.setBackgroundPaint(Color.WHITE);
		plot.setOutlineVisible(false);
		
		CategoryItemRenderer renderer = (CategoryItemRenderer)plot.getRenderer();
		renderer.setSeriesPaint(0, new Color(255, 0, 0));
		renderer.setSeriesPaint(1, new Color(255, 255, 0));
		renderer.setSeriesPaint(2, new Color(0, 205, 0));
		renderer.setSeriesPaint(3, new Color(204, 204, 204));
		
		return chart;
	        
	}
	
	/**
     * This method creates charts from the supplied List of data items
     * @param level  The hierarchy level where chart is created
     * @param entityStats  List of maps that contain the chart data 
     * @return charts  List of created charts
     */
	public static List<JFreeChart> createCharts(String level, List<Map<String,Object>> entityStats)
    {
    	
	    logger.info("list size for chart..."+entityStats.size());
	    List<JFreeChart> charts = new ArrayList<JFreeChart>();
      
        // Generate StackedBarChart
        JFreeChart trendingChart = createBarChart(createDataset(level,entityStats));
        charts.add(trendingChart);
 
        return charts;
    }
}