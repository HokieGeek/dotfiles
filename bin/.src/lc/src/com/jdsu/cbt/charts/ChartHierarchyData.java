package com.jdsu.cbt.charts;

import java.awt.Color;
import java.awt.Font;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PiePlot;
import org.jfree.chart.title.TextTitle;
import org.jfree.data.general.DefaultPieDataset;
import org.jfree.data.general.PieDataset;


public class ChartHierarchyData {
	
	protected static final Log logger = LogFactory.getLog(ChartHierarchyData.class);
	
	/**
     * This method returns dataset created from the input List
     * @param level  The hierarchy level where chart is created
     * @param statTotals  List of totals that contain the chart data 
     * @return dataset  PieDataset type 
     */
    public static PieDataset createPieDataset(String level, List<Long> statTotals) {
		
		DefaultPieDataset dataset = new DefaultPieDataset();
		long fullTotal=0;
		for(int i=0;i<statTotals.size();i++){
			fullTotal += statTotals.get(i).longValue();
		}
		
		dataset.setValue("SectionPoor", (Long)statTotals.get(0));
		dataset.setValue("SectionFair", (Long)statTotals.get(1));
		dataset.setValue("SectionGood", (Long)statTotals.get(2));
		dataset.setValue("SectionUnknown", (Long)statTotals.get(3));
		
		return dataset;        
    }
    
    /**
     * This method contructs Pie chart from the input dataset
     * @param dataset  PieDataset type - created in createPieDataset(...) 
     * @return chart  JFreeChart type
     */
	public static JFreeChart createPieChart(PieDataset dataset) {
	
		JFreeChart chart = ChartFactory.createPieChart(
	        	"",  // chart title
	            dataset,             // data
	            false,                // include legend
	            false,
	            false
	    );
	        
		TextTitle title = chart.getTitle();
		title.setToolTipText("A title tooltip!");
		chart.setBorderVisible(false);
		
		PiePlot plot = (PiePlot) chart.getPlot();
		plot.setLabelFont(new Font("SansSerif", Font.PLAIN, 12));
		plot.setNoDataMessage("No data available");
		//plot.setSectionOutlinesVisible(false);
		plot.setShadowPaint(null);
		plot.setOutlineStroke(null);
		plot.setOutlinePaint(null);
		plot.setCircular(true);
		//plot.setLabelGap(0.02);
		//chart.setBackgroundPaint(Color.WHITE);
		plot.setLabelGenerator(null);
		plot.setOutlineVisible(false);
		
		plot.setSectionPaint("SectionPoor", new Color(255, 0, 0));
		plot.setSectionPaint("SectionFair", new Color(255, 255, 0));
		plot.setSectionPaint("SectionGood", new Color(0, 205, 0));
		plot.setSectionPaint("SectionUnknown", new Color(204, 204, 204));
		
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
        List<Long> wanTotal = new ArrayList<Long>();
        List<Long> lanTotal = new ArrayList<Long>();
        List<Long> iptvTotal = new ArrayList<Long>();
       if(entityStats.size()>0)
       {
           
      
        if (Integer.parseInt(level)<=4){
    		// Last element in Hierarchy List is Totals-Map
	        Map<String, Object> totalsMap = entityStats.get(entityStats.size()-1);
	        wanTotal.add(new Long(((BigDecimal)(totalsMap.get("wan_poor"))).longValue()));
	        wanTotal.add(new Long(((BigDecimal)(totalsMap.get("wan_fair"))).longValue()));
	        wanTotal.add(new Long(((BigDecimal)(totalsMap.get("wan_good"))).longValue()));
	        wanTotal.add(new Long(((BigDecimal)(totalsMap.get("wan_unknown"))).longValue()));

	        lanTotal.add(new Long(((BigDecimal)(totalsMap.get("lan_poor"))).longValue()));
	        lanTotal.add(new Long(((BigDecimal)(totalsMap.get("lan_fair"))).longValue()));
	        lanTotal.add(new Long(((BigDecimal)(totalsMap.get("lan_good"))).longValue()));
	        lanTotal.add(new Long(((BigDecimal)(totalsMap.get("lan_unknown"))).longValue()));
	        
	        iptvTotal.add(new Long(((BigDecimal)(totalsMap.get("iptv_poor"))).longValue()));
	        iptvTotal.add(new Long(((BigDecimal)(totalsMap.get("iptv_fair"))).longValue()));
	        iptvTotal.add(new Long(((BigDecimal)(totalsMap.get("iptv_good"))).longValue()));
	        iptvTotal.add(new Long(((BigDecimal)(totalsMap.get("iptv_unknown"))).longValue()));
        }
        
        if (Integer.parseInt(level)==5){
        	
        	//Initialize arraylists
        	for (int i=0;i<4;i++){
                wanTotal.add((long)0);
                lanTotal.add((long)0);
                iptvTotal.add((long)0);
        	}
    		for (int i=0;i<entityStats.size();i++){
    			logger.info("wan size..."+wanTotal.size()+entityStats.get(i).get("WAN").toString()+entityStats.get(i).get("LAN").toString()+entityStats.get(i).get("IPTV").toString());
    			if (entityStats.get(i).get("WAN").toString().equalsIgnoreCase("poor")){
    				wanTotal.set(0, wanTotal.get(0)+1);
    			}
    			else if (entityStats.get(i).get("WAN").toString().equalsIgnoreCase("fair")){
    				wanTotal.set(1, wanTotal.get(1)+1);
    			}
    			else if (entityStats.get(i).get("WAN").toString().equalsIgnoreCase("good")){
    				wanTotal.set(2, wanTotal.get(2)+1);
    			}
    			else if (entityStats.get(i).get("WAN").toString().equalsIgnoreCase("unknown")){
    				wanTotal.set(3, wanTotal.get(3)+1);
    			}
    			
    			if (entityStats.get(i).get("LAN").toString().equalsIgnoreCase("poor")){
    				lanTotal.set(0, lanTotal.get(0)+1);
    			}
    			else if (entityStats.get(i).get("LAN").toString().equalsIgnoreCase("fair")){
    				lanTotal.set(1, lanTotal.get(1)+1);
    			}
    			else if (entityStats.get(i).get("LAN").toString().equalsIgnoreCase("good")){
    				lanTotal.set(2, lanTotal.get(2)+1);
    			}
    			else if (entityStats.get(i).get("LAN").toString().equalsIgnoreCase("unknown")){
    				lanTotal.set(3, lanTotal.get(3)+1);
    			}
    			
    			if (entityStats.get(i).get("IPTV").toString().equalsIgnoreCase("poor")){
    				iptvTotal.set(0, iptvTotal.get(0)+1);
    			}
    			else if (entityStats.get(i).get("IPTV").toString().equalsIgnoreCase("fair")){
    				iptvTotal.set(1, iptvTotal.get(1)+1);
    			}
    			else if (entityStats.get(i).get("IPTV").toString().equalsIgnoreCase("good")){
    				iptvTotal.set(2, iptvTotal.get(2)+1);
    			}
    			else if (entityStats.get(i).get("IPTV").toString().equalsIgnoreCase("unknown")){
    				iptvTotal.set(3, iptvTotal.get(3)+1);
    			}
    		}
        }	
        
        // Generate Pie Charts from totals
        JFreeChart wanChart = createPieChart(createPieDataset(level,wanTotal));
		JFreeChart lanChart = createPieChart(createPieDataset(level,lanTotal));
		JFreeChart iptvChart = createPieChart(createPieDataset(level,iptvTotal));
        charts.add(wanChart);
        charts.add(lanChart);
        charts.add(iptvChart);
       }
        return charts;
    }
}