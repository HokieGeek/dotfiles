package com.jdsu.helpers;

import java.util.Date;
import java.util.List;
import java.util.Map;
import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;

import com.jdsu.beans.DataSet;

public class JdbcHelper
{
	
	/** Logger for this class and subclasses */
    protected static final Log logger = LogFactory.getLog(JdbcHelper.class);
	private static JdbcTemplate jdbcTemplate;

    public void setDataSource(DataSource dataSource) {
        JdbcHelper.jdbcTemplate = new JdbcTemplate(dataSource);
    }


    /**
     * Executes an sql query using the private jdbcConnection and returns the
     * results in an ArrayList<DataSet>
     * 
     * @param querySql
     *            An Sql query String (required)
     *            
     * @return an Array of DataSet
     * 
     */
    public static DataSet runAQuery(String querySql)
    {
        return runAQuery(querySql, null);
    }
    
    
    /**
     * 
     * @param querySql
     * @param id
     * @return
     */
    public static DataSet runAQuery(String querySql, String id)
    {
        
    	 logger.info(querySql);
         DataSet queryDataSet = new DataSet();
    	 if (jdbcTemplate == null)
    	 {
             DriverManagerDataSource dataSource = new DriverManagerDataSource();
             dataSource.setDriverClassName("oracle.jdbc.driver.OracleDriver");
             dataSource.setUrl("jdbc:oracle:thin:@zinc:1521:tdce");
             dataSource.setUsername("tdce");
             dataSource.setPassword("linesize");
             jdbcTemplate = new JdbcTemplate(dataSource);
    	 }
    	 
    	 System.out.println(querySql);
    	 List<Map<String,Object>> rowList = (List<Map<String,Object>>) jdbcTemplate.queryForList(querySql);
    	 System.out.println("Finished select");
    	 if (id!=null && id.length()>0)
    	 {
             queryDataSet.add(id,rowList);
    	 }
    	 else
         {
    	     queryDataSet.add("LAST_QRY",rowList);
         }
    	 queryDataSet.add("DS TIME", new Date());

         return queryDataSet; 

    }
}
