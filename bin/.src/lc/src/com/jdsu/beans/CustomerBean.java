package com.jdsu.beans;

import java.util.*;

public class CustomerBean {
	private String customerId;
	private String timeRange;
    private String[] statThv;
    
    public void setCustomerId(String custId){
    	customerId = custId;
    }
    
    public void setTimeRange(String tRange){
    	timeRange = tRange;
    }
    
    public void setStatThv(String[] sThv){
    	statThv = sThv;
    }
    
    public String getCustomerId(){
    	return customerId;
    }
    
    public String getTimeRange(){
    	return timeRange;
    }
    
    public String[] getStatThv(){
    	return statThv;
    }
}