package com.jdsu.interfaces;

import java.util.Date;

import com.jdsu.beans.DataSet;

/**
 * Methods to obtain NetOptimize Component data
 */
public interface Component
{
    
    /**
     * Returns null if no records were found for this level.
     * 
     * 
     * @param level - One of the valid levels e.g REGION, STATE, etc.
     * @param lookupStr - A value that will be used as a filter
     * @return DataSet[], if found or null.
     */
    
    public DataSet getNe(String level, DataSet hierarchy);
    
    
    /**
     * Returns null if the component is not found, otherwise return the data
     * set.
     * 
     * @param compType
     *            Component Type (required)
     * @param name
     *            Component name (required)
     */
    public DataSet getNeComp(DataSet params);

    /**
     * Returns null if the component is not found, otherwise return the
     * specified data set. If the specified date is null, return the latest
     * measurement record.<br>
     * If the date is specified, return the row with the closet date.
     * 
     * @param compType
     *            Component Type (required)
     * @param name
     *            Component name (required)
     * @param date
     *            Date/Timestamp (not required)
     */
    public DataSet getMsmt(DataSet params);

    /**
     * Returns null if the component is not found, otherwise return the
     * specified data set. If the specified date is null, return the latest
     * measurement summary record.<br>
     * If the date is specified, return the row with the closet date.
     * 
     * @param compType
     *            Component Type (required)
     * @param name
     *            Component name (required)
     * @param date
     *            Date/Timestamp (not required)
     */
    public DataSet getMsmtSumm(String compType, String name, Date date);

    /**
     * Returns the measurement timestamps for the specified component.
     * 
     * @param compType
     *            Component Type (required)
     * @param name
     *            Component name (required)
     */
    public Date[] getMsmtTimestamps(String compType, String name);

    /**
     * 
     * Returns the measurement summary timestamps for the specified component.
     * 
     * @param compType
     *            Component Type (required)
     * @param name
     *            Component name (required)
     */
    public Date[] getMsmtSummTimestamps(String compType, String name);
}
