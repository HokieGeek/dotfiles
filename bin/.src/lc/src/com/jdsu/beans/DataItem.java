package com.jdsu.beans;

import java.sql.Timestamp;

/**
 * DataItem Java Bean
 * <br>
 * This bean is used to keep all related information about a database column, 
 * such as: id, value, short description, etc.
 * 
 *
 */
public class DataItem
{

    /**
     * Default constructor
     */
    public DataItem()
    {
    }

    /**
     * Id or Key that is used to identify this database column
     */
    private String key = null;
    
    /**
     * Value of this column. 
     * <br>
     *  A String representation is used for now (prototype), but will require to 
     *  support other data types in the future such as long, double, date, etc.
     */
    private String value = null;
    
    /**
     * Short name or description of this column.
     * <br>
     * This piece of information is not always directly available, but obtained
     * after mapping generic fields. It is useful to keep it with the key and 
     * value for display purposes. 
     */
    private String name = null;
    private String status = null;

    /**
     *  More useful constructor, sets all three fields at the time of creation.
     * @param status TODO
     */
    public DataItem(String key, String value, String name, String status)
    {
        this.key = key;
        this.value = value;
        this.name = name;
        this.status = status;
    }

    public String getKey()
    {
        return key;
    }

    public void setKey(String key)
    {
        this.key = key;
    }

    public String getValue()
    {
        return value;
    }

    public void setValue(String value)
    {
        this.value = value;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    
    
    /**
     *  Return a integer value of a key-word
     *  
     * @param key  Key or id of a DataItem
     * @return the value as an integer 
     */
    public int getIntValue(String key)
    {
        return new Integer(this.value).intValue();
    }
    
    /**
     * Return a Timestamp value of a key-word
     * 
     * @param key  Key or id of a DataItem
     */

    public Timestamp getTimestamp(String key)
    {
        // Prefer Timestamp over date since it keeps the nanos 
        Timestamp ts = Timestamp.valueOf(value);
        return ts;

    }

    /**
     * Returns value represented as type long.
     * 
     * 
     * @param Key
     * @return value as type long
     */
    public long getLongValue(String Key)
    {
        return new Long(this.value).longValue();
    }

    public String getStatus()
    {
        return status;
    }

    public void setStatus(String status)
    {
        this.status = status;
    }
    
}
