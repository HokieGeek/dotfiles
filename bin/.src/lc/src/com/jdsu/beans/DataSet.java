package com.jdsu.beans;

import java.security.InvalidParameterException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.collections.map.ListOrderedMap;

/**
 * DataSet Java Bean
 */
public class DataSet
{

    /**
     * Default constructor
     */
    public DataSet()
    {
        dataSet = new Hashtable<String, Object>();
    }

    /*
     * Container to hold data from any table, particularly: NE_COMP,
     * MEASUREMENT, MEASUREMENT_SUMMARY
     */
    public Hashtable<String, Object> dataSet;

    /**
     * Get the DataItem object that this key is pointing to
     * 
     * @param key
     * @return Object
     */

    public Object get(String key)
    {
        if (dataSet.containsKey(key))
        {
            Object value = dataSet.get(key);
            return cloneValue(value);
        }
        return null;
    }


    private Object cloneValue(Object orig)
    {
        /*
         * supports List, String, Date
         */
        
        // FIXME: Each time a new type is supported
        if (orig instanceof List)
        {
            List<Map<String,Object>> copy = new ArrayList<Map<String, Object>>();
            copy.addAll((Collection<? extends Map<String, Object>>) orig);
            return copy;
        }
        else if (orig instanceof String)
        {
            return new String((String) orig);
            
            
        }
        else if (orig instanceof Date)
        {
            return new Date(((Date)orig).getTime());
        }
        return orig;
        
    }

    /**
     * Get the DataItem object that this key is pointing to
     * 
     * @param key
     * @return Object
     */

    public Object getStringElements(String key)
    {
        if (dataSet.containsKey(key))
        {
            Object value = dataSet.get(key);
            return cloneValueToString(value);
        }
        return null;
    }



    private Object cloneValueToString(Object orig)
    {
        /*
         * supports List, String, Date
         */
        
        // FIXME: Each time a new type is supported
        if (orig instanceof List)
        {
            List thisList = (List<Map<String,String>>)orig;
            List<Map<String,String>> copy = new ArrayList<Map<String, String>>();
            for (int i=0;i<thisList.size();i++)
            {
                Map<String, String> copyMap = new ListOrderedMap();
                Map<String, Object> origMap = (Map<String, Object>)thisList.get(i);
                Iterator<String> itr = origMap.keySet().iterator();
                while(itr.hasNext())
                {
                    String key = (String) itr.next();
                    copyMap.put(key,origMap.get(key).toString());
                }
                copy.add(i, copyMap);
            }
            return copy;
        }
        else if (orig instanceof String)
        {
            return new String((String) orig);
            
            
        }
        else if (orig instanceof Date)
        {
            return ((Date)orig).toString();
        }
        return orig.toString();
        
    }

    
    
    /**
     * Insert a single key-word value pair in a DataSet
     * 
     * @param key  Key or id of a DataItem
     * @param item   Object
     */

    public void add(String key, Object item)
    {
        // Neither Key nor Value can be null!
        if (key == null || item == null)
        {
            throw new InvalidParameterException("Cannot insert null value into DataSet");
        }

        dataSet.put(key, item);
        return;
    }

    
    /**
     * Returns a list of keys this DataSet has
     *  
     * @return Enumeration of keys
     */
    
    public Enumeration<String> getKeys()
    {
        return dataSet.keys();
    }
    

    /**
     * Return the name of the DataSet
     * 
     * @return name of the DataSet as a String.
     */
    public String getDataSetName()
    {

        // FIXME: DB NAME - a random key word!
        if (dataSet.containsKey("DS NAME"))
        {
            return (String)dataSet.get("DS NAME");
        }
        return null;
    }
    
    /**
     * 
     * Get the time this DataSet was created.
     * 
     * @return Timestamp - This DataSet's timestamp
     */

    public Timestamp getDataSetTimestamp()
    {

        // FIXME: DB TIME - a random key word!
        if (dataSet.containsKey("DS TIME"))
        {
            
            return Timestamp.valueOf((String)dataSet.get("DS TIME"));
        }
        return null;
    }

    /**
     * Return all data as an array of @DataItem
     *  
     *  @return DataItem[] - an array of DataItem
     */

/*    public DataItem[] getDataSet()
    {
        DataItem[] aCopy = new DataItem[dataSet.size()];
        Enumeration<String> allKeys = dataSet.keys();
        int index = 0;
        while (allKeys.hasMoreElements())
        {
            String aKey = (String) allKeys.nextElement();
            DataItem dItem = dataSet.get(aKey);
            aCopy[index] = new DataItem(aKey, dItem.getValue(), dItem.getName(), null);
            index++;
        }

        return aCopy;
    }
*/
    /**
     * Set a DataSet from an array of DataItems
     * 
     * @param dItems - An array of DataItem
     */
/*    public void setDataSet(DataItem dItems[])
    {
        dataSet.clear();

        for (int index = 0; index < dItems.length; index++)
        {
            String aKey = dItems[index].getKey();

            DataItem newItem = new DataItem(aKey, dItems[index].getValue(),
                    dItems[index].getName(), null);

            dataSet.put(aKey, newItem);
            index++;
        }

    }
*/


}
