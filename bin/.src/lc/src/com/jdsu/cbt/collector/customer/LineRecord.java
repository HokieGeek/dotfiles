/**
 * @author mur50399
 *
 */

package com.jdsu.cbt.collector.customer;

import com.jdsu.netopt.util.Record;

public class LineRecord extends Record{
	private static final long serialVersionUID = 1L;
	public static final String RECORD_TYPE = "J/Data Collection";
	public static final String CUSTOMER_ID = "customer_id";
    public static final String EMS_ID = "ems_id";
    public static final String CLASS = "class";
    public static final String PRIORITY = "priority";
    public static final String NE_VENDOR = "ne_vendor";
    public static final String NE_MODEL = "ne_model";
    public static final String NE_VERSION = "ne_version";
    public static final String NE_ID = "ne_id";
    public static final String NE_PORT = "ne_port_id";
    public static final String NE_VENDOR2 = "ne_vendor2";
    public static final String NE_MODEL2 = "ne_model2";
    public static final String NE_VERSION2 = "ne_version2";
    public static final String NE_ID2 = "ne_id2";
    public static final String NE_PORT2 = "ne_port_id2";

    public static final String[] headerStrings = {
        RECORD_TYPE, CUSTOMER_ID, EMS_ID, CLASS, PRIORITY, 
        NE_VENDOR, NE_MODEL, NE_VERSION, NE_ID, NE_PORT, 
        NE_VENDOR2, NE_MODEL2, NE_VERSION2, NE_ID2, NE_PORT2
    };

}