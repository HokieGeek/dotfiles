/**
 * @author mur50399
 *
 */

package com.jdsu.cbt.collector.util;

import java.util.ArrayList;
import java.util.HashMap;

import com.jdsu.netopt.collector.CollectorParameters;

public class ProxyServerCollectorParameters {
    /** param id=List of (comma separated) EMS ID(s) */
    protected static String[] COLLECTION_EMS_LIST_PARAM = {"p_user_parm1",
                                "p_user_parm2", "p_user_parm3",
                                "p_user_parm11", "p_user_parm12",
                                "p_user_parm13" };

    /** param value signifies all EMS should be retrieved */
    public static String COLLECTION_EMS_LIST_PARAM_DEFAULT_VALUE = "ALL";

    private ArrayList ems_list = null;
    private boolean all_ems = false;

    /** ctor */
    public ProxyServerCollectorParameters()
    {}

    /** parsing collection params */
    public void parseCollectorParameters(CollectorParameters collection_params) throws Exception
    {
		HashMap map = new HashMap();
		
		String parmVal = null;
		String val = null;
        for (int i=0; i < COLLECTION_EMS_LIST_PARAM.length; i++)
        {
        	parmVal = collection_params.getParam(COLLECTION_EMS_LIST_PARAM[i]);
        	if ((parmVal == null) || (parmVal.equals("")))
        		continue;

   			String[] parts = parmVal.split(",");
   			
			for (int j=0; j < parts.length; j++)
			{
				val = parts[j].trim();
				if (COLLECTION_EMS_LIST_PARAM_DEFAULT_VALUE.equals(val))
				{
					//return the list of all EMS IDs
					ems_list = new ArrayList();
					ems_list.add(COLLECTION_EMS_LIST_PARAM_DEFAULT_VALUE);
					all_ems = true;
					break;
				}
				map.put(val, val);
			}
			if (all_ems)
			break;
        }
        
        if (!all_ems)
        	ems_list = new ArrayList(map.values());
    }

    /** return list of EMS IDs */
    public ArrayList getEMSIDs()
    {
       return ems_list;
    }

    /** flag indicates all EMS should be retrieved */
    public boolean isAllEMSSpecified()
    { return all_ems; }
}
