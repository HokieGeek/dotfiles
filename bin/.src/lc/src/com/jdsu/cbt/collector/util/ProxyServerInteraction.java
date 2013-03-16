/**
 * @author mur50399
 *
 */

package com.jdsu.cbt.collector.util;

import java.io.File;
import java.io.IOException;
//import java.util.ArrayList;

import com.acterna.netopt.core.AdaProps;

public class ProxyServerInteraction {
    private String DEFAULT_EMS_ROOT_DIRECTORY_NAME = "ems";
    private String DEFAULT_PROXY_SERVER_ROOT_DIRECTORY_NAME = "sp";
    private String REQUEST_DIRECTORY_NAME = "request";
    private String RESPONSE_DIRECTORY_NAME = "response";

    /**
     * datalb directory for CBT
     * default: /ada/tdce/datalb/site/cbt
     */
    protected String datalb_dir = AdaProps.TDCESITEDIR + File.separator + "cbt_ops";
    protected String ems_root_dir = null;
    protected String proxyserver_root_dir = null;

    public ProxyServerInteraction()
    {
       setRootDirectory(datalb_dir);
    }

    public ProxyServerInteraction(String datalb_dir)
    {
       setRootDirectory(datalb_dir);
    }

    public void setRootDirectory(String datalb_dir)
    { 
      this.datalb_dir = datalb_dir; 
      ems_root_dir = this.datalb_dir + File.separator + DEFAULT_EMS_ROOT_DIRECTORY_NAME;
      proxyserver_root_dir = this.datalb_dir + File.separator + DEFAULT_PROXY_SERVER_ROOT_DIRECTORY_NAME;
    } 

    public String getRootDirectory()
    { return datalb_dir; }

    /**
     * return top level directory for EMS based on id
     * @param ems_id EMS id
     */
    private String getTopLevelDirectoryForEMS(String ems_id) throws IOException
    { 
      String dir = ems_root_dir + File.separator + ems_id;
      return validatePath(dir);
    }

    /**
     * return input directory for EMS based on id
     * @param ems_id EMS id
     */
    public String getDataInputDirectoryForEMS(String ems_id) throws IOException
    {
      String dir = getTopLevelDirectoryForEMS(ems_id) + File.separator + REQUEST_DIRECTORY_NAME;
      return validatePath(dir);
    }

    /**
     * return output directory for EMS based on id
     * @param ems_id EMS id
     */
    public String getDataOutputDirectoryForEMS(String ems_id) throws IOException
    {
      String dir = getTopLevelDirectoryForEMS(ems_id) + File.separator + RESPONSE_DIRECTORY_NAME;
      return validatePath(dir);
    }

    /**
     * return top level directory for proxy server based on id
     * @param sp_id proxy server id
     */
    private String getTopLevelDirectoryForSuperProxy(String sp_id) throws IOException
    {
      String dir = proxyserver_root_dir + File.separator + sp_id;
      return validatePath(dir);
    }

    /**
     * return input directory for proxy server based on id
     * @param sp_id proxy server id
     */
    public String getDataInputDirectoryForSuperProxy(String sp_id) throws IOException
    {
      String dir = getTopLevelDirectoryForSuperProxy(sp_id) + File.separator + REQUEST_DIRECTORY_NAME;
      return validatePath(dir);
    }

    /** 
     * return output directory for proxy server based on id
     * @param sp_id proxy server id
     */
    public String getDataOutputDirectoryForSuperProxy(String sp_id) throws IOException
    {
      String dir = getTopLevelDirectoryForSuperProxy(sp_id) + File.separator + RESPONSE_DIRECTORY_NAME;
      return validatePath(dir);
    }

    /** return list of EMS IDs */
/*    public ArrayList getAllEMSIDs() throws IOException
    {
        validatePath(ems_root_dir);
        File f = new File(ems_root_dir);

        ArrayList list = new ArrayList();
        File[] subdirs = f.listFiles();
        for (int i=0; i < subdirs.length; i++)
        {
           //expect a subdir for each EMS ID
           if (subdirs[i].isDirectory())
              list.add(subdirs[i].getName());
        }
        return list;
    }
*/
    /** validate path */
    public static String validatePath(String path) throws IOException
    {
        File f = new File(path);
        if (!f.exists())
           throw new IOException("Directory dir=" + f.getAbsolutePath() + " does not exist");

        if (!f.isDirectory())
           throw new IOException("Directory dir=" + f.getAbsolutePath() + " is a non-directory path");

        return f.getAbsolutePath();
    }
}
