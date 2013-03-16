/**
 * @author mur50399
 *
 */

package com.jdsu.cbt.collector.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import com.jdsu.cbt.collector.util.ProxyServerInteraction;
import com.jdsu.netopt.util.RecordHeader;
import com.jdsu.netopt.util.RecordWriter;

public class ProxyServerRequest {
    /** PassiveFile timestamp format string */
    private static String PASSIVEFILE_DATE_FORMAT = "yyyyMMddHHmm";

    /** PassiveFile timestamp format object */
    private static SimpleDateFormat PASSIVEFILE_DATE = new SimpleDateFormat(PASSIVEFILE_DATE_FORMAT);

    private static String REQUEST_HEADER = "NetComplete CBT Reference Data File version 0001";

    private static String REQUEST_FOOTER = "_END_OF_STRUCTURED_DATA";

    public static String REQUEST_POSTFIX = ".csv";

    private static String REQUEST_WORKING_DIRECTORY = "work";
    private String file_path = null;
    private String file_name = null;

    private boolean isOpened = false;
    private File work = null;
    private RecordWriter writer = null;
    private String time_stamp = null;

    public ProxyServerRequest(String filePath, String fileName, String timeStamp) throws IOException
    {
       file_path = filePath;
       ProxyServerInteraction.validatePath(file_path);
       file_name = fileName;
       time_stamp = timeStamp;
    }

    protected String getWorkingDirectory() throws IOException
    {
       String dir = file_path + File.separator + REQUEST_WORKING_DIRECTORY;
       File f = new File(dir);
       if ((!f.exists()) && (!f.mkdirs()))
	     throw new IOException("Failing to create working dir=" +f.getAbsolutePath());
       return f.getAbsolutePath();
    }

    /** open request for writting */
    public void open() throws IOException, Exception
    {
       if (isOpened) 
	  return;

       //create working file
       String fileStr = getWorkingDirectory() + File.separator + file_name;
       work = new File(fileStr);

       //initialize RecordWriter
       OutputStreamWriter ow = new OutputStreamWriter(new FileOutputStream(work));
       writer = new RecordWriter(ow);

       //write header
       writeRequestHeader();

       isOpened = true;
    }

    private void writeRequestHeader() throws IOException
    {
       StringBuffer buf = new StringBuffer(REQUEST_HEADER);
       buf.append("\nTIMESTAMP: ");
       buf.append(time_stamp);
       buf.append(" EST");
       String[] str = new String[1];
       str[0] = buf.toString();

       RecordHeader rh = new RecordHeader();
       rh.addAll(str);
       writer.writeRecordHeader(rh);
    }

    private void writeRequestFooter() throws IOException
    {
       String[] str = new String[1];
       str[0] = REQUEST_FOOTER;

       RecordHeader rh = new RecordHeader();
       rh.addAll(str);
       writer.writeRecordHeader(rh);
    }

    public RecordWriter getRecordWriter()
    { return writer; }

    /** close request */
    public void close() throws IOException, Exception
    {
       if (!isOpened)
	  return;

       if ((work == null) || (writer == null))
          throw new IOException("Request is not intialized");

       //write request footer
       writeRequestFooter();

       //close RecordWriter
       writer.flush();
       writer.close();

       File dest = new File(file_path + File.separator + file_name);
       //moving the file
       if (dest.exists())
       {
         //delete old file
         if (!dest.delete())
         {
            throw new IOException("Failed to delete old output file=" + dest.getAbsolutePath());
         }
       }

       //rename 
       if (!work.renameTo(dest))
       {
          //delete working file if cannot rename
          work.deleteOnExit();
          throw new IOException("Failed to rename file=" + work.getAbsolutePath() + " to dest=" + dest.getAbsolutePath());
       }
       isOpened = false;
    }

    /** get request */
    public File getWorkingFile() throws IOException, Exception
    {  
       if ((work == null) || (!isOpened))
	  throw new Exception("Request has not been initialized.");  

       return work;
    }

    public static String getCurrentTimeStamp()
    { 
       Calendar cal = Calendar.getInstance();
       return PASSIVEFILE_DATE.format(cal.getTime());
    }

}
