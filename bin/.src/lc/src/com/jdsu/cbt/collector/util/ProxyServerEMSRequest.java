/**
 * @author mur50399
 *
 */

package com.jdsu.cbt.collector.util;

import com.jdsu.cbt.collector.util.ProxyServerInteraction;

public class ProxyServerEMSRequest extends ProxyServerRequest{
	public static String REQUEST_FILE_PATTERN = "_request_";

	public ProxyServerEMSRequest(ProxyServerInteraction interaction, String ems_id, String ne_id) throws Exception
	{
		this(interaction, ems_id, ne_id, getCurrentTimeStamp(), REQUEST_FILE_PATTERN);
	}

	public ProxyServerEMSRequest(ProxyServerInteraction interaction, String ems_id, 
			String ne_id, String timestamp) throws Exception{
		this(interaction, ems_id, ne_id, timestamp, REQUEST_FILE_PATTERN);
	}

	public ProxyServerEMSRequest(ProxyServerInteraction interaction, String ems_id, 
			String ne_id, String timestamp, String request_file_pattern) throws Exception {
		super(interaction.getDataInputDirectoryForEMS(ems_id), ne_id + request_file_pattern + 
				getCurrentTimeStamp() + REQUEST_POSTFIX, timestamp);
	}
}
