package com.jdsu.tools.KPIAggregator;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Random;
import java.util.TimeZone;

public class KPIAggregatorTest implements Runnable {
	public void run() {

		Calendar cal = null;
		try {	
			cal = Calendar.getInstance();
			System.out.println(cal.get(Calendar.HOUR_OF_DAY)+":"+cal.get(Calendar.MINUTE)+":"+
					   cal.get(Calendar.SECOND)+"."+cal.get(Calendar.MILLISECOND));
			
			KPIAggregator kpiAggregator = new KPIAggregator();

			// Add the virtual types
			kpiAggregator.GetVirtualTypeIDs();

			cal = Calendar.getInstance();
			System.out.println(cal.get(Calendar.HOUR_OF_DAY)+":"+cal.get(Calendar.MINUTE)+":"+
					   cal.get(Calendar.SECOND)+"."+cal.get(Calendar.MILLISECOND));
		
			// Store the values
			kpiAggregator.storeAllKPIValues();

			cal = Calendar.getInstance();
			System.out.println(cal.get(Calendar.HOUR_OF_DAY)+":"+cal.get(Calendar.MINUTE)+":"+
					   cal.get(Calendar.SECOND)+"."+cal.get(Calendar.MILLISECOND));
		}
		catch (Exception ex) {
			ex.printStackTrace(System.err);
			System.exit(-1);
		}
	}

        public static void main(String[] args) throws Exception {
		KPIAggregatorTest bm = new KPIAggregatorTest();
		bm.run();
	}
}

