package com.drajer.test.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import org.apache.commons.io.IOUtils;

import com.fasterxml.jackson.databind.ObjectMapper;

public class TestUtils {
	
	public static String toJson(Object object) throws IOException {
        ObjectMapper mapper = new ObjectMapper();
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS+Z");
        df.setTimeZone(TimeZone.getTimeZone("GMT"));
        mapper.setDateFormat(df);
        
        return mapper.writeValueAsString(object);
    }
	
	
	//This method compared two buffered readers line by line except for lines that are expected to change in each test
		public static boolean compareStringBuffer(BufferedReader br1, BufferedReader br2, Set<Integer> exceptionSet) throws IOException {
			boolean isSame = false;
			String sCurrentLine;
			List<String> list1 = new ArrayList<String>();
			List<String> list2 = new ArrayList<String>();

			int count = 0;

			while ((sCurrentLine = br1.readLine()) != null) {
				list1.add(sCurrentLine);
			}
			while ((sCurrentLine = br2.readLine()) != null) {
				list2.add(sCurrentLine);
			}

			if (list1.size() != list2.size())
				return false;

			for (int i = 0; i < list1.size(); i++) {
				if (!exceptionSet.contains(i))// skip lines containing transactional data
					if (list1.get(i).equals(list2.get(i))) {
						// System.out.println(list1.get(i));
					} else {
						System.out.println(i);
						count++;
					}

			}
			if (count == 0)
				isSame = true;
			return isSame;
		}

}