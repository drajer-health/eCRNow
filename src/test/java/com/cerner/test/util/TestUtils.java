package com.cerner.test.util;

import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.TimeZone;

import com.fasterxml.jackson.databind.ObjectMapper;

public class TestUtils {
	
	public static String toJson(Object object) throws IOException {
        ObjectMapper mapper = new ObjectMapper();
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS+Z");
        df.setTimeZone(TimeZone.getTimeZone("GMT"));
        mapper.setDateFormat(df);
        
        return mapper.writeValueAsString(object);
    }

}