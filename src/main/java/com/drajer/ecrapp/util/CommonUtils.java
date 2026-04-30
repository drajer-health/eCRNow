package com.drajer.ecrapp.util;

import com.fasterxml.jackson.databind.ObjectMapper;

public class CommonUtils {

  private static final ObjectMapper objectMapper = new ObjectMapper();

  /**
   * This method converts a given object to its string representation using Jackson's ObjectMapper.
   *
   * @param value The object to be converted.
   * @return A string representing the object.
   * @throws Exception If the object cannot be converted to a string.
   */
  public static String writeValueAsString(Object value) throws Exception {
    return objectMapper.writeValueAsString(value);
  }
}
