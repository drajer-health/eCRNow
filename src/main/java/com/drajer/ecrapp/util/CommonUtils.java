package com.drajer.ecrapp.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public class CommonUtils {

  private CommonUtils() {
    // Utility class - private constructor to hide the implicit public one
  }

  private static final ObjectMapper objectMapper = new ObjectMapper();

  /**
   * This method converts a given object to its string representation using Jackson's ObjectMapper.
   *
   * @param value The object to be converted.
   * @return A string representing the object.
   * @throws JsonProcessingException If the object cannot be converted to a string.
   */
  public static String writeValueAsString(Object value) throws JsonProcessingException {
    return objectMapper.writeValueAsString(value);
  }
}
