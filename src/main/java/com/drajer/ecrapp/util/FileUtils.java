package com.drajer.ecrapp.util;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FileUtils {

  private static final Logger logger = LoggerFactory.getLogger(FileUtils.class);

  private static final ObjectMapper mapper = new ObjectMapper();

  public static <T> T readFileContents(String filename, TypeReference<T> typeReference) {
    T object = null;
    try (InputStream inputStream = new FileInputStream(new File(filename))) {
      object = mapper.readValue(inputStream, typeReference);
    } catch (Exception e) {
      logger.error("Error reading file {}: {}", filename, e.getMessage());
    }
    return object;
  }

  /**
   * The method saves the provided data to a file.
   *
   * @param data -- The data to be saved.
   * @param filename -- The filename to be used for saving the data.
   */
  public static void saveDataToFile(String data, String filename) {

    try (DataOutputStream outStream =
        new DataOutputStream(new BufferedOutputStream(new FileOutputStream(filename)))) {

      logger.info(" Writing data to file: {}", filename);
      outStream.writeBytes(data);
    } catch (IOException e) {
      logger.debug(" Unable to write data to file: {}", filename, e);
    }
  }
}
