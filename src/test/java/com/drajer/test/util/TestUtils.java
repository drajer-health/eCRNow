package com.drajer.test.util;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.apache.commons.io.IOUtils;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Resource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

public class TestUtils {

  private static final Logger logger = LoggerFactory.getLogger(TestUtils.class);

  static ObjectMapper mapper = new ObjectMapper();

  public static String toJson(Object object) throws IOException {
    ObjectMapper mapper = new ObjectMapper();
    DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS+Z");
    df.setTimeZone(TimeZone.getTimeZone("GMT"));
    mapper.setDateFormat(df);

    return mapper.writeValueAsString(object);
  }

  // This method compared two buffered readers line by line except for lines that
  // are expected to
  // change in each test
  public static boolean compareStringBuffer(
      BufferedReader br1, BufferedReader br2, Set<Integer> exceptionSet) throws IOException {
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

    if (list1.size() != list2.size()) return false;

    for (int i = 0; i < list1.size(); i++) {
      if (!exceptionSet.contains(i)) // skip lines containing transactional data
      if (list1.get(i).equals(list2.get(i))) {
          // System.out.println(list1.get(i));
        } else {
          System.out.println(i);
          count++;
        }
    }
    if (count == 0) isSame = true;
    return isSame;
  }

  public static String getFileContentAsString(String fileName) {
    String fileContent = "";
    InputStream stream = TestUtils.class.getResourceAsStream("/" + fileName);
    StringWriter writer = new StringWriter();
    try {
      IOUtils.copy(stream, writer, StandardCharsets.UTF_8);
      fileContent = writer.toString();
      stream.close();
      writer.close();
    } catch (Exception e) {
      logger.error("File not found::" + fileName);
    }

    return fileContent;
  }

  public static Document getXmlDocument(String expectedXml)
      throws ParserConfigurationException, SAXException, IOException {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    DocumentBuilder builder = factory.newDocumentBuilder();
    Document document = builder.parse(TestUtils.class.getResourceAsStream("/" + expectedXml));
    return document;
  }

  public static Bundle getExpectedBundle(FhirContext context, String expectedBundle)
      throws IOException {
    final IParser jsonParser = context.newJsonParser();

    return jsonParser.parseResource(Bundle.class, expectedBundle);
  }

  public static Resource getResourceFromBundle(Bundle bundle, Class<?> resource) {
    try {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        if (entry.getResource() != null) {
          if (entry.getResource().getClass() == resource) {
            return entry.getResource();
          }
        }
      }
    } catch (Exception e) {
      logger.error("Error in getting the Resource from Bundle");
    }
    return null;
  }

  public static List<Resource> getResourcesFromBundle(Bundle bundle, Class<?> resource) {

    List<Resource> resources = new ArrayList<>();
    try {
      for (BundleEntryComponent entry : bundle.getEntry()) {
        if (entry.getResource() != null) {
          if (entry.getResource().getClass() == resource) {
            resources.add(entry.getResource());
          }
        }
      }
    } catch (Exception e) {
      logger.error("Error in getting the Resource from Bundle");
    }
    return resources;
  }

  public static Bundle getR4BundleFromJson(String fileName) {

    String response = getFileContentAsString(fileName);
    Bundle bundle = FhirContext.forR4().newJsonParser().parseResource(Bundle.class, response);
    return bundle;
  }

  public static Object getR4ResourceFromJson(String fileName, Class clazz) {

    String response = getFileContentAsString(fileName);
    return FhirContext.forR4().newJsonParser().parseResource(clazz, response);
  }

  public static ObjectMapper getmapperObject() {
    return mapper;
  }

  public static Object getFileContentAsObject(String fileName, Class clazz) {
    String content = getFileContentAsString(fileName);
    Object obj = null;
    try {
      obj = mapper.readValue(content, clazz);
    } catch (Exception e) {

      logger.info("Error in parsing json : " + fileName + " to Object");
    }
    return obj;
  }

  public static String convertToString(Date date, String pattern) {

    if (date != null) {
      SimpleDateFormat formatter = new SimpleDateFormat(pattern);
      return formatter.format(date);
    }
    return null;
  }
}
