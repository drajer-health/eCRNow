package com.drajer.test.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

public class TestUtils {

  private static final Logger logger = LoggerFactory.getLogger(TestUtils.class);

  private static final ObjectMapper mapper = new ObjectMapper();
  private static final ClassLoader classLoader = TestUtils.class.getClassLoader();

  public static ObjectMapper getJsonMapper() {
    return mapper;
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

  public static Object getResourceAsObject(String fileName, Class<?> clazz) {

    Object obj = null;
    try {
      obj = mapper.readValue(classLoader.getResourceAsStream(fileName), clazz);
    } catch (Exception e) {

      logger.info("Error in parsing : " + fileName + " to Object");
    }
    return obj;
  }

  public static Document getXmlDocument(String xmlContent)
      throws ParserConfigurationException, SAXException, IOException {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
    factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
    DocumentBuilder builder = factory.newDocumentBuilder();
    Document document = builder.parse(IOUtils.toInputStream(xmlContent.replace("\n", "")));
    return document;
  }
}
