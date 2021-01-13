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
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import org.apache.commons.io.IOUtils;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.v3.POCDMT000040ClinicalDocument;
import org.hl7.v3.POCDMT000040Component3;
import org.hl7.v3.POCDMT000040Section;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

public class TestUtils {

  private static final Logger logger = LoggerFactory.getLogger(TestUtils.class);

  private static final ObjectMapper mapper = new ObjectMapper();
  private static final ClassLoader classLoader = TestUtils.class.getClassLoader();

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

  public static Object getResourceAsObject(String fileName, Class<?> clazz) {

    Object obj = null;
    try {
      obj = mapper.readValue(classLoader.getResourceAsStream(fileName), clazz);
    } catch (Exception e) {

      logger.info("Error in parsing : " + fileName + " to Object");
    }
    return obj;
  }

  public static String dateToString(Date date, String pattern) {

    if (date != null) {
      SimpleDateFormat formatter = new SimpleDateFormat(pattern);
      return formatter.format(date);
    }
    return null;
  }

  public static POCDMT000040Section getSection(
      POCDMT000040ClinicalDocument cd, String requiredSection) {

    List<POCDMT000040Component3> components = cd.getComponent().getStructuredBody().getComponent();
    POCDMT000040Section section = null;
    String code = null;

    if (ValueSetMapping.sectionConversion.containsKey(requiredSection)) {
      code = ValueSetMapping.sectionConversion.get(requiredSection);
      for (POCDMT000040Component3 component : components) {
        if (component.getSection().getCode().getCode().equals(code)) {
          section = component.getSection();
        }
      }
    }
    return section;
  }

  public static POCDMT000040ClinicalDocument getClinicalDocXml(String eicr) {

    JAXBContext jaxbContext;
    Unmarshaller jaxbUnmarshaller;
    POCDMT000040ClinicalDocument clinicalDoc = null;
    try {
      jaxbContext = JAXBContext.newInstance("org.hl7.v3:org.hl7.sdtc");
      jaxbUnmarshaller = jaxbContext.createUnmarshaller();

      Source source = new StreamSource(IOUtils.toInputStream(eicr.replace("\n", "")));

      JAXBElement<POCDMT000040ClinicalDocument> root =
          jaxbUnmarshaller.unmarshal(source, POCDMT000040ClinicalDocument.class);

      clinicalDoc = root.getValue();
    } catch (JAXBException e) {
      logger.error("Error in unmarshalling EIRC. " + e.getMessage());
    }

    return clinicalDoc;
  }

  public static Document getXmlDocument(String xmlContent)
      throws ParserConfigurationException, SAXException, IOException {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    DocumentBuilder builder = factory.newDocumentBuilder();
    Document document = builder.parse(IOUtils.toInputStream(xmlContent.replace("\n", "")));
    return document;
  }
}
