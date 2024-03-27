package com.drajer.cda.utils;

import java.text.SimpleDateFormat;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaGeneratorUtils {

  public static Pattern digitPattern = Pattern.compile("[^0-9]");

  private CdaGeneratorUtils() {
    throw new IllegalStateException("Utility class");
  }

  public static final Logger logger = LoggerFactory.getLogger(CdaGeneratorUtils.class);

  public static String getGuid() {
    return java.util.UUID.randomUUID().toString();
  }

  public static String getCurrentDate() {

    SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMdd");
    Date date = new Date(System.currentTimeMillis());
    return formatter.format(date);
  }

  public static String getCurrentDateTime() {
    SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMddHHmmssZ");
    Date date = new Date(System.currentTimeMillis());
    return formatter.format(date);
  }

  public static String getStringForDateTime(Date d, TimeZone t) {

    if (d != null && t != null) {

      ZonedDateTime zt = ZonedDateTime.ofInstant(d.toInstant(), t.toZoneId());
      DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmssZ");
      return zt.format(formatter);
    } else if (d != null) {
      SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMdd");
      return formatter.format(d);
    } else return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  public static String getXmlForStartElement(String name) {
    return CdaGeneratorConstants.START_XMLTAG
        + name
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForStartElementWithAttribute(
      String name, String attrName, String attrVal) {

    if (!StringUtils.isEmpty(attrName) && !StringUtils.isEmpty(attrVal)) {
      return CdaGeneratorConstants.START_XMLTAG
          + name
          + CdaGeneratorConstants.SPACE
          + attrName
          + CdaGeneratorConstants.EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + attrVal
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
          + "\n";
    } else {
      return getXmlForStartElement(name);
    }
  }

  public static String getXmlForStartElementWithClassCode(String name, String classCode) {
    return CdaGeneratorConstants.START_XMLTAG
        + name
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.CLASSCODE_ATTR_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + classCode
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForStartElementWithTypeCode(String name, String typeCode) {
    return CdaGeneratorConstants.START_XMLTAG
        + name
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.TYPECODE_ATTR_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + typeCode
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForNFSection(String name, String nf) {
    return CdaGeneratorConstants.START_XMLTAG
        + name
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.NULL_FLAVOR_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + nf
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForEndElement(String name) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.FORWARD_SLASH
        + name
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlHeaderForClinicalDocument() {
    String xmlHeader =
        CdaGeneratorConstants.DOC_HEADER_XML + CdaGeneratorConstants.CLINICAL_DOC_HEADER_XML;

    xmlHeader +=
        getXmlForCD(
            CdaGeneratorConstants.REALM_CODE_EL_NAME, CdaGeneratorConstants.US_REALM_CODE_VAL);

    xmlHeader +=
        CdaGeneratorConstants.START_XMLTAG
            + CdaGeneratorConstants.TYPEID_ROOT
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.CDA_DOC_ROOT
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.SPACE
            + CdaGeneratorConstants.EXTENSION
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.CDA_DOC_EXT
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.END_XMLTAG_NEWLN;

    xmlHeader += getXmlForTemplateId(CdaGeneratorConstants.CCDA_CCD_TEMPLATE_ID1);
    xmlHeader +=
        getXmlForTemplateId(
            CdaGeneratorConstants.CCDA_CCD_TEMPLATE_ID1, CdaGeneratorConstants.US_REALM_HEADER_EXT);
    xmlHeader +=
        getXmlForTemplateId(
            CdaGeneratorConstants.PUBLIC_HEALTH_TEMPLATE_ID,
            CdaGeneratorConstants.PUBLIC_HEALTH_EXT);

    return xmlHeader;
  }

  public static String getEndXMLHeaderForCdaDocument() {
    return CdaGeneratorConstants.END_HEADER_CLINICAL_DOC;
  }

  public static String getXmlForTemplateId(String input) {
    return CdaGeneratorConstants.START_XMLTAG
        + "templateId root="
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + input
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForTemplateId(String input, String extension) {
    return CdaGeneratorConstants.START_XMLTAG
        + "templateId root="
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + input
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.EXTENSION
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + extension
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForII(String input) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.ID_ROOT
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + input
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForIIUsingGuid() {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.ID_ROOT
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + getGuid()
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getNFXMLForII(String nf) {
    return CdaGeneratorConstants.START_XMLTAG
        + "id nullFlavor="
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + nf
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getNFXMLForElement(String element, String nf) {
    return CdaGeneratorConstants.START_XMLTAG
        + element
        + " nullFlavor="
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + nf
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForCD(
      String cdName, String code, String codeSystem, String codeSystemName, String displayName) {
    if (!StringUtils.isEmpty(displayName)) {
      return CdaGeneratorConstants.START_XMLTAG
          + cdName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(displayName)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;

    } else {
      return CdaGeneratorConstants.START_XMLTAG
          + cdName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    }
  }

  public static String getXmlForCDWithoutEndTag(
      String cdName, String code, String codeSystem, String codeSystemName, String displayName) {
    if (!StringUtils.isEmpty(displayName)) {
      return CdaGeneratorConstants.START_XMLTAG
          + cdName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(displayName)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

    } else {
      return CdaGeneratorConstants.START_XMLTAG
          + cdName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;
    }
  }

  public static String getXmlForCD(String cdName, String code, String codeSystem) {
    return CdaGeneratorConstants.START_XMLTAG
        + cdName
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.CODE_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + StringEscapeUtils.escapeXml10(code)
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + codeSystem
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForCD(String cdName, String code) {
    return CdaGeneratorConstants.START_XMLTAG
        + cdName
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.CODE_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + StringEscapeUtils.escapeXml10(code)
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForValue(String elName, String val) {
    return CdaGeneratorConstants.START_XMLTAG
        + elName
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.VALUE_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + val
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForNullCD(String cdName, String code) {
    return CdaGeneratorConstants.START_XMLTAG
        + cdName
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + code
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForNullValueCD(String cdName, String code) {
    return CdaGeneratorConstants.START_XMLTAG
        + cdName
        + CdaGeneratorConstants.SPACE
        + "xsi:type=\"CD\""
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + code
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForNullValueCDWithoutEndTag(String cdName, String code) {
    return CdaGeneratorConstants.START_XMLTAG
        + cdName
        + CdaGeneratorConstants.SPACE
        + "xsi:type=\"CD\""
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + code
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;
  }

  public static String getXmlForNullCDWithoutEndTag(String cdName, String code) {
    return CdaGeneratorConstants.START_XMLTAG
        + cdName
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + code
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;
  }

  public static String getXmlForNullCDWithText(String cdName, String code, String text) {
    return CdaGeneratorConstants.START_XMLTAG
        + cdName
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + code
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + CdaGeneratorConstants.NEW_LINE
        + getXmlForStartElement(CdaGeneratorConstants.ORIGINAL_TEXT_EL_NAME)
        + text
        + getXmlForEndElement(CdaGeneratorConstants.ORIGINAL_TEXT_EL_NAME)
        + CdaGeneratorConstants.NEW_LINE
        + getXmlForEndElement(cdName);
  }

  public static String getXmlForII(String root, String ext) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.ID_ROOT
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + root
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.EXTENSION
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + ext
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForIIWithElName(String elName, String root, String ext) {
    return CdaGeneratorConstants.START_XMLTAG
        + elName
        + " root="
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + root
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.EXTENSION
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + ext
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForIIWithElName(String elName, String root) {
    return CdaGeneratorConstants.START_XMLTAG
        + elName
        + " root="
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + root
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForText(String elName, String text) {
    return CdaGeneratorConstants.START_XMLTAG
        + elName
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + StringEscapeUtils.escapeXml10(text)
        + CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.FORWARD_SLASH
        + elName
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForElementWithAttribute(
      String elName, String attrName, String attrVal) {

    return CdaGeneratorConstants.START_XMLTAG
        + elName
        + CdaGeneratorConstants.SPACE
        + attrName
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + attrVal
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForTextWithAttribute(
      String elName, String attrName, String attrVal, String text) {

    if (!StringUtils.isEmpty(attrName) && !StringUtils.isEmpty(attrVal)) {
      return CdaGeneratorConstants.START_XMLTAG
          + elName
          + CdaGeneratorConstants.SPACE
          + attrName
          + CdaGeneratorConstants.EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + attrVal
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
          + StringEscapeUtils.escapeXml10(text)
          + CdaGeneratorConstants.START_XMLTAG
          + CdaGeneratorConstants.FORWARD_SLASH
          + elName
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
          + "\n";
    } else {
      return getXmlForText(elName, text);
    }
  }

  public static String getXmlForNFText(String elName, String nf) {
    return CdaGeneratorConstants.START_XMLTAG
        + elName
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + nf
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.FORWARD_SLASH
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForSDTCElement(String elName, String value) {
    return CdaGeneratorConstants.START_XMLTAG
        + elName
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.VALUE_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + value
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForEffectiveTime(String elName, String value) {
    return CdaGeneratorConstants.START_XMLTAG
        + elName
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.VALUE_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + value
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForValueEffectiveTime(String elName, String value) {
    return CdaGeneratorConstants.START_XMLTAG
        + elName
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.XSI_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.TS_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.VALUE_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + value
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getHl7StringForDate(Date value) {

    String s = "";
    if (value != null) {
      SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMddHHmmss");
      s = formatter.format(value);
    }

    return s;
  }

  public static String getXmlForEffectiveTime(String elName, Date value, TimeZone t) {
    String s = "";
    if (value != null) {

      String val = CdaGeneratorUtils.getStringForDateTime(value, t);

      s +=
          CdaGeneratorConstants.START_XMLTAG
              + elName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.VALUE_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + val
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.END_XMLTAG_NEWLN;

    } else {
      s +=
          CdaGeneratorConstants.START_XMLTAG
              + elName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.NF_NI
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    }

    return s;
  }

  public static String getXmlForValueEffectiveTime(String elName, Date value, TimeZone t) {
    String s = "";
    if (value != null) {

      String val = CdaGeneratorUtils.getStringForDateTime(value, t);

      s +=
          CdaGeneratorConstants.START_XMLTAG
              + elName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.XSI_TYPE
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.TS_TYPE
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.VALUE_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + val
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.END_XMLTAG_NEWLN;

    } else {
      s +=
          CdaGeneratorConstants.START_XMLTAG
              + elName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.XSI_TYPE
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.TS_TYPE
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.NF_NI
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    }

    return s;
  }

  public static String getXmlForQuantityWithUnits(String elName, String value, String units) {

    if (StringUtils.isNotEmpty(units))
      return CdaGeneratorConstants.START_XMLTAG
          + elName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + value
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.UNIT_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(units)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    else return getXmlForQuantity(elName, value);
  }

  public static String getXmlForQuantity(String elName, String value) {
    return CdaGeneratorConstants.START_XMLTAG
        + elName
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.VALUE_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + value
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForNfQuantity(String elName, String nf, Boolean valFlag) {

    if (Boolean.FALSE.equals(valFlag)) {
      return CdaGeneratorConstants.START_XMLTAG
          + elName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + nf
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    } else {
      return CdaGeneratorConstants.START_XMLTAG
          + elName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.PQ_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + nf
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    }
  }

  public static String getXmlForNullEffectiveTime(String elName, String value) {
    return CdaGeneratorConstants.START_XMLTAG
        + elName
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + value
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForTelecom(String telName, String telNo, String use, Boolean fax) {

    String s = "";

    String tel = digitPattern.matcher(telNo).replaceAll("");
    String finalTel = tel;

    if (tel.length() > 10) {
      finalTel = tel.substring(tel.length() - 10);
    }

    String telprefix = "tel:(";

    if (fax) {
      telprefix = "fax:(";
    }

    if (!StringUtils.isEmpty(use) && finalTel.length() == 10) {

      s +=
          CdaGeneratorConstants.START_XMLTAG
              + telName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.VALUE_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + telprefix
              + finalTel.substring(0, 3)
              + ")"
              + finalTel.substring(3, 6)
              + "-"
              + finalTel.substring(6, 10)
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + "use="
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + use
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    } else if (finalTel.length() == 10) {

      s +=
          CdaGeneratorConstants.START_XMLTAG
              + telName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.VALUE_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + telprefix
              + finalTel.substring(0, 3)
              + ")"
              + finalTel.substring(3, 6)
              + "-"
              + finalTel.substring(6, 10)
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    } else {

      s +=
          CdaGeneratorConstants.START_XMLTAG
              + telName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.NF_NI
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    }

    return s;
  }

  public static String getXmlForEmail(String telName, String telNo, String use) {
    String s = "";

    if (!StringUtils.isEmpty(use)) {

      s +=
          CdaGeneratorConstants.START_XMLTAG
              + telName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.VALUE_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + "mailto:"
              + telNo
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + "use="
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + use
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    } else {

      s +=
          CdaGeneratorConstants.START_XMLTAG
              + telName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.VALUE_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + "mailto:"
              + telNo
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    }

    return s;
  }

  public static String getCDADocHeaderTemplateXML() {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.TYPEID_ROOT
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.CDA_DOC_ROOT
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.EXTENSION
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.CDA_DOC_EXT
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForEntryTemplate(String template, String typeCode) {
    return CdaGeneratorConstants.START_XMLTAG
        + template
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.TYPECODE_ATTR_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + typeCode
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForActEntry(String typeCode) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.ENTRY_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.TYPECODE_ATTR_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + typeCode
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForEntryRelationship(String typeCode) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.ENTRY_REL_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.TYPECODE_ATTR_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + typeCode
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForEntryRelationship(String typeCode, String invInd) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.ENTRY_REL_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.TYPECODE_ATTR_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + typeCode
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.INV_IND_ATTR_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + invInd
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForAct(String actName, String classCode, String moodCode) {
    return CdaGeneratorConstants.START_XMLTAG
        + actName
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.CLASSCODE_ATTR_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + classCode
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.MOODCODE_ATTR_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + moodCode
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForActWithNegationInd(
      String actName, String classCode, String moodCode, String negInd, Boolean includeNeg) {
    if (negInd != null && (negInd.equalsIgnoreCase("T") || negInd.equalsIgnoreCase("true"))) {
      return CdaGeneratorConstants.START_XMLTAG
          + actName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CLASSCODE_ATTR_NAME
          + CdaGeneratorConstants.EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + classCode
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.MOODCODE_ATTR_NAME
          + CdaGeneratorConstants.EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + moodCode
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.NEG_IND_ATTR_NAME
          + CdaGeneratorConstants.EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + "true"
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
          + "\n";

    } else if (Boolean.TRUE.equals(includeNeg)) {
      return CdaGeneratorConstants.START_XMLTAG
          + actName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CLASSCODE_ATTR_NAME
          + CdaGeneratorConstants.EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + classCode
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.MOODCODE_ATTR_NAME
          + CdaGeneratorConstants.EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + moodCode
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.NEG_IND_ATTR_NAME
          + CdaGeneratorConstants.EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + "false"
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
          + "\n";

    } else {
      return getXmlForAct(actName, classCode, moodCode);
    }
  }

  public static String getXmlForIVLWithTS(String elName, String low, String high) {
    return CdaGeneratorUtils.getXmlForStartElement(elName)
        + CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_LOW_EL_NAME, low)
        + CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_HIGH_EL_NAME, high)
        + CdaGeneratorUtils.getXmlForEndElement(elName);
  }

  public static String getXmlForIVLWithTS(
      String elName,
      Pair<Date, TimeZone> low,
      Pair<Date, TimeZone> high,
      Boolean requireNullFlavor) {
    String s = "";
    if (low != null && high != null) {
      s +=
          CdaGeneratorUtils.getXmlForStartElement(elName)
              + CdaGeneratorUtils.getXmlForEffectiveTime(
                  CdaGeneratorConstants.TIME_LOW_EL_NAME, low.getValue0(), low.getValue1())
              + CdaGeneratorUtils.getXmlForEffectiveTime(
                  CdaGeneratorConstants.TIME_HIGH_EL_NAME, high.getValue0(), high.getValue1())
              + CdaGeneratorUtils.getXmlForEndElement(elName);
    } else if (low != null) {
      s +=
          CdaGeneratorUtils.getXmlForStartElement(elName)
              + CdaGeneratorUtils.getXmlForEffectiveTime(
                  CdaGeneratorConstants.TIME_LOW_EL_NAME, low.getValue0(), low.getValue1())
              + CdaGeneratorUtils.getXmlForEndElement(elName);
    } else if (high != null) {

      s += CdaGeneratorUtils.getXmlForStartElement(elName);
      s +=
          CdaGeneratorUtils.getXmlForNullEffectiveTime(
              CdaGeneratorConstants.TIME_LOW_EL_NAME, CdaGeneratorConstants.NF_NI);
      s +=
          CdaGeneratorUtils.getXmlForEffectiveTime(
              CdaGeneratorConstants.TIME_HIGH_EL_NAME, high.getValue0(), high.getValue1());
      s += CdaGeneratorUtils.getXmlForEndElement(elName);

    } else if (Boolean.TRUE.equals(requireNullFlavor)) {
      s += CdaGeneratorUtils.getXmlForStartElement(elName);
      s +=
          CdaGeneratorUtils.getXmlForNullEffectiveTime(
              CdaGeneratorConstants.TIME_LOW_EL_NAME, CdaGeneratorConstants.NF_NI);
      s +=
          CdaGeneratorUtils.getXmlForNullEffectiveTime(
              CdaGeneratorConstants.TIME_HIGH_EL_NAME, CdaGeneratorConstants.NF_NI);
      s += CdaGeneratorUtils.getXmlForEndElement(elName);

    } else {
      s += CdaGeneratorUtils.getXmlForNullEffectiveTime(elName, CdaGeneratorConstants.NF_NI);
    }

    return s;
  }

  public static String getXmlForValueIVLWithTS(String elName, String low, String high) {

    String retVal =
        CdaGeneratorConstants.START_XMLTAG
            + elName
            + CdaGeneratorConstants.SPACE
            + CdaGeneratorConstants.XSI_TYPE
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.IVL_TS_TYPE
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

    if (!StringUtils.isEmpty(low) && (!CdaGeneratorConstants.UNKNOWN_VALUE.contentEquals(low)))
      retVal +=
          CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_LOW_EL_NAME, low);
    else
      retVal +=
          CdaGeneratorUtils.getXmlForNullEffectiveTime(
              CdaGeneratorConstants.TIME_LOW_EL_NAME, CdaGeneratorConstants.NF_NI);

    if (!StringUtils.isEmpty(high) && (!CdaGeneratorConstants.UNKNOWN_VALUE.contentEquals(high)))
      retVal +=
          CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_HIGH_EL_NAME, high);
    else
      retVal +=
          CdaGeneratorUtils.getXmlForNullEffectiveTime(
              CdaGeneratorConstants.TIME_HIGH_EL_NAME, CdaGeneratorConstants.NF_NI);

    retVal += CdaGeneratorUtils.getXmlForEndElement(elName);

    return retVal;
  }

  public static String getXmlForLowIVLWithTSWithNFHigh(String elName, String value) {
    String s = "";

    s +=
        CdaGeneratorConstants.START_XMLTAG
            + elName
            + CdaGeneratorConstants.SPACE
            + CdaGeneratorConstants.XSI_TYPE
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.IVL_TS_TYPE
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
            + "\n"
            + CdaGeneratorUtils.getXmlForEffectiveTime(
                CdaGeneratorConstants.TIME_LOW_EL_NAME, value)
            + CdaGeneratorUtils.getXmlForNullEffectiveTime(
                CdaGeneratorConstants.TIME_HIGH_EL_NAME, CdaGeneratorConstants.NF_UNK)
            + CdaGeneratorUtils.getXmlForEndElement(elName);

    return s;
  }

  public static String getXmlForPartialValueIVLWithTS(
      String elName, String value, String lowOrHigh) {
    String s = "";
    if (CdaGeneratorConstants.TIME_LOW_EL_NAME.equals(lowOrHigh)) {
      s +=
          CdaGeneratorConstants.START_XMLTAG
              + elName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.XSI_TYPE
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.IVL_TS_TYPE
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
              + CdaGeneratorUtils.getXmlForEffectiveTime(
                  CdaGeneratorConstants.TIME_LOW_EL_NAME, value)
              + CdaGeneratorUtils.getXmlForEndElement(elName);
    } else if (CdaGeneratorConstants.TIME_HIGH_EL_NAME.equals(lowOrHigh)) {
      s +=
          CdaGeneratorConstants.START_XMLTAG
              + elName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.XSI_TYPE
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.IVL_TS_TYPE
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
              + CdaGeneratorUtils.getXmlForEffectiveTime(
                  CdaGeneratorConstants.TIME_HIGH_EL_NAME, value)
              + CdaGeneratorUtils.getXmlForEndElement(elName);
    }

    return s;
  }

  public static String getXmlForPIVLWithTS(String elName, String frequencyInHours) {

    String s = "";
    s +=
        CdaGeneratorConstants.START_XMLTAG
            + elName
            + CdaGeneratorConstants.SPACE
            + CdaGeneratorConstants.XSI_TYPE
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.PIVL_TS_TYPE
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.SPACE
            + "institutionSpecified="
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.CCDA_TRUE
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.SPACE
            + "operator="
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.PIVL_TS_OPERATOR_VAL
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
            + "\n"
            + CdaGeneratorConstants.START_XMLTAG
            + "period value="
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + frequencyInHours
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.SPACE
            + CdaGeneratorConstants.UNIT_WITH_EQUAL
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.HOURS_UNITS_NAME
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.END_XMLTAG_NEWLN
            + CdaGeneratorUtils.getXmlForEndElement(elName);

    return s;
  }

  public static String getXmlForPIVLWithTS(String elName, int frequency) {
    int hours = 24 / frequency;
    String s = "";

    s +=
        CdaGeneratorConstants.START_XMLTAG
            + elName
            + CdaGeneratorConstants.SPACE
            + CdaGeneratorConstants.XSI_TYPE
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.PIVL_TS_TYPE
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.SPACE
            + "institutionSpecified="
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.CCDA_TRUE
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.SPACE
            + "operator="
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.PIVL_TS_OPERATOR_VAL
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
            + "\n"
            + CdaGeneratorConstants.START_XMLTAG
            + "period value="
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + Integer.toString(hours)
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.SPACE
            + CdaGeneratorConstants.UNIT_WITH_EQUAL
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.HOURS_UNITS_NAME
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.END_XMLTAG_NEWLN
            + CdaGeneratorUtils.getXmlForEndElement(elName);

    return s;
  }

  public static String getXmlForValueCD(
      String code, String codeSystem, String codeSystemName, String displayName) {
    if (!StringUtils.isEmpty(displayName)) {
      return CdaGeneratorConstants.START_XMLTAG
          + CdaGeneratorConstants.VAL_EL_NAME
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(displayName)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;

    } else {
      return CdaGeneratorConstants.START_XMLTAG
          + CdaGeneratorConstants.VAL_EL_NAME
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    }
  }

  public static String getXmlForValueCDWithoutEndTag(
      String code, String codeSystem, String codeSystemName, String displayName) {
    if (!StringUtils.isEmpty(displayName)) {
      return CdaGeneratorConstants.START_XMLTAG
          + CdaGeneratorConstants.VAL_EL_NAME
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(displayName)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

    } else {
      return CdaGeneratorConstants.START_XMLTAG
          + CdaGeneratorConstants.VAL_EL_NAME
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;
    }
  }

  public static String getXmlForValueCDWithValueSet(
      String code, String codeSystem, String codeSystemName, String displayName, String valueSet) {
    if (!StringUtils.isEmpty(displayName) && !StringUtils.isEmpty(valueSet)) {
      return CdaGeneratorConstants.START_XMLTAG
          + CdaGeneratorConstants.VAL_EL_NAME
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(displayName)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUESET
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + valueSet
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;

    } else if (!StringUtils.isEmpty(displayName)) {
      return CdaGeneratorConstants.START_XMLTAG
          + CdaGeneratorConstants.VAL_EL_NAME
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(displayName)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;

    } else {
      return CdaGeneratorConstants.START_XMLTAG
          + CdaGeneratorConstants.VAL_EL_NAME
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    }
  }

  public static String getXmlForValueCDWithValueSetAndVersion(
      String code,
      String codeSystem,
      String codeSystemName,
      String valueSet,
      String valuesetVersion,
      String displayName) {
    if (!StringUtils.isEmpty(displayName)
        && !StringUtils.isEmpty(valueSet)
        && (!StringUtils.isEmpty(valuesetVersion))) {
      return CdaGeneratorConstants.START_XMLTAG
          + CdaGeneratorConstants.VAL_EL_NAME
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(displayName)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUESET
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + valueSet
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUESET_VERSION
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + valuesetVersion
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;

    } else if (!StringUtils.isEmpty(valueSet) && (!StringUtils.isEmpty(valuesetVersion))) {

      return CdaGeneratorConstants.START_XMLTAG
          + CdaGeneratorConstants.VAL_EL_NAME
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUESET
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + valueSet
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUESET_VERSION
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + valuesetVersion
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;

    } else if (!StringUtils.isEmpty(displayName)) {
      return CdaGeneratorConstants.START_XMLTAG
          + CdaGeneratorConstants.VAL_EL_NAME
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(displayName)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;

    } else {
      return CdaGeneratorConstants.START_XMLTAG
          + CdaGeneratorConstants.VAL_EL_NAME
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    }
  }

  public static String getXmlForCDWithValueSet(
      String elName,
      String code,
      String codeSystem,
      String codeSystemName,
      String displayName,
      String valueSet) {
    if (!StringUtils.isEmpty(displayName) && !StringUtils.isEmpty(valueSet)) {
      return CdaGeneratorConstants.START_XMLTAG
          + elName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(displayName)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUESET
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + valueSet
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;

    } else if (!StringUtils.isEmpty(displayName)) {
      return CdaGeneratorConstants.START_XMLTAG
          + elName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + code
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(displayName)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;

    } else {
      return CdaGeneratorConstants.START_XMLTAG
          + elName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + code
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.END_XMLTAG_NEWLN;
    }
  }

  public static String getXmlForCDWithValueSetAndVersionWihoutEndTag(
      String codeElName,
      String code,
      String codeSystem,
      String codeSystemName,
      String valueSet,
      String valuesetVersion,
      String displayName) {
    if (!StringUtils.isEmpty(displayName)
        && !StringUtils.isEmpty(valueSet)
        && (!StringUtils.isEmpty(valuesetVersion))) {
      return CdaGeneratorConstants.START_XMLTAG
          + codeElName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(displayName)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUESET
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + valueSet
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUESET_VERSION
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + valuesetVersion
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

    } else if (!StringUtils.isEmpty(valueSet) && (!StringUtils.isEmpty(valuesetVersion))) {

      return CdaGeneratorConstants.START_XMLTAG
          + codeElName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + code
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUESET
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + valueSet
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUESET_VERSION
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + valuesetVersion
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

    } else if (!StringUtils.isEmpty(displayName)) {
      return CdaGeneratorConstants.START_XMLTAG
          + codeElName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + code
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(displayName)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

    } else {
      return CdaGeneratorConstants.START_XMLTAG
          + codeElName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + code
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;
    }
  }

  public static String getXmlForValueCDWithValueSetAndVersionWihoutEndTag(
      String codeElName,
      String code,
      String codeSystem,
      String codeSystemName,
      String valueSet,
      String valuesetVersion,
      String displayName) {
    if (!StringUtils.isEmpty(displayName)
        && !StringUtils.isEmpty(valueSet)
        && (!StringUtils.isEmpty(valuesetVersion))) {
      return CdaGeneratorConstants.START_XMLTAG
          + codeElName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(displayName)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUESET
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + valueSet
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUESET_VERSION
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + valuesetVersion
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

    } else if (!StringUtils.isEmpty(valueSet) && (!StringUtils.isEmpty(valuesetVersion))) {

      return CdaGeneratorConstants.START_XMLTAG
          + codeElName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUESET
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + valueSet
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.VALUESET_VERSION
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + valuesetVersion
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

    } else if (!StringUtils.isEmpty(displayName)) {
      return CdaGeneratorConstants.START_XMLTAG
          + codeElName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(displayName)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

    } else {
      return CdaGeneratorConstants.START_XMLTAG
          + codeElName
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.XSI_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.CD_TYPE
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODE_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + StringEscapeUtils.escapeXml10(code)
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystem
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.SPACE
          + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + codeSystemName
          + CdaGeneratorConstants.DOUBLE_QUOTE
          + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;
    }
  }

  public static String getXmlForCDWithValueSetAndVersion(
      String codeElName,
      String code,
      String codeSystem,
      String codeSystemName,
      String valueSet,
      String valuesetVersion,
      String displayName,
      String contentRef) {
    if (!StringUtils.isEmpty(displayName)
        && !StringUtils.isEmpty(valueSet)
        && (!StringUtils.isEmpty(valuesetVersion))) {
      String retval =
          CdaGeneratorConstants.START_XMLTAG
              + codeElName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.CODE_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + StringEscapeUtils.escapeXml10(code)
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + codeSystem
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + codeSystemName
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + StringEscapeUtils.escapeXml10(displayName)
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.VALUESET
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + valueSet
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.VALUESET_VERSION
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + valuesetVersion
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
              + "\n";

      if (!contentRef.isEmpty()) retval += getXmlForOriginalTextWithReference(contentRef);

      retval += getXmlForEndElement(codeElName);

      return retval;

    } else if (!StringUtils.isEmpty(valueSet) && (!StringUtils.isEmpty(valuesetVersion))) {

      String retval =
          CdaGeneratorConstants.START_XMLTAG
              + codeElName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.CODE_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + code
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + codeSystem
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + codeSystemName
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.VALUESET
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + valueSet
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.VALUESET_VERSION
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + valuesetVersion
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
              + "\n";

      if (!contentRef.isEmpty()) retval += getXmlForOriginalTextWithReference(contentRef);

      retval += getXmlForEndElement(codeElName);

      return retval;

    } else if (!StringUtils.isEmpty(displayName)) {
      String retval =
          CdaGeneratorConstants.START_XMLTAG
              + codeElName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.CODE_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + code
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + codeSystem
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + codeSystemName
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + StringEscapeUtils.escapeXml10(displayName)
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
              + "\n";

      if (!contentRef.isEmpty()) retval += getXmlForOriginalTextWithReference(contentRef);

      retval += getXmlForEndElement(codeElName);

      return retval;

    } else {
      String retval =
          CdaGeneratorConstants.START_XMLTAG
              + codeElName
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.CODE_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + code
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + codeSystem
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.SPACE
              + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + codeSystemName
              + CdaGeneratorConstants.DOUBLE_QUOTE
              + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
              + "\n";

      if (!contentRef.isEmpty()) retval += getXmlForOriginalTextWithReference(contentRef);

      retval += getXmlForEndElement(codeElName);

      return retval;
    }
  }

  public static String getXmlForValueCO(
      String code, String codeSystem, String codeSystemName, String displayName) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.VAL_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.XSI_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.CO_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.CODE_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + StringEscapeUtils.escapeXml10(code)
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.CODESYSTEM_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + codeSystem
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.CODESYSTEMNAME_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + codeSystemName
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + StringEscapeUtils.escapeXml10(displayName)
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForValueEd(String value) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.VAL_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.XSI_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.ED_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + StringEscapeUtils.escapeXml10(value)
        + CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.FORWARD_SLASH
        + CdaGeneratorConstants.VAL_EL_NAME
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForValueString(String value) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.VAL_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.XSI_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.ST_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + StringEscapeUtils.escapeXml10(value)
        + CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.FORWARD_SLASH
        + CdaGeneratorConstants.VAL_EL_NAME
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getNFXmlForValueString(String value) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.VAL_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.XSI_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.ST_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + StringEscapeUtils.escapeXml10(value)
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForQuantityWithUnits(
      String elName, String value, String units, Boolean valFlag) {
    if (Boolean.TRUE.equals(valFlag)) {
      return getXmlForValuePQ(value, units);
    }

    return getXmlForQuantityWithUnits(elName, value, units);
  }

  public static String getXmlForQuantity(
      String elName, String value, String units, Boolean valFlag) {
    if (Boolean.TRUE.equals(valFlag)) {
      return getXmlForValuePQ(value, units);
    }

    return getXmlForQuantity(elName, value);
  }

  public static String getXmlForValuePQ(String value, String units) {
    String defaultUnits = StringUtils.isNotBlank(units) ? units : "1";
    String escapedUnits = StringEscapeUtils.escapeXml10(defaultUnits);

    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.VAL_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.XSI_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.PQ_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.VALUE_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + value
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.UNIT_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + escapedUnits
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForValueBoolean(String val) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.VAL_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.XSI_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.BL_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.VALUE_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + val
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForNullValuePQ(String nf) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.VAL_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.XSI_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.PQ_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + nf
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getXmlForValueINT(String value) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.VAL_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.XSI_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.INT_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.VALUE_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + value
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getNFXMLForValue(String nf) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.VAL_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.XSI_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.CD_TYPE
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + nf
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.END_XMLTAG_NEWLN;
  }

  public static String getNFXMLForValueWithText(String nf, String text) {
    String s =
        CdaGeneratorConstants.START_XMLTAG
            + CdaGeneratorConstants.VAL_EL_NAME
            + CdaGeneratorConstants.SPACE
            + CdaGeneratorConstants.XSI_TYPE
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.CD_TYPE
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.SPACE
            + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + nf
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

    s += getXmlForText(CdaGeneratorConstants.ORIGINAL_TEXT_EL_NAME, text);

    s += getXmlForEndElement(CdaGeneratorConstants.VAL_EL_NAME);

    return s;
  }

  public static String getXmlForValueCDTranslation(
      String code, String codeSystem, String codeSystemName, String displayName) {
    String s =
        CdaGeneratorConstants.START_XMLTAG
            + CdaGeneratorConstants.VAL_EL_NAME
            + CdaGeneratorConstants.SPACE
            + CdaGeneratorConstants.XSI_TYPE
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.CD_TYPE
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.SPACE
            + CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.NF_OTH
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

    s +=
        getXmlForCD(
            CdaGeneratorConstants.TRANSLATION_EL_NAME,
            code,
            codeSystem,
            codeSystemName,
            displayName);
    s += getXmlForEndElement(CdaGeneratorConstants.VAL_EL_NAME);

    return s;
  }

  public static String getXmlForOriginalTextWithReference(String refText) {

    return CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ORIGINAL_TEXT_EL_NAME)
        + CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.REFR_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.VALUE_WITH_EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + "#"
        + refText
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.FORWARD_SLASH
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n"
        + CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ORIGINAL_TEXT_EL_NAME);
  }

  public static String getXmlForPerformer(String perfType) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.PERF_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.TYPECODE_ATTR_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + perfType
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForParticipant(String participantType) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.PARTICIPANT_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.TYPECODE_ATTR_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + participantType
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForParticipantRole(String classCode) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.PARTICIPANT_ROLE_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.CLASSCODE_ATTR_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + classCode
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getXmlForTableHeader(List<String> headerVals, int border, int width) {
    String brdr = Integer.toString(border);
    String wid = Integer.toString(width) + "%";

    String s =
        CdaGeneratorConstants.START_XMLTAG
            + CdaGeneratorConstants.TABLE_EL_NAME
            + CdaGeneratorConstants.SPACE
            + CdaGeneratorConstants.TABLE_BORDER_ATTR_NAME
            + CdaGeneratorConstants.EQUAL
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + brdr
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.SPACE
            + CdaGeneratorConstants.TABLE_WIDTH_ATTR_NAME
            + CdaGeneratorConstants.EQUAL
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + wid
            + CdaGeneratorConstants.DOUBLE_QUOTE
            + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
            + "\n";

    StringBuilder sb = new StringBuilder();
    sb.append(s);
    sb.append(getXmlForStartElement(CdaGeneratorConstants.TABLE_HEAD_EL_NAME));
    sb.append(getXmlForStartElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME));

    for (String headerval : headerVals) {
      sb.append(getXmlForText(CdaGeneratorConstants.TABLE_HEAD_CONTENT_EL_NAME, headerval));
    }

    sb.append(getXmlForEndElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME));
    sb.append(getXmlForEndElement(CdaGeneratorConstants.TABLE_HEAD_EL_NAME));

    return sb.toString();
  }

  public static String getXmlForTableBodyContent(String name, String val) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.TABLE_BODY_CONTENT_SUB_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.TABLE_BODY_CONTENT_ID_EL_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + name
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + StringEscapeUtils.escapeXml10(val)
        + CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.FORWARD_SLASH
        + CdaGeneratorConstants.TABLE_BODY_CONTENT_SUB_EL_NAME
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String addTableRow(Map<String, String> vals, int rowNum) {

    StringBuilder sb = new StringBuilder();
    sb.append(getXmlForStartElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME));

    for (Map.Entry<String, String> entry : vals.entrySet()) {
      sb.append(getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_ROW_EL_NAME));

      String name = entry.getKey() + Integer.toString(rowNum);
      sb.append(getXmlForTableBodyContent(name, entry.getValue()));

      sb.append(getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_ROW_EL_NAME));
    }

    sb.append(getXmlForEndElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME));
    return sb.toString();
  }

  public static String getXmlForReference(String typecode) {
    return CdaGeneratorConstants.START_XMLTAG
        + CdaGeneratorConstants.REFR_EL_NAME
        + CdaGeneratorConstants.SPACE
        + CdaGeneratorConstants.TYPECODE_ATTR_NAME
        + CdaGeneratorConstants.EQUAL
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + typecode
        + CdaGeneratorConstants.DOUBLE_QUOTE
        + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
        + "\n";
  }

  public static String getRootOid(String systemValue, String aaId) {

    if (systemValue != null && systemValue.contains("urn:oid")) {
      return systemValue.replace("urn:oid:", "");
    } else {

      Pair<String, String> system = CdaGeneratorConstants.getOID(systemValue);

      if (!system.getValue0().isEmpty()) {
        return system.getValue0();
      } else return aaId;
    }
  }
}
