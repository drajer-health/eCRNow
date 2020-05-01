package com.drajer.cda.utils;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ca.uhn.fhir.model.dstu2.composite.AddressDt;
import ca.uhn.fhir.model.dstu2.composite.CodingDt;
import ca.uhn.fhir.model.dstu2.composite.IdentifierDt;
import ca.uhn.fhir.model.dstu2.valueset.AddressUseEnum;
import ca.uhn.fhir.model.dstu2.valueset.IdentifierTypeCodesEnum;

public class CdaGeneratorUtils {
	
	public static final Logger logger = LoggerFactory.getLogger(CdaGeneratorUtils.class);

	public static String getGuid()
	{
		return java.util.UUID.randomUUID().toString();
	}

	public static String getCurrentDate()
	{
		// SimpleDateFormat formatter= new SimpleDateFormat("yyyy-MM-dd 'at' HH:mm:ss z");
		
		SimpleDateFormat formatter= new SimpleDateFormat("yyyyMMdd");
		Date date = new Date(System.currentTimeMillis());
		return formatter.format(date);
	}

	public static String getCurrentDateTime()
	{
		SimpleDateFormat formatter= new SimpleDateFormat("yyyyMMddHHmmss");
		Date date = new Date(System.currentTimeMillis());
		return formatter.format(date);
	} 
	
	public static String getStringForDate(Date d) {
		
		if(d != null) {			
			SimpleDateFormat formatter= new SimpleDateFormat("yyyyMMddHHmmss");
			return formatter.format(d);
		}
		else {
			
			return CdaGeneratorConstants.UNKNOWN_VALUE;			
		}
	}

	public static String getXmlForStartElement(String name)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + name + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";
		return s;
	}

	public static String getXmlForStartElementWithClassCode(String name, String classCode)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + name + CdaGeneratorConstants.SPACE + CdaGeneratorConstants.CLASSCODE_ATTR_NAME +
				CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE + classCode + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";
		return s;
	}

	public static String getXmlForStartElementWithTypeCode(String name, String typeCode)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + name + CdaGeneratorConstants.SPACE + CdaGeneratorConstants.TYPECODE_ATTR_NAME +
				CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE + typeCode + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";
		return s;
	}

	public static String getXmlForNFSection(String name, String nf)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + name + CdaGeneratorConstants.SPACE + CdaGeneratorConstants.NULL_FLAVOR_NAME +
				CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE + nf + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";
		return s;
	}

	public static String getXmlForEndElement(String name)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.FORWARD_SLASH + name + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";
		return s;
	}

	public static String getXmlHeaderForClinicalDocument()
	{
		String xmlHeader = CdaGeneratorConstants.DOC_HEADER_XML + CdaGeneratorConstants.CLINICAL_DOC_HEADER_XML;
		
		xmlHeader += getXmlForCD(CdaGeneratorConstants.REALM_CODE_EL_NAME, CdaGeneratorConstants.US_REALM_CODE_VAL);
		
		xmlHeader += CdaGeneratorConstants.START_XMLTAG + "typeId root=" + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CDA_DOC_ROOT
						+ CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE + "extension=" + CdaGeneratorConstants.DOUBLE_QUOTE
						+ CdaGeneratorConstants.CDA_DOC_EXT + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN;
		
		xmlHeader += getXmlForTemplateId(CdaGeneratorConstants.US_REALM_HEADER_TEMPLATE_ID);
		xmlHeader += getXmlForTemplateId(CdaGeneratorConstants.US_REALM_HEADER_TEMPLATE_ID, CdaGeneratorConstants.US_REALM_HEADER_EXT);
		xmlHeader += getXmlForTemplateId(CdaGeneratorConstants.PUBLIC_HEALTH_TEMPLATE_ID, CdaGeneratorConstants.PUBLIC_HEALTH_EXT);
		
		return xmlHeader;
	}

	

	public static String getEndXMLHeaderForCdaDocument()
	{
		return CdaGeneratorConstants.END_HEADER_CLINICAL_DOC;
	}

	public static String getXmlForTemplateId(String input)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + "templateId root=" + CdaGeneratorConstants.DOUBLE_QUOTE + input + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getXmlForTemplateId(String input, String extension)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + "templateId root=" + CdaGeneratorConstants.DOUBLE_QUOTE + input + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "extension=" + CdaGeneratorConstants.DOUBLE_QUOTE + extension + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getXmlForII(String input)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + "id root=" + CdaGeneratorConstants.DOUBLE_QUOTE + input + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getXmlForIIUsingGuid()
	{
		String s = CdaGeneratorConstants.START_XMLTAG + "id root=" + CdaGeneratorConstants.DOUBLE_QUOTE + getGuid() + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getNFXMLForII(String nf)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + "id nullFlavor=" + CdaGeneratorConstants.DOUBLE_QUOTE + nf + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getNFXMLFoElement(String element, String nf)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + element + " nullFlavor=" + CdaGeneratorConstants.DOUBLE_QUOTE + nf + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getXmlForCD(String cdName, String code, String codeSystem, String codeSystemName, String displayName)
	{
		if (!StringUtils.isEmpty(displayName))
		{
			String s = CdaGeneratorConstants.START_XMLTAG + cdName + CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "displayName=" + CdaGeneratorConstants.DOUBLE_QUOTE + displayName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.END_XMLTAG_NEWLN;

			return s;
		}
		else
		{
			String s = CdaGeneratorConstants.START_XMLTAG + cdName + CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
					//      CCdaGeneratorConstants.SPACE + "displayName=" + CCdaGeneratorConstants.DOUBLE_QUOTE + displayName + CCdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.END_XMLTAG_NEWLN;

			return s;
		}
	}

	public static String getXmlForCDWithoutEndTag(String cdName, String code, String codeSystem, String codeSystemName, String displayName)
	{
		if (!StringUtils.isEmpty(displayName))
		{
			String s = CdaGeneratorConstants.START_XMLTAG + cdName + CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "displayName=" + CdaGeneratorConstants.DOUBLE_QUOTE + displayName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

			return s;
		}
		else
		{
			String s = CdaGeneratorConstants.START_XMLTAG + cdName + CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

			return s;
		}
	}

	public static String getXmlForCD(String cdName, String code, String codeSystem)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + cdName + CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;

		return s;
	}

	public static String getXmlForCD(String cdName, String code)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + cdName + CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}
	
	public static String getXmlForValue(String elName, String val)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE + "value=" + CdaGeneratorConstants.DOUBLE_QUOTE + val + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getXmlForNullCD(String cdName, String code)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + cdName + CdaGeneratorConstants.SPACE + "nullFlavor=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getXmlForNullValueCD(String cdName, String code)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + cdName + CdaGeneratorConstants.SPACE + "xsi:type=\"CD\"" +
				CdaGeneratorConstants.SPACE +
				"nullFlavor=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getXmlForNullCDWithoutEndTag(String cdName, String code)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + cdName + CdaGeneratorConstants.SPACE + "nullFlavor=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;
		return s;
	}

	public static String getXmlForII(String root, String ext)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + "id root=" + CdaGeneratorConstants.DOUBLE_QUOTE + root + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "extension=" + CdaGeneratorConstants.DOUBLE_QUOTE + ext + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getXmlForIIWithElName(String elName, String root, String ext)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + elName + " root=" + CdaGeneratorConstants.DOUBLE_QUOTE + root + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "extension=" + CdaGeneratorConstants.DOUBLE_QUOTE + ext + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getXmlForIIWithElName(String elName, String root)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + elName + " root=" + CdaGeneratorConstants.DOUBLE_QUOTE + root + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getXmlForText(String elName, String text)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET +
				text + CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.FORWARD_SLASH + elName + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET +
				"\n";
		return s;
	}
	
	public static String getXmlForNFText(String elName, String nf)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE + "nullFlavor=" + 
				CdaGeneratorConstants.DOUBLE_QUOTE + nf + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.FORWARD_SLASH + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET +
				"\n";
		return s;
	}

	public static String getXmlForSDTCElement(String elName, String value)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE + "value=" + CdaGeneratorConstants.DOUBLE_QUOTE +
				value + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getXmlForEffectiveTime(String elName, String value)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE + "value=" + CdaGeneratorConstants.DOUBLE_QUOTE +
				value + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}
	
	public static String getXmlForEffectiveTime(String elName, Date value)
	{
		String s = "";
		if(value != null) {
			SimpleDateFormat formatter= new SimpleDateFormat("yyyyMMdd");
			String val = formatter.format(value);
			
			s += CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE + "value=" + CdaGeneratorConstants.DOUBLE_QUOTE +
					val + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN;
			
		}
		else {
			s += CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE + "nullFlavor=" + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.NF_NI + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN;
		}
		
		return s;
	}


	public static String getXmlForQuantity(String elName, String value)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE + "value=" + CdaGeneratorConstants.DOUBLE_QUOTE +
				value + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getXmlForNullEffectiveTime(String elName, String value)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE + "nullFlavor=" + CdaGeneratorConstants.DOUBLE_QUOTE +
				value + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getXmlForTelecom(String telName, String telNo, String use)
	{
		String s = "";
		
		if(!StringUtils.isEmpty(use) && 
		   telNo.length() == 10) {
			
			s += CdaGeneratorConstants.START_XMLTAG + telName + CdaGeneratorConstants.SPACE + "value="
					+ CdaGeneratorConstants.DOUBLE_QUOTE + "tel:(" + telNo.substring(0, 3) + ")" + telNo.substring(3, 6) + "-"
					+ telNo.substring(6, 10) + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE + "use=" +
					CdaGeneratorConstants.DOUBLE_QUOTE + use + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN;
		}
		else {
			
			s += CdaGeneratorConstants.START_XMLTAG + telName + CdaGeneratorConstants.SPACE + "value="
					+ CdaGeneratorConstants.DOUBLE_QUOTE + "tel:(" + telNo.substring(0, 3) + ")" + telNo.substring(3, 3) + "-"
					+ telNo.substring(6, 4) + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN;
		}

		return s;
	}

	public static String getCDADocHeaderTemplateXML()
	{
		String s = CdaGeneratorConstants.START_XMLTAG + "typeId root=" + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CDA_DOC_ROOT
				+ CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE + "extension=" + CdaGeneratorConstants.DOUBLE_QUOTE
				+ CdaGeneratorConstants.CDA_DOC_EXT + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN;
		return s;
	}

	public static String getHeaderTemplatesXML(String version)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + "typeId root=" + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CDA_DOC_ROOT
				+ CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE + "extension=" + CdaGeneratorConstants.DOUBLE_QUOTE
				+ CdaGeneratorConstants.CDA_DOC_EXT + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN;

		if (version.equals("R51"))
		{
			s += getXmlForTemplateId(CdaGeneratorConstants.CCDA_CCD_TEMPLATE_ID1, CdaGeneratorConstants.CCDA_CCD_TEMPLATE_ID1_EXT);
			s += getXmlForTemplateId(CdaGeneratorConstants.CCDA_CCD_TEMPLATE_ID2, CdaGeneratorConstants.CCDA_CCD_TEMPLATE_ID2_EXT);
		}
		else
		{
			s += getXmlForTemplateId(CdaGeneratorConstants.CCDA_CCD_TEMPLATE_ID1);
			s += getXmlForTemplateId(CdaGeneratorConstants.CCDA_CCD_TEMPLATE_ID2);
		}

		return s;
	}

	public static String getXmlForEntryTemplate(String template, String typeCode)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + template + CdaGeneratorConstants.SPACE
				+ CdaGeneratorConstants.TYPECODE_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
				+ typeCode + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";

		return s;
	}

	public static String getXmlForActEntry(String typeCode)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.ENTRY_EL_NAME + CdaGeneratorConstants.SPACE
				+ CdaGeneratorConstants.TYPECODE_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
				+ typeCode + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";

		return s;
	}

	public static String getXmlForEntryRelationship(String typeCode)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.ENTRY_REL_EL_NAME + CdaGeneratorConstants.SPACE
				+ CdaGeneratorConstants.TYPECODE_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
				+ typeCode + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";

		return s;
	}

	public static String getXmlForEntryRelationship(String typeCode, String invInd)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.ENTRY_REL_EL_NAME + CdaGeneratorConstants.SPACE
				+ CdaGeneratorConstants.TYPECODE_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
				+ typeCode + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE + CdaGeneratorConstants.INV_IND_ATTR_NAME
				+ CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE + invInd + CdaGeneratorConstants.DOUBLE_QUOTE
				+ CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";

		return s;
	}

	public static String getXmlForAct(String actName, String classCode, String moodCode)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + actName + CdaGeneratorConstants.SPACE
				+ CdaGeneratorConstants.CLASSCODE_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
				+ classCode + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE
				+ CdaGeneratorConstants.MOODCODE_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
				+ moodCode + CdaGeneratorConstants.DOUBLE_QUOTE
				+ CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";

		return s;
	}

	public static String getXmlForActWithNegationInd(String actName, String classCode, String moodCode, String negInd, Boolean includeNeg)
	{
		if(negInd != null && negInd.equals("T"))
		{
			String s = CdaGeneratorConstants.START_XMLTAG + actName + CdaGeneratorConstants.SPACE
					+ CdaGeneratorConstants.CLASSCODE_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
					+ classCode + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE
					+ CdaGeneratorConstants.MOODCODE_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
					+ moodCode + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE
					+ CdaGeneratorConstants.NEG_IND_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
					+ "true" + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE
					+ CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";

			return s;
		}
		else if(includeNeg)
		{
			String s = CdaGeneratorConstants.START_XMLTAG + actName + CdaGeneratorConstants.SPACE
					+ CdaGeneratorConstants.CLASSCODE_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
					+ classCode + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE
					+ CdaGeneratorConstants.MOODCODE_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
					+ moodCode + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE
					+ CdaGeneratorConstants.NEG_IND_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
					+ "false" + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE
					+ CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";

			return s;
		}
		else
		{
			return getXmlForAct(actName, classCode, moodCode);
		}
	}

	public static String getXmlForIVL_TS(String elName, String low, String high)
	{
		String s = CdaGeneratorUtils.getXmlForStartElement(elName)
				+ CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_LOW_EL_NAME, low)
				+ CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_HIGH_EL_NAME, high)
				+ CdaGeneratorUtils.getXmlForEndElement(elName);

		return s;
	}
	
	public static String getXmlForIVL_TS(String elName, Date low, Date high)
	{
		String s = "";
		if(low != null && high != null) {
			s += CdaGeneratorUtils.getXmlForStartElement(elName)
						+ CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_LOW_EL_NAME, low)
						+ CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_HIGH_EL_NAME, high)
						+ CdaGeneratorUtils.getXmlForEndElement(elName);
		}
		else if(low != null) {
			s += CdaGeneratorUtils.getXmlForStartElement(elName)
					+ CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_LOW_EL_NAME, low)
					+ CdaGeneratorUtils.getXmlForEndElement(elName);
		}
		else {
			s += CdaGeneratorUtils.getXmlForNullEffectiveTime(elName, CdaGeneratorConstants.NF_NI);
		}

		return s;
	}

	public static String getXmlForValueIVL_TS(String elName, String low, String high)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE +
				CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.IVL_TS_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.RIGHT_ANGLE_BRACKET +
				CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_LOW_EL_NAME, low) +
				CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_HIGH_EL_NAME, high) +
				CdaGeneratorUtils.getXmlForEndElement(elName);
		return s;
	}

	public static String getXmlForLowIVL_TSWithNFHigh(String elName, String value)
	{
		String s = "";

		s += CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE +
				CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.IVL_TS_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n" +
				CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_LOW_EL_NAME, value) +
				CdaGeneratorUtils.getXmlForNullEffectiveTime(CdaGeneratorConstants.TIME_HIGH_EL_NAME, CdaGeneratorConstants.NF_UNK) +
				CdaGeneratorUtils.getXmlForEndElement(elName);

		return s;
	}

	public static String getXmlForPartialValueIVL_TS(String elName, String value, String lowOrHigh)
	{
		String s = "";
		if (lowOrHigh == CdaGeneratorConstants.TIME_LOW_EL_NAME)
		{
			s += CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE +
					CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.IVL_TS_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.RIGHT_ANGLE_BRACKET +
					CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_LOW_EL_NAME, value) +
					CdaGeneratorUtils.getXmlForEndElement(elName);
		}
		else if (lowOrHigh == CdaGeneratorConstants.TIME_HIGH_EL_NAME)
		{
			s += CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE +
					CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.IVL_TS_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.RIGHT_ANGLE_BRACKET +
					CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_HIGH_EL_NAME, value) +
					CdaGeneratorUtils.getXmlForEndElement(elName);
		}

		return s;
	}
	
	public static String getXmlForPIVL_TS(String elName, String frequencyInHours) {
	
		String s = "";
		s += CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE +
				CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.PIVL_TS_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "institutionSpecified=" + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CCDA_TRUE + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "operator=" + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.PIVL_TS_OPERATOR_VAL + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n" +
				CdaGeneratorConstants.START_XMLTAG + "period value=" + CdaGeneratorConstants.DOUBLE_QUOTE +
				frequencyInHours + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE + "unit=" + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.HOURS_UNITS_NAME + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN
				+ CdaGeneratorUtils.getXmlForEndElement(elName);

		return s;
	}

	public static String getXmlForPIVL_TS(String elName, int frequency)
	{
		int hours = 24 / frequency;
		String s = "";

		s += CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE +
				CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.PIVL_TS_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "institutionSpecified=" + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CCDA_TRUE + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "operator=" + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.PIVL_TS_OPERATOR_VAL + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n" +
				CdaGeneratorConstants.START_XMLTAG + "period value=" + CdaGeneratorConstants.DOUBLE_QUOTE +
				Integer.toString(hours) + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE + "unit=" + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.HOURS_UNITS_NAME + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.END_XMLTAG_NEWLN
				+ CdaGeneratorUtils.getXmlForEndElement(elName);

		return s;
	}

	public static String getXmlForValueCD(String code, String codeSystem, String codeSystemName, String displayName)
	{
		if (!StringUtils.isEmpty(displayName))
		{
			String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
					CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CD_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "displayName=" + CdaGeneratorConstants.DOUBLE_QUOTE + displayName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.END_XMLTAG_NEWLN;

			return s;
		}
		else
		{
			String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
					CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CD_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.END_XMLTAG_NEWLN;
			return s;
		}
	}
	
	public static String getXmlForValueCDWithoutEndTag(String code, String codeSystem, String codeSystemName, String displayName)
	{
		if (!StringUtils.isEmpty(displayName))
		{
			String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
					CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CD_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "displayName=" + CdaGeneratorConstants.DOUBLE_QUOTE + displayName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

			return s;
		}
		else
		{
			String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
					CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CD_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;
			return s;
		}
	}

	public static String getXmlForValueCDWithValueSet(String code, String codeSystem, String codeSystemName, String displayName, String valueSet)
	{
		if (!StringUtils.isEmpty(displayName) && !StringUtils.isEmpty(valueSet))
		{
			String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
					CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CD_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "displayName=" + CdaGeneratorConstants.DOUBLE_QUOTE + displayName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "sdtc:valueSet=" + CdaGeneratorConstants.DOUBLE_QUOTE + valueSet + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.END_XMLTAG_NEWLN;

			return s;
		}
		else if(!StringUtils.isEmpty(displayName))
		{
			String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
					CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CD_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "displayName=" + CdaGeneratorConstants.DOUBLE_QUOTE + displayName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.END_XMLTAG_NEWLN;

			return s;
		}
		else
		{
			String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
					CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CD_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.END_XMLTAG_NEWLN;

			return s;
		}
	}

	public static String getXmlForCDWithValueSet(String elName, String code, String codeSystem, String codeSystemName, String displayName, String valueSet)
	{
		if (!StringUtils.isEmpty(displayName) && !StringUtils.isEmpty(valueSet))
		{
			String s = CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE +
					CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "displayName=" + CdaGeneratorConstants.DOUBLE_QUOTE + displayName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "sdtc:valueSet=" + CdaGeneratorConstants.DOUBLE_QUOTE + valueSet + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.END_XMLTAG_NEWLN;

			return s;
		}
		else if (!StringUtils.isEmpty(displayName))
		{
			String s = CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE +
					CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "displayName=" + CdaGeneratorConstants.DOUBLE_QUOTE + displayName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.END_XMLTAG_NEWLN;

			return s;
		}
		else
		{
			String s = CdaGeneratorConstants.START_XMLTAG + elName + CdaGeneratorConstants.SPACE +
					CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
					CdaGeneratorConstants.END_XMLTAG_NEWLN;

			return s;
		}
	}

	public static String getXmlForValueCO(String code, String codeSystem, String codeSystemName, String displayName)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
				CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CO_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "code=" + CdaGeneratorConstants.DOUBLE_QUOTE + code + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "codeSystem=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystem + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "codeSystemName=" + CdaGeneratorConstants.DOUBLE_QUOTE + codeSystemName + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "displayName=" + CdaGeneratorConstants.DOUBLE_QUOTE + displayName + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;

		return s;
	}

	public static String getXmlForValueEd(String value)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
				CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.ED_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + value +
				CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.FORWARD_SLASH + CdaGeneratorConstants.VAL_EL_NAME +
				CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";

		return s;
	}
	
	public static String getXmlForQuantity(String elName, String value, String units, Boolean valFlag)
	{
		if(valFlag) {
			return getXmlForValuePQ(value,units);
		}
		
		return getXmlForQuantity(elName, value);

	}

	public static String getXmlForValuePQ(String value, String units)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
				CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.PQ_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "value=" + CdaGeneratorConstants.DOUBLE_QUOTE + value + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "unit=" + CdaGeneratorConstants.DOUBLE_QUOTE + units + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;

		return s;
	}
	
	public static String getXmlForNullValuePQ(String nf)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
				CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.PQ_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "nullFlavor=" + CdaGeneratorConstants.DOUBLE_QUOTE + nf + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;

		return s;
	}

	public static String getXmlForValueINT(String value)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
				CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.INT_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "value=" + CdaGeneratorConstants.DOUBLE_QUOTE + value + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;

		return s;
	}

	public static String getNFXMLForValue(String nf)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
				CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CD_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "nullFlavor=" + CdaGeneratorConstants.DOUBLE_QUOTE + nf + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.END_XMLTAG_NEWLN;

		return s;
	}

	public static String getNFXMLForValueWithText(String nf, String text)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
				CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CD_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "nullFlavor=" + CdaGeneratorConstants.DOUBLE_QUOTE + nf + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

		s += getXmlForText(CdaGeneratorConstants.ORIGINAL_TEXT_EL_NAME, text);

		s += getXmlForEndElement(CdaGeneratorConstants.VAL_EL_NAME);

		return s;
	}

	public static String getXmlForValueCDTranslation(String code, String codeSystem, String codeSystemName, String displayName)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.VAL_EL_NAME + CdaGeneratorConstants.SPACE +
				CdaGeneratorConstants.XSI_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.CD_TYPE + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.SPACE + "nullFlavor=" + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.NF_OTH + CdaGeneratorConstants.DOUBLE_QUOTE +
				CdaGeneratorConstants.RIGHT_ANGLE_BRACKET;

		s += getXmlForCD(CdaGeneratorConstants.TRANSLATION_EL_NAME, code, codeSystem, codeSystemName, displayName);
		s += getXmlForEndElement(CdaGeneratorConstants.VAL_EL_NAME);

		return s;
	}


	public static String getXmlForPerformer(String perfType)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.PERF_EL_NAME + CdaGeneratorConstants.SPACE
				+ CdaGeneratorConstants.TYPECODE_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
				+ perfType + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";

		return s;
	}

	public static String getXmlForParticipant(String participantType)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.PARTICIPANT_EL_NAME + CdaGeneratorConstants.SPACE
				+ CdaGeneratorConstants.TYPECODE_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
				+ participantType + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";

		return s;
	}

	public static String getXmlForParticipantRole(String classCode)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.PARTICIPANT_ROLE_EL_NAME + CdaGeneratorConstants.SPACE
				+ CdaGeneratorConstants.CLASSCODE_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
				+ classCode + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";

		return s;
	}

	public static String getXmlForTableHeader(List<String> headerVals, int border, int width)
	{
		String brdr = Integer.toString(border);
		String wid = Integer.toString(width) + "%";

		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.TABLE_EL_NAME + CdaGeneratorConstants.SPACE
				+ CdaGeneratorConstants.TABLE_BORDER_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
				+ brdr + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.SPACE
				+ CdaGeneratorConstants.TABLE_WIDTH_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
				+ wid + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";

		s += getXmlForStartElement(CdaGeneratorConstants.TABLE_HEAD_EL_NAME);
		s += getXmlForStartElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME);

		for (String headerval : headerVals)
		{
			s += getXmlForText(CdaGeneratorConstants.TABLE_HEAD_CONTENT_EL_NAME, headerval);
		}


		s += getXmlForEndElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME);
		s += getXmlForEndElement(CdaGeneratorConstants.TABLE_HEAD_EL_NAME);


		return s;

	}

	public static String getXmlForTableBodyContent(String name, String val)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.TABLE_BODY_CONTENT_SUB_EL_NAME + CdaGeneratorConstants.SPACE
				+ CdaGeneratorConstants.TABLE_BODY_CONTENT_ID_EL_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE
				+ name + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET
				+ val + CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.FORWARD_SLASH + CdaGeneratorConstants.TABLE_BODY_CONTENT_SUB_EL_NAME
				+ CdaGeneratorConstants.RIGHT_ANGLE_BRACKET + "\n";

		return s;
	}

	public static String AddTableRow(Map<String, String> vals, int rowNum)
	{
		String s = "";

		s += getXmlForStartElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME);

		for (Map.Entry<String, String> entry : vals.entrySet())
		{
			s += getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_ROW_EL_NAME);

			String name = entry.getKey() + Integer.toString(rowNum);
			s += getXmlForTableBodyContent(name, entry.getValue());

			s += getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_ROW_EL_NAME);
		}

		s += getXmlForEndElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME);

		return s;

	}

	public static String getXmlForReference(String typecode)
	{
		String s = CdaGeneratorConstants.START_XMLTAG + CdaGeneratorConstants.REFR_EL_NAME + CdaGeneratorConstants.SPACE +
				CdaGeneratorConstants.TYPECODE_ATTR_NAME + CdaGeneratorConstants.EQUALS + CdaGeneratorConstants.DOUBLE_QUOTE +
				typecode + CdaGeneratorConstants.DOUBLE_QUOTE + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET +
				"\n";
		return s;
	}
	

}
