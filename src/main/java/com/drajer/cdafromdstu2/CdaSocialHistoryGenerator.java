package com.drajer.cdafromdstu2;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;

public class CdaSocialHistoryGenerator {
	
	private static final Logger logger = LoggerFactory.getLogger(CdaSocialHistoryGenerator.class);
	
	public static String generateSocialHistorySection(Dstu2FhirData data, LaunchDetails details) {
		
		StringBuilder sb = new StringBuilder(2000);
		
		// Will have to wait to discuss with vendors on Travel History, Pregnancy, and Birth Sex Observations.
		// Then we can generte the entries. Till then it will be empty section.
		
		sb.append(generateEmptySocialHistorySection());
		
		
		return sb.toString();
	}
	
	public static String generateEmptySocialHistorySection() {
		
		StringBuilder sb = new StringBuilder();

        // Generate the component and section end tags
        sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
        sb.append(CdaGeneratorUtils.getXmlForNFSection(CdaGeneratorConstants.SECTION_EL_NAME, 
            CdaGeneratorConstants.NF_NI));

        sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.SOC_HISTORY_SEC_TEMPLATE_ID));
        sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.SOC_HISTORY_SEC_TEMPLATE_ID, CdaGeneratorConstants.SOC_HISTORY_SEC_TEMPLATE_ID_EXT));

        sb.append(CdaGeneratorUtils.getXmlForCD(CdaGeneratorConstants.CODE_EL_NAME, 
            CdaGeneratorConstants.SOC_HISTORY_SEC_CODE, 
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME, 
            CdaGeneratorConstants.SOC_HISTORY_SEC_NAME));

        // Add Title
        sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.TITLE_EL_NAME, 
            CdaGeneratorConstants.SOC_HISTORY_SEC_TITLE));

        // Add Narrative Text
        sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.TEXT_EL_NAME, 
            "No Social History Information"));

        // Complete the section end tags.
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

		
		return sb.toString();
	}

}
