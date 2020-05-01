package com.drajer.cda;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;

public class CdaReasonForVisitGenerator {
	
private static final Logger logger = LoggerFactory.getLogger(CdaReasonForVisitGenerator.class);
	
	public static String generateReasonForVisitSection(Dstu2FhirData data, LaunchDetails details) {
		
		StringBuilder sb = new StringBuilder(2000);
		
		// Will have to wait to discuss with vendors on Reason For Visit and how to obtain that information reliably..
		// Then we can generate better text.
		
		sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
        sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));
        
        sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.REASON_FOR_VISIT_SEC_TEMPLATE_ID));
		
        sb.append(CdaGeneratorUtils.getXmlForCD(CdaGeneratorConstants.CODE_EL_NAME, 
                CdaGeneratorConstants.REASON_FOR_VISIT_SEC_CODE, 
                CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
                CdaGeneratorConstants.LOINC_CODESYSTEM_NAME, 
                CdaGeneratorConstants.REASON_FOR_VISIT_SEC_CODE_NAME));
        
        // Add Title
        sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.TITLE_EL_NAME, 
            CdaGeneratorConstants.REASON_FOR_VISIT_SEC_TITLE));

        // Add Narrative Text
        // Need to Discuss with vendors on how to best get this information. 
        sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.TEXT_EL_NAME, 
            "Reason Not Known"));

        // Complete the section end tags.
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

		
		return sb.toString();
	}

}
