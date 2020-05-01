package com.drajer.cda;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;

import ca.uhn.fhir.model.dstu2.composite.CodingDt;
import ca.uhn.fhir.model.dstu2.resource.Observation;


public class CdaResultGenerator {

	private static final Logger logger = LoggerFactory.getLogger(CdaResultGenerator.class);
	
	public static String generateResultsSection(Dstu2FhirData data, LaunchDetails details) {
		
		StringBuilder hsb = new StringBuilder(5000);
		StringBuilder sb = new StringBuilder(2000);
		StringBuilder resultEntries = new StringBuilder();
		
		List<Observation> results = data.getLabResults();
		
		if(results != null && results.size() > 0) {
			
			hsb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
	        hsb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));
	        
	        hsb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.LAB_RESULTS_SEC_TEMPLATE_ID));
            hsb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.LAB_RESULTS_SEC_TEMPLATE_ID, CdaGeneratorConstants.LAB_RESULTS_SEC_TEMPLATE_ID_EXT));

            hsb.append(CdaGeneratorUtils.getXmlForCD(CdaGeneratorConstants.CODE_EL_NAME, 
                    CdaGeneratorConstants.LAB_RESULTS_SEC_CODE, 
                    CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
                    CdaGeneratorConstants.LOINC_CODESYSTEM_NAME, 
                    CdaGeneratorConstants.LAB_RESULTS_SEC_NAME));

            // Add Title
            hsb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.TITLE_EL_NAME, 
                    CdaGeneratorConstants.LAB_RESULTS_SEC_TITLE));

            // Add Narrative Text
            hsb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));
            
            //Create Table Header.
            List<String> list = new ArrayList<String>();
            list.add(CdaGeneratorConstants.LABTEST_TABLE_COL_1_TITLE);
            list.add(CdaGeneratorConstants.LABTEST_TABLE_COL_2_TITLE);
            list.add(CdaGeneratorConstants.LABTEST_TABLE_COL_3_TITLE);
            hsb.append(CdaGeneratorUtils.getXmlForTableHeader(list, 
                CdaGeneratorConstants.TABLE_BORDER, 
                CdaGeneratorConstants.TABLE_WIDTH));

            // Add Table Body
            hsb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));
            
            int rowNum = 1;
            for(Observation obs : results) {
            	
            	String obsDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;
            	List<CodingDt> cds = null;
            	if(obs.getCode() != null && 
                   obs.getCode().getCodingFirstRep() != null && 
                   !StringUtils.isEmpty(obs.getCode().getCodingFirstRep().getDisplay())) {
            		
            		cds = obs.getCode().getCoding();
                 	obsDisplayName =  obs.getCode().getCodingFirstRep().getDisplay();                		
                } 
            	
            	Map<String, String> bodyvals = new HashMap<String, String>();
                bodyvals.put(CdaGeneratorConstants.LABTEST_TABLE_COL_1_BODY_CONTENT, 
                		obsDisplayName);
                
                String val = CdaGeneratorConstants.UNKNOWN_VALUE;
                if(obs.getValue() != null) {
                	
                	val = CdaFhirUtilities.getStringForIDataType(obs.getValue());
                }
                
                bodyvals.put(CdaGeneratorConstants.LABTEST_TABLE_COL_2_BODY_CONTENT, val);
                
                String dt = CdaGeneratorConstants.UNKNOWN_VALUE;
                if(obs.getEffective() != null) {
                
                	dt = CdaFhirUtilities.getStringForIDataType(obs.getEffective());
                }
                bodyvals.put(CdaGeneratorConstants.LABTEST_TABLE_COL_3_BODY_CONTENT, dt);
                
                sb.append(CdaGeneratorUtils.AddTableRow(bodyvals, 
                        rowNum));

            	// Setup the Organizer and Entries
                StringBuilder lrEntry = new StringBuilder();

                // Add the Entries.
                lrEntry.append(CdaGeneratorUtils.getXmlForActEntry(CdaGeneratorConstants.TYPE_CODE_DEF));

                // Add the Organizer Act
                lrEntry.append(CdaGeneratorUtils.getXmlForAct(CdaGeneratorConstants.ORGANIZER_EL_NAME, 
                    CdaGeneratorConstants.ORGANIZER_CLASS_CODE_CLUSTER, 
                    CdaGeneratorConstants.MOOD_CODE_DEF));
                
                lrEntry.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.LAB_RESULTS_ORG_TEMPLATE_ID));
                lrEntry.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.LAB_RESULTS_ORG_TEMPLATE_ID, CdaGeneratorConstants.LAB_RESULTS_ORG_TEMPLATE_ID_EXT));
            
                lrEntry.append(CdaGeneratorUtils.getXmlForIIUsingGuid());
                
                // Fix the Code to be the same as the result code..
                lrEntry.append(CdaGeneratorUtils.getXmlForNullCD(CdaGeneratorConstants.CODE_EL_NAME, 
                        CdaGeneratorConstants.NF_UNK));
                
                lrEntry.append(CdaGeneratorUtils.getXmlForCD(CdaGeneratorConstants.STATUS_CODE_EL_NAME, 
                        CdaGeneratorConstants.COMPLETED_STATUS));
                
                // Add the actual Result Observation 
                lrEntry.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
                lrEntry.append(CdaGeneratorUtils.getXmlForAct(CdaGeneratorConstants.OBS_ACT_EL_NAME, 
                    CdaGeneratorConstants.OBS_CLASS_CODE, 
                    CdaGeneratorConstants.MOOD_CODE_DEF));
                
                lrEntry.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.LAB_RESULTS_ENTRY_TEMPLATE_ID));
                lrEntry.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.LAB_RESULTS_ENTRY_TEMPLATE_ID, CdaGeneratorConstants.LAB_RESULTS_ENTRY_TEMPLATE_ID_EXT));
            
                lrEntry.append(CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), obs.getId().getIdPart()));
                
                lrEntry.append(CdaFhirUtilities.getCodingXml(cds, CdaGeneratorConstants.CODE_EL_NAME));
                
                lrEntry.append(CdaGeneratorUtils.getXmlForCD(CdaGeneratorConstants.STATUS_CODE_EL_NAME, 
                        CdaGeneratorConstants.COMPLETED_STATUS));
                
                lrEntry.append(CdaFhirUtilities.getIDataTypeXml(obs.getEffective(), CdaGeneratorConstants.EFF_TIME_EL_NAME, false));
                
                lrEntry.append(CdaFhirUtilities.getIDataTypeXml(obs.getValue(), CdaGeneratorConstants.EFF_TIME_EL_NAME, true));
                
                // End Tag for Entry Relationship
                lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
                lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

                // End Tags for Entries
                lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ORGANIZER_EL_NAME));
                lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

                resultEntries.append(lrEntry);
                rowNum++;
            }
            
            // End the Sb string.
            sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

            //End Table.
            sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
            sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

            hsb.append(sb);

            // Add lab results
            hsb.append(resultEntries);

            // Complete the section end tags.
            hsb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
            hsb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

            
		}
		else {
			hsb.append(generateEmptyLabResults());
		}
		
		return hsb.toString();
	}
        
	public static String generateEmptyLabResults()
	{
		StringBuilder sb = new StringBuilder();

		// Generate the component and section end tags
		sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
		sb.append(CdaGeneratorUtils.getXmlForNFSection(CdaGeneratorConstants.SECTION_EL_NAME, 
				CdaGeneratorConstants.NF_NI));

		sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.LAB_RESULTS_SEC_TEMPLATE_ID));
		sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.LAB_RESULTS_SEC_TEMPLATE_ID, CdaGeneratorConstants.LAB_RESULTS_SEC_TEMPLATE_ID_EXT));

		sb.append(CdaGeneratorUtils.getXmlForCD(CdaGeneratorConstants.CODE_EL_NAME, 
				CdaGeneratorConstants.LAB_RESULTS_SEC_CODE, 
				CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
				CdaGeneratorConstants.LOINC_CODESYSTEM_NAME, 
				CdaGeneratorConstants.LAB_RESULTS_SEC_NAME));

		// Add Title
		sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.TITLE_EL_NAME, 
				CdaGeneratorConstants.LAB_RESULTS_SEC_TITLE));

		// Add Narrative Text
		sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.TEXT_EL_NAME, 
				"No Lab Results Information"));

		// Complete the section end tags.
		sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
		sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

		return sb.toString();
	}
}
