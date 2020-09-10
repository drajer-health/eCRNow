package com.drajer.cdafromR4;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Identifier;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;

public class CdaEncounterGenerator {

	private static final Logger logger = LoggerFactory.getLogger(CdaEncounterGenerator.class);
	
	public static String generateEncounterSection(R4FhirData data, LaunchDetails details) {
		
		StringBuilder sb = new StringBuilder(2000);
		Encounter encounter = data.getEncounter();
		
		if(encounter != null) {
			
			logger.info(" Generating Encounter section ");
			
			sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
	        sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));
	        
	        sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.ENC_SEC_TEMPLATE_ID));
            sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.ENC_SEC_TEMPLATE_ID, CdaGeneratorConstants.ENC_SEC_TEMPLATE_ID_EXT));
	        
            sb.append(CdaGeneratorUtils.getXmlForCD(CdaGeneratorConstants.CODE_EL_NAME, 
                    CdaGeneratorConstants.ENC_SEC_CODE, 
                    CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
                    CdaGeneratorConstants.LOINC_CODESYSTEM_NAME, 
                    CdaGeneratorConstants.ENC_SEC_NAME));

            // Add Title
            sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.TITLE_EL_NAME, 
                    CdaGeneratorConstants.ENC_SEC_TITLE));

            // Add Narrative Text
            sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));
            
            //Create Table Header.
            List<String> list = new ArrayList<String>();
            list.add(CdaGeneratorConstants.ENC_TABLE_COL_1_TITLE);
            list.add(CdaGeneratorConstants.ENC_TABLE_COL_2_TITLE);

            sb.append(CdaGeneratorUtils.getXmlForTableHeader(list, 
                CdaGeneratorConstants.TABLE_BORDER, 
                CdaGeneratorConstants.TABLE_WIDTH));
            
            // Add Body
            sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));
            
            String actDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;
        	
        	if(encounter.getTypeFirstRep() != null && 
        	   encounter.getTypeFirstRep().getCodingFirstRep() != null && 
        	   !StringUtils.isEmpty(encounter.getTypeFirstRep().getCodingFirstRep().getDisplay())) {
        		
        		actDisplayName =  encounter.getTypeFirstRep().getCodingFirstRep().getDisplay();		
        	}
        	
        	String dt = CdaGeneratorConstants.UNKNOWN_VALUE;
        	if(encounter.getPeriod() != null &&
        			encounter.getPeriod().getStart() != null) {
        		
        		dt = CdaGeneratorUtils.getStringForDate(encounter.getPeriod().getStart());
        		
        	}
	        
            Map<String, String> bodyvals = new HashMap<String, String>();
            bodyvals.put(CdaGeneratorConstants.ENC_TABLE_COL_1_BODY_CONTENT, actDisplayName);
            bodyvals.put(CdaGeneratorConstants.ENC_TABLE_COL_2_BODY_CONTENT, dt);
            
            sb.append(CdaGeneratorUtils.AddTableRow(bodyvals, 1));
            
            sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

            //End Table.
            sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
            sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));
            
            // Add the Entries.
            sb.append(CdaGeneratorUtils.getXmlForActEntry(CdaGeneratorConstants.TYPE_CODE_DEF));
            sb.append(CdaGeneratorUtils.getXmlForAct(CdaGeneratorConstants.ENC_ACT_EL_NAME, 
                CdaGeneratorConstants.ENC_CLASS_CODE, 
                CdaGeneratorConstants.MOOD_CODE_DEF));

            sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.ENC_ENTRY_TEMPLATE_ID));
            sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.ENC_ENTRY_TEMPLATE_ID, CdaGeneratorConstants.ENC_ENTRY_TEMPLATE_ID_EXT));
            
            sb.append(CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), encounter.getId()));
            
            //Add Identifiers
			List<Identifier> ids = encounter.getIdentifier();
			if(ids != null) {
				
				for(Identifier id : ids) {
					
					if(id.getSystem() != null && id.getValue() != null) {
						
						sb.append(CdaGeneratorUtils.getXmlForII(CdaGeneratorUtils.getRootOid(id.getSystem(),  details.getAssigningAuthorityId()), id.getValue()));
					}
				}
			}
            
            List<CodeableConcept> cds = encounter.getType();
            sb.append(CdaFhirUtilities.getCodeableConceptXml(cds, CdaGeneratorConstants.CODE_EL_NAME, false));
            
            sb.append(CdaFhirUtilities.getPeriodXml(encounter.getPeriod(), CdaGeneratorConstants.EFF_TIME_EL_NAME));
            
            // End Entry Tags
            sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENC_ACT_EL_NAME));
            sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));
            
            // Complete the section end tags.
            sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
            sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

            
		}
		else {
			
			sb.append(generateEmptyEncounterSection());
		}
		
		
		return sb.toString();
	}
	
	public static String generateEmptyEncounterSection()
	{
	    StringBuilder sb = new StringBuilder();
	
	    // Generate the component and section end tags
	    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
	    sb.append(CdaGeneratorUtils.getXmlForNFSection(CdaGeneratorConstants.SECTION_EL_NAME, 
	        CdaGeneratorConstants.NF_NI));
	
	    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.ENC_SEC_TEMPLATE_ID));
	    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.ENC_SEC_TEMPLATE_ID, CdaGeneratorConstants.ENC_SEC_TEMPLATE_ID_EXT));
	   
	    
	    sb.append(CdaGeneratorUtils.getXmlForCD(CdaGeneratorConstants.CODE_EL_NAME, 
	        CdaGeneratorConstants.ENC_SEC_CODE, 
	        CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
	        CdaGeneratorConstants.LOINC_CODESYSTEM_NAME, 
	        CdaGeneratorConstants.ENC_SEC_NAME));
	
	    // Add Title
	    sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.TITLE_EL_NAME, 
	        CdaGeneratorConstants.ENC_SEC_TITLE));
	
	    // Add Narrative Text
	    sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.TEXT_EL_NAME, 
	        "No Encounter Information"));
	
	    // Complete the section end tags.
	    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
	    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));
	
	    return sb.toString();
	}
		
	
}
