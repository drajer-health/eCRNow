package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.composite.CodingDt;
import ca.uhn.fhir.model.dstu2.resource.Condition;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.javatuples.Pair;
import org.junit.Assert;
import org.junit.Test;

import java.util.*;

public class Dstu2CdaProblemGeneratorTest extends BaseGenerator {
    private static String PROBLEMSECTION = "CdaDstuTestData/Cda/Problem/ProblemSection.xml";
    private static String EMPTY_PROBLEM_SECTION = "CdaDstuTestData/Cda/Problem/EmptyProblemSection.xml";

    @Test
    public void generateProblemSectionTest() {
        Condition condition = new Condition();
        CodeableConceptDt codeableConceptDt = new CodeableConceptDt();
        CodingDt codingDt = new CodingDt();
        codingDt.setDisplay("MedicinCode");
        List<CodingDt> codingDtList = new ArrayList<>();
        codingDtList.add(codingDt);
        codeableConceptDt.setCoding(codingDtList);
        condition.setCode(codeableConceptDt);
        List<Condition> conditionList = new ArrayList<>();
        conditionList.add(condition);
        dstu2FhirDataForPatient.setConditions(conditionList);
        String exceptedXml = TestUtils.getFileContentAsString(PROBLEMSECTION);
        String actualXml = Dstu2CdaProblemGenerator.generateProblemSection(dstu2FhirDataForPatient, launchDetails);
        Assert.assertNotNull(actualXml);
        //assertXmlEquals(exceptedXml, actualXml);
    }

    @Test
    public void addTriggerCodesTest() {
        ObjectMapper mapper = new ObjectMapper();
        Condition cond = new Condition();
        TimeZone timeZone = TimeZone.getTimeZone("PST");
        Pair<Date, TimeZone> onset = new Pair<>(new Date(), timeZone);
        Pair<Date, TimeZone> abatement =  new Pair<>(new Date(), timeZone);
        MatchedTriggerCodes matchedTriggerCodes = new MatchedTriggerCodes();
        Set<String> matchedCodes = new HashSet<>();
        matchedCodes.add("MatchedCode");
        matchedTriggerCodes.setMatchedPath("Condition");
        matchedTriggerCodes.setMatchedCodes(matchedCodes);
        List<MatchedTriggerCodes> matchedTriggerCodesList = new ArrayList<>();
        matchedTriggerCodesList.add(matchedTriggerCodes);
       /* PatientExecutionState state = null;
        try {
            state = mapper.readValue(launchDetails.getStatus(), PatientExecutionState.class);
        } catch (JsonMappingException e1) {
            String msg = "Unable to read/write execution state";
            logger.error(msg);
            throw new RuntimeException(msg);

        } catch (JsonProcessingException e1) {
            String msg = "Unable to read/write execution state";
            logger.error(msg);

            throw new RuntimeException(msg);
        }*/
        //state.getMatchTriggerStatus().setMatchedCodes(matchedTriggerCodesList);
        String actualXml = Dstu2CdaProblemGenerator.addTriggerCodes(dstu2FhirDataForPatient, launchDetails, cond
            , onset, abatement);
        Assert.assertNotNull(actualXml);
    }

    @Test
    public void generateEmptyProblemSectionTest() {
        String exceptedXml = TestUtils.getFileContentAsString(EMPTY_PROBLEM_SECTION);
        String actualXml = Dstu2CdaProblemGenerator.generateEmptyProblemSection();
        assertXmlEquals(exceptedXml, actualXml);
    }
}
