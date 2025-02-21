package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.composite.CodingDt;
import ca.uhn.fhir.model.dstu2.composite.ResourceReferenceDt;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticOrder;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticReport;
import ca.uhn.fhir.model.dstu2.valueset.DiagnosticReportStatusEnum;
import com.drajer.test.util.TestUtils;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

public class Dstu2CdaPlanOfTreatmentGeneratorTest extends BaseGenerator {

    private static String PLANOFTREATMENTSECTION = "CdaDstuTestData/Cda/PlanOfTreatment/PlanOfTreatment.xml";
    private static String EMPTY_PLAN_OF_TREATMENT_SECTION = "CdaDstuTestData/Cda/PlanOfTreatment/EmptyPlanOfTreatment.xml";

    @Test
    public void generatePlanOfTreatmentSectionTest() {
        String exceptedXml = TestUtils.getFileContentAsString(PLANOFTREATMENTSECTION);
        CodingDt codingDt = new CodingDt();
        codingDt.setCode("ABC");
        codingDt.setSystem("https://abc.org");
        List<CodingDt> codingDtList = new ArrayList<>();
        codingDtList.add(codingDt);
        CodeableConceptDt codeableConceptDt = new CodeableConceptDt();
        codeableConceptDt.setCoding(codingDtList);
        DiagnosticReport diagnosticReport = new DiagnosticReport();
        diagnosticReport.setStatus(DiagnosticReportStatusEnum.FINAL);
        List<ResourceReferenceDt> resourceReferenceDtsList = new ArrayList<>();
        ResourceReferenceDt resourceReferenceDt = new ResourceReferenceDt();
        resourceReferenceDtsList.add(resourceReferenceDt);
        diagnosticReport.setResult(resourceReferenceDtsList);
        diagnosticReport.setCode(codeableConceptDt);
        List<DiagnosticReport> diagnosticReportList = new ArrayList<>();
        diagnosticReportList.add(diagnosticReport);
        dstu2FhirDataForPatient.setDiagReports(diagnosticReportList);
        String actualXml = Dstu2CdaPlanOfTreatmentGenerator.generatePlanOfTreatmentSection(dstu2FhirDataForPatient, launchDetails);
        Assert.assertNotNull(actualXml);
        //assertXmlEquals(exceptedXml, actualXml);
    }

    @Test
    public void getPlannedObservationXmlTest() {
        DiagnosticReport diagnosticReport = new DiagnosticReport();
        String actualXml = Dstu2CdaPlanOfTreatmentGenerator.getPlannedObservationXml(diagnosticReport, launchDetails, "DummyContentRef");
        Assert.assertNotNull(actualXml);
    }

    @Test
    public void getValidDiagnosticReportsTest() {
        CodingDt codingDt = new CodingDt();
        codingDt.setCode("ABC");
        List<CodingDt> codingDtList = new ArrayList<>();
        codingDtList.add(codingDt);
        CodeableConceptDt codeableConceptDt = new CodeableConceptDt();
        codeableConceptDt.setCoding(codingDtList);
        DiagnosticReport diagnosticReport = new DiagnosticReport();
        diagnosticReport.setCode(codeableConceptDt);
        List<DiagnosticReport> inputDiagnosticReportList = new ArrayList<>();
        inputDiagnosticReportList.add(diagnosticReport);
        dstu2FhirDataForPatient.setDiagReports(inputDiagnosticReportList);
        List<DiagnosticReport> diagnosticReportList  = Dstu2CdaPlanOfTreatmentGenerator.getValidDiagnosticReports(dstu2FhirDataForPatient);
        Assert.assertNotNull(diagnosticReportList);
    }

    @Test
    public void getValidDiagnosticOrdersTest() {
        List<DiagnosticOrder> diagnosticOrder  = Dstu2CdaPlanOfTreatmentGenerator.getValidDiagnosticOrders(dstu2FhirDataForPatient);
        Assert.assertNotNull(diagnosticOrder);
    }

    @Test
    public void generateEmptyPlanOfTreatmentSectionTest() {
        String exceptedXml = TestUtils.getFileContentAsString(EMPTY_PLAN_OF_TREATMENT_SECTION);
        String actualXml = Dstu2CdaPlanOfTreatmentGenerator.generateEmptyPlanOfTreatmentSection();
        assertXmlEquals(exceptedXml, actualXml);
    }
}
