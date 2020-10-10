package com.drajer.test.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.ecrapp.model.Eicr;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Address.AddressUse;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.codesystems.ConditionClinical;
import org.hl7.v3.AD;
import org.hl7.v3.ANY;
import org.hl7.v3.AdxpCity;
import org.hl7.v3.AdxpCountry;
import org.hl7.v3.AdxpPostalCode;
import org.hl7.v3.AdxpState;
import org.hl7.v3.AdxpStreetAddressLine;
import org.hl7.v3.CD;
import org.hl7.v3.CE;
import org.hl7.v3.II;
import org.hl7.v3.IVLTS;
import org.hl7.v3.PN;
import org.hl7.v3.POCDMT000040ClinicalDocument;
import org.hl7.v3.POCDMT000040Component3;
import org.hl7.v3.POCDMT000040Section;
import org.hl7.v3.QTY;
import org.hl7.v3.StrucDocContent;
import org.hl7.v3.StrucDocTable;
import org.hl7.v3.StrucDocTd;
import org.hl7.v3.StrucDocText;
import org.hl7.v3.StrucDocTh;
import org.hl7.v3.StrucDocThead;
import org.hl7.v3.StrucDocTr;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ValidationUtils {

  private static final Map<String, String> sectionConversion = new HashMap<>();

  static {
    sectionConversion.put("PROBLEMS", "11450-4");
    sectionConversion.put("ENCOUNTERS", "46240-8");
    sectionConversion.put("RESULTS", "30954-2");
    sectionConversion.put("MEDICATIONS", "29549-3");
    sectionConversion.put("IMMUNIZATIONS", "11369-6");
    sectionConversion.put("HISTORY", "29762-2");
    sectionConversion.put("ILLNESS", "10164-2");
    sectionConversion.put("VISITS", "29299-5");
    sectionConversion.put("TREATMENTS", "18776-5");
  }

  private static final Logger logger = LoggerFactory.getLogger(ValidationUtils.class);

  public static POCDMT000040ClinicalDocument getClinicalDocXml(Eicr eicr) {

    JAXBContext jaxbContext;
    Unmarshaller jaxbUnmarshaller;
    POCDMT000040ClinicalDocument clinicalDoc = null;
    try {
      jaxbContext = JAXBContext.newInstance("org.hl7.v3:org.hl7.sdtc");
      jaxbUnmarshaller = jaxbContext.createUnmarshaller();

      Source source = new StreamSource(IOUtils.toInputStream(eicr.getData()));

      JAXBElement<POCDMT000040ClinicalDocument> root =
          jaxbUnmarshaller.unmarshal(source, POCDMT000040ClinicalDocument.class);

      clinicalDoc = root.getValue();
    } catch (JAXBException e) {
      logger.error("Error in unmarshalling EIRC. " + e.getMessage());
    }

    return clinicalDoc;
  }

  public static POCDMT000040Section getSection(
      POCDMT000040ClinicalDocument cd, String requiredSection) {

    List<POCDMT000040Component3> components = cd.getComponent().getStructuredBody().getComponent();
    POCDMT000040Section section = null;
    String code = null;

    if (sectionConversion.containsKey(requiredSection)) {
      code = sectionConversion.get(requiredSection);
      for (POCDMT000040Component3 component : components) {
        if (component.getSection().getCode().getCode().equals(code)) {
          section = component.getSection();
        }
      }
    }
    return section;
  }

  public static void validateAddress(List<Address> r4addr, List<AD> cdaAddr) {

    int idx = -1;

    for (Address addr : r4addr) {

      // Only HOME and WORK addresses are Supported currently.
      // May need to change this validation once more address type are supported.
      if (addr.getUseElement().getValue() == AddressUse.HOME) {

        idx++;
        AD ad = cdaAddr.get(idx);
        assertNotNull(ad);
        validateAddress(addr, ad);

      } else if (addr.getUseElement().getValue() == AddressUse.WORK) {

        idx++;
        AD ad = cdaAddr.get(idx);
        assertNotNull(ad);
        validateAddress(addr, ad);
      }
    }
  }

  public static void validateAddress(Address addr, AD ad) {

    int adIdx = 0;
    // street address
    List<StringType> lines = addr.getLine();
    if (lines != null && lines.size() > 0) {

      for (StringType line : lines) {

        AdxpStreetAddressLine streetAddressLine =
            (AdxpStreetAddressLine) ad.getContent().get(adIdx);
        assertNotNull(streetAddressLine);
        assertEquals(line.getValue(), streetAddressLine.getPartType().get(0));
        adIdx++;
      }
    } else {

      AdxpStreetAddressLine streetAddressLine = (AdxpStreetAddressLine) ad.getContent().get(adIdx);
      assertNotNull(streetAddressLine);
      validateNullFlavor(streetAddressLine, "NI");
      adIdx++;
    }

    // city
    AdxpCity city = (AdxpCity) ad.getContent().get(adIdx);
    assertNotNull(city);
    adIdx++;
    if (addr.getCity() != null) {
      assertEquals(addr.getCity(), city.getPartType().get(0));
    } else {
      validateNullFlavor(city, "NI");
    }

    // state
    AdxpState state = (AdxpState) ad.getContent().get(adIdx);
    assertNotNull(state);
    adIdx++;
    if (addr.getState() != null) {
      assertEquals(addr.getState(), state.getPartType().get(0));
    } else {
      validateNullFlavor(state, "NI");
    }

    // postal code
    AdxpPostalCode postCode = (AdxpPostalCode) ad.getContent().get(adIdx);
    assertNotNull(postCode);
    adIdx++;
    if (addr.getPostalCode() != null) {
      assertEquals(addr.getPostalCode(), postCode.getPartType().get(0));
    } else {
      validateNullFlavor(postCode, "NI");
    }

    // country
    AdxpCountry country = (AdxpCountry) ad.getContent().get(adIdx);
    assertNotNull(country);
    if (addr.getCountry() != null) {
      assertEquals(addr.getCountry(), country.getPartType().get(0));
    } else {
      validateNullFlavor(country, "NI");
    }
  }

  public static void validateCode(
      CE codes, String code, String codeSystem, String codeSystemName, String displayName) {

    assertNotNull(codes);
    assertEquals(code, codes.getCode());
    assertEquals(codeSystem, codes.getCodeSystem());
    assertEquals(codeSystemName, codes.getCodeSystemName());
    assertEquals(displayName, codes.getDisplayName());
  }

  public static void validateIdentifier() {}

  public static void validateTelecom() {}

  public static void validateEffectiveDtTm(IVLTS effDtTm, String high, String low) {

    QTY highTime = (QTY) ((JAXBElement<? extends QTY>) effDtTm.getRest().get(0)).getValue();
    assertEquals(highTime.toString(), high);

    QTY lowTime = (QTY) ((JAXBElement<? extends QTY>) effDtTm.getRest().get(1)).getValue();
    assertEquals(lowTime.toString(), low);
  }

  public static void validateCodeWithTranslation(List<CodeableConcept> codes, CD code) {

    if (codes != null && codes.size() > 0) {

      CodeableConcept cd = codes.get(0);
      List<Coding> codings = cd.getCoding();

      Boolean translation = false;
      int idx = -1;

      for (Coding c : codings) {

        Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

        if (!StringUtils.isEmpty(csd.getValue0())) {

          if (translation == false) {

            translation = true;
            assertEquals(c.getCode(), code.getCode());
            assertEquals(c.getDisplay(), code.getDisplayName());
            assertEquals(csd.getValue0(), code.getCodeSystem());
            assertEquals(csd.getValue1(), code.getCodeSystemName());
          } else {

            idx++;
            CD transCode = code.getTranslation().get(idx);
            assertEquals(c.getCode(), transCode.getCode());
            assertEquals(c.getDisplay(), transCode.getDisplayName());
            assertEquals(csd.getValue0(), transCode.getCodeSystem());
            assertEquals(csd.getValue1(), transCode.getCodeSystemName());
          }
        }
      }
    } else {

      validateNullFlavor(code, "NI");
    }
  }

  public static void validateNullFlavor(ANY instance, String nullFlavor) {
    assertNotNull(instance);
    assertEquals(1, instance.getNullFlavor().size());
    assertEquals(nullFlavor, instance.getNullFlavor().get(0));
  }

  public static void validateName(List<HumanName> r4name, PN cdaName) {}

  public static void validateNarrativeTextWithTable(StrucDocText narrativeText) {

    StrucDocTable table =
        (StrucDocTable) ((JAXBElement<?>) narrativeText.getContent().get(0)).getValue();

    assertNotNull(table);
    validateTableBorderAndWidth(table, "1", "100%");
  }

  public static void validateTableBorderAndWidth(StrucDocTable table, String border, String width) {

    assertNotNull(table);
    assertEquals(border, table.getBorder());
    assertEquals(width, table.getWidth());
  }

  public static void validateTableHeadingTitles(
      StrucDocTable table, List<String> tableHeadingTitles) {
    StrucDocThead thead = table.getThead();
    assertNotNull(thead);
    List<Object> ths = thead.getTr().get(0).getThOrTd();
    assertEquals(tableHeadingTitles.size(), ths.size());
    int loopIndex = 0;
    for (String s : tableHeadingTitles) {
      StrucDocTh th = (StrucDocTh) ths.get(loopIndex++);
      assertEquals(s, th.getContent().get(0));
    }
  }

  public static void validateTableBody(StrucDocTable table, List<Pair<String, String>> rowValues) {

    List<StrucDocTr> trs = table.getTbody().get(0).getTr();

    assertFalse(trs.isEmpty());
    assertEquals(trs.size(), rowValues.size());

    int idx = -1;

    for (StrucDocTr tr : trs) {

      idx++;
      List<Object> tds = tr.getThOrTd();
      // Only two columns added in code, needs to change if code changes.
      assertEquals(2, tds.size());

      // Column1
      StrucDocTd col1 = (StrucDocTd) tds.get(0);
      StrucDocContent rowCol1 =
          (StrucDocContent) (((JAXBElement<?>) col1.getContent().get(1)).getValue());
      assertEquals(rowValues.get(idx).getValue0(), (String) rowCol1.getContent().get(0));

      // Column2
      StrucDocTd col2 = (StrucDocTd) tds.get(1);
      StrucDocContent rowCol2 =
          (StrucDocContent) (((JAXBElement<?>) col2.getContent().get(1)).getValue());
      assertEquals(rowValues.get(idx).getValue1(), (String) rowCol2.getContent().get(0));
    }
  }

  public static void validateTemplateID(II templateID, String root, String extension) {

    assertNotNull(templateID);
    assertEquals(root, templateID.getRoot());
    if (extension != null) {
      assertEquals(extension, templateID.getExtension());
    }
  }

  public static void validateProblemSection(
      List<Condition> r4Conds, POCDMT000040Section problemSection) {

    // TemplateID
    // Only one templateID should be present as per spec.
    validateTemplateID(
        problemSection.getTemplateId().get(0), "2.16.840.1.113883.10.20.22.2.5.1", null);
    validateTemplateID(
        problemSection.getTemplateId().get(1), "2.16.840.1.113883.10.20.22.2.5.1", "2015-08-01");

    // Code
    validateCode(
        problemSection.getCode(), "11450-4", "2.16.840.1.113883.6.1", "LOINC", "PROBLEM LIST");

    // Title - To Do
    // assertEquals(Arrays.asList("PROBLEMS - DIAGNOSES"),
    // problemSection.getTitle().getAny());

    // Narrative Text of type Table
    StrucDocText docText = problemSection.getText();
    StrucDocTable table = (StrucDocTable) ((JAXBElement<?>) docText.getContent().get(1)).getValue();

    validateTableBorderAndWidth(table, "1", "100%");

    List<String> header = new ArrayList<>();
    header.add("Problem or Diagnosis");
    header.add("Problem Status");
    validateTableHeadingTitles(table, header);

    List<Pair<String, String>> rowValues = new ArrayList<>();
    String display = "";
    String status = "";

    for (Condition cond : r4Conds) {

      display = cond.getCode().getCodingFirstRep().getDisplay();
      if (cond.getClinicalStatus()
              .getCodingFirstRep()
              .getCode()
              .contentEquals(ConditionClinical.ACTIVE.toCode())
          || cond.getClinicalStatus()
              .getCodingFirstRep()
              .getCode()
              .contentEquals(ConditionClinical.RELAPSE.toCode())) {

        status = "Active";
      } else {

        status = "Resolved";
      }
      Pair<String, String> row = new Pair<>(display, status);
      rowValues.add(row);
    }
    validateTableBody(table, rowValues);
  }

  public static void validateReasonForVisitSection(
      Encounter r4Encounter, POCDMT000040Section resonForVisitSection) {

    // TemplateID
    validateTemplateID(
        resonForVisitSection.getTemplateId().get(0), "2.16.840.1.113883.10.20.22.2.12", null);
    // Code
    validateCode(
        resonForVisitSection.getCode(),
        "29299-5",
        "2.16.840.1.113883.6.1",
        "LOINC",
        "Reason For Visit");

    // Title - To Do
    // assertEquals(Arrays.asList("Reason For Visit"),
    // problemSection.getTitle().getAny());

    // Narrative Text of type Table
    StrucDocText docText = resonForVisitSection.getText();
    StrucDocTable table = (StrucDocTable) ((JAXBElement<?>) docText.getContent().get(1)).getValue();

    validateTableBorderAndWidth(table, "1", "100%");

    List<String> header = new ArrayList<>();
    header.add("text");
    validateTableHeadingTitles(table, header);

    List<StrucDocTr> trs = table.getTbody().get(0).getTr();
    assertFalse(trs.isEmpty());
    // Only one row
    assertEquals(1, trs.size());
    // Only one column
    assertEquals(1, trs.get(0).getThOrTd().size());

    StrucDocTd col1 = (StrucDocTd) trs.get(0).getThOrTd().get(0);
    StrucDocContent rowCol1 =
        (StrucDocContent) (((JAXBElement<?>) col1.getContent().get(1)).getValue());
    String rowColValue = (String) rowCol1.getContent().get(0);

    if (r4Encounter.getReasonCodeFirstRep() != null) {

      if (!StringUtils.isEmpty(r4Encounter.getReasonCodeFirstRep().getText())) {
        assertEquals(rowColValue, r4Encounter.getReasonCodeFirstRep().getText());
      }
    } else if (r4Encounter.getReasonCodeFirstRep().getCodingFirstRep() != null
        && !StringUtils.isEmpty(
            r4Encounter.getReasonCodeFirstRep().getCodingFirstRep().getDisplay())) {
      assertEquals(
          rowColValue, r4Encounter.getReasonCodeFirstRep().getCodingFirstRep().getDisplay());
    } else {
      assertEquals(rowColValue, "Unknown Reason For Visit");
    }
  }

  public static void validateEncounterSection(
      Encounter r4Encounter, POCDMT000040Section encounterSection) {

    validateTemplateID(
        encounterSection.getTemplateId().get(0), "2.16.840.1.113883.10.20.22.2.22.1", null);

    validateTemplateID(
        encounterSection.getTemplateId().get(1), "2.16.840.1.113883.10.20.22.2.22.1", "2015-08-01");

    validateCode(
        encounterSection.getCode(),
        "46240-8",
        "2.16.840.1.113883.6.1",
        "LOINC",
        "History of Encounters");

    // TODO Title

    StrucDocText encounterText = encounterSection.getText();
    StrucDocTable encounterTable =
        (StrucDocTable) ((JAXBElement<?>) (encounterText.getContent().get(1))).getValue();

    validateTableBorderAndWidth(encounterTable, "1", "100%");
    validateTableHeadingTitles(
        encounterTable,
        new ArrayList<String>(Arrays.asList("Encounter Reason", "Date of Encounter")));

    String encounterName = r4Encounter.getTypeFirstRep().getCodingFirstRep().getDisplay();
    String date = TestUtils.convertToString(r4Encounter.getPeriod().getStart());
    List<Pair<String, String>> rowValues = new ArrayList<>();

    Pair<String, String> row = new Pair<>(encounterName, date);
    rowValues.add(row);

    validateTableBody(encounterTable, rowValues);

    // TODO Entry

  }
}
