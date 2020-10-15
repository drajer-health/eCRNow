package com.drajer.test.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
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
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Identifier;
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
import org.hl7.v3.POCDMT000040Entry;
import org.hl7.v3.POCDMT000040EntryRelationship;
import org.hl7.v3.POCDMT000040Observation;
import org.hl7.v3.POCDMT000040Section;
import org.hl7.v3.QTY;
import org.hl7.v3.StrucDocContent;
import org.hl7.v3.StrucDocTable;
import org.hl7.v3.StrucDocTd;
import org.hl7.v3.StrucDocText;
import org.hl7.v3.StrucDocTh;
import org.hl7.v3.StrucDocThead;
import org.hl7.v3.StrucDocTr;
import org.hl7.v3.TEL;
import org.hl7.v3.TS;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ValidationUtils {

  private static final Map<String, String> sectionConversion = new HashMap<>();

  private static LaunchDetails launchDetails;

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

  public static void setLaunchDetails(LaunchDetails launchDetails) {
    ValidationUtils.launchDetails = launchDetails;
  }

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

  public static void validateIdentifier(
      List<Identifier> r4Identifiers, List<II> cdaIdentifiers, String entityId) {

    int idx = 0;
    if (entityId != null && !entityId.isEmpty()) {
      // ToDo - hardcoded need to change to retrieve for clientdetails.
      validateID(cdaIdentifiers.get(idx++), launchDetails.getAssigningAuthorityId(), entityId);
    }

    for (Identifier id : r4Identifiers) {

      String root;
      if (id.getSystem() != null && id.getValue() != null) {
        if (id.getSystem().contains("urn:oid")) {
          root = id.getSystem().replace("urn:oid:", "");
        } else {
          // ToDo - hardcoded need to change to retrieve for clientdetails.
          root = launchDetails.getAssigningAuthorityId();
        }

        validateID(cdaIdentifiers.get(idx++), root, id.getValue());
      }
    }
  }

  public static void validateTelecom(List<ContactPoint> cr4Contacts, List<TEL> telecoms) {}

  public static void validateConditionEffectiveDtTm(Condition cond, IVLTS effDtTm) {
    String onset = null;
    String abatement = null;
    if (cond.getOnset() != null && cond.getOnset() instanceof DateTimeType) {
      DateTimeType dt = (DateTimeType) cond.getOnset();
      onset = TestUtils.convertToString(dt.getValue(), "yyyyMMdd");
    }
    if (cond.getAbatement() != null && cond.getAbatement() instanceof DateTimeType) {
      DateTimeType dt = (DateTimeType) cond.getAbatement();
      abatement = TestUtils.convertToString(dt.getValue(), "yyyyMMdd");
    }
    validateEffectiveDtTm(effDtTm, abatement, onset);
  }

  public static void validateEffectiveDtTm(IVLTS effDtTm, String high, String low) {

    assertNotNull(effDtTm);
    if (low != null && !low.isEmpty()) {
      TS lowTime = (TS) ((JAXBElement<? extends QTY>) effDtTm.getRest().get(0)).getValue();
      assertEquals(lowTime.getValue(), low);
    }

    if (high != null && !high.isEmpty()) {
      TS highTime = (TS) ((JAXBElement<? extends QTY>) effDtTm.getRest().get(1)).getValue();
      assertEquals(highTime.getValue(), high);
    }

    // Note: Spec doesn't say anything on nullflavour and hence not supported.

  }

  public static void validateCodeWithTranslation(CodeableConcept codes, CD code) {

    if (codes != null) {

      List<Coding> codings = codes.getCoding();

      Boolean translation = false;
      int idx = 0;

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

            CD transCode = code.getTranslation().get(idx++);
            assertEquals(c.getCode(), transCode.getCode());
            assertEquals(c.getDisplay(), transCode.getDisplayName());
            assertEquals(csd.getValue0(), transCode.getCodeSystem());
            assertEquals(csd.getValue1(), transCode.getCodeSystemName());
          }
        }
      }

      if (translation == false) {
        validateNullFlavor(code, "NI");
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

  public static void validateID(II id, String root, String extension) {

    assertNotNull(id);
    assertEquals(root, id.getRoot());
    if (extension != null) {
      assertEquals(extension, id.getExtension());
    }
  }

  public static void validateProblemSection(
      List<Condition> conditions, POCDMT000040Section problemSection) {

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

    for (Condition cond : conditions) {
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

    // validateTableBody(table, rowValues);

    List<POCDMT000040Entry> entries = problemSection.getEntry();

    for (Condition cond : conditions) {
      for (POCDMT000040Entry entry : entries) {
        validateConditionEntries(cond, entry);
      }
    }

    // TODO This needs to be uncommented once the bug is fixed
    /*int idx = 0;
    for (Condition cond : conditions) {
    	validateConditionEntries(cond, entries.get(idx++));
      }*/
  }

  public static void validateConditionEntries(Condition cond, POCDMT000040Entry entry) {

    assertEquals(entry.getTypeCode().value(), "DRIV");
    assertEquals(entry.getAct().getClassCode().value(), "ACT");
    assertEquals(entry.getAct().getMoodCode().value(), "EVN");

    // validate template id
    validateTemplateID(
        entry.getAct().getTemplateId().get(0), "2.16.840.1.113883.10.20.22.4.3", null);
    validateTemplateID(
        entry.getAct().getTemplateId().get(1), "2.16.840.1.113883.10.20.22.4.3", "2015-08-01");

    // TO-DO assertion for Identifier(Random UUID is generated in code and populated.)
    // validateIdentifier();

    validateCode(
        entry.getAct().getCode(), "CONC", "2.16.840.1.113883.5.6", "HL7ActClass", "Concern");

    // validating status code
    if (cond.getClinicalStatus() != null
        && cond.getClinicalStatus().getCodingFirstRep() != null
        && !StringUtils.isEmpty(cond.getClinicalStatus().getCodingFirstRep().getCode())
        && (cond.getClinicalStatus()
                .getCodingFirstRep()
                .getCode()
                .contentEquals(ConditionClinical.ACTIVE.toCode())
            || cond.getClinicalStatus()
                .getCodingFirstRep()
                .getCode()
                .contentEquals(ConditionClinical.RELAPSE.toCode()))) {
      assertEquals(entry.getAct().getStatusCode().getCode(), "active");
    } else {
      assertEquals(entry.getAct().getStatusCode().getCode(), "completed");
    }

    // validate effective date time
    validateConditionEffectiveDtTm(cond, entry.getAct().getEffectiveTime());

    // validate entryRelationship
    List<POCDMT000040EntryRelationship> entryRelationships = entry.getAct().getEntryRelationship();
    validateEntryRelationships(entryRelationships, cond);
  }

  public static void validateEntryRelationships(
      List<POCDMT000040EntryRelationship> entryRelationships, Condition cond) {

    for (POCDMT000040EntryRelationship entryRelationship : entryRelationships) {
      if (entryRelationship.getTypeCode().value().equals("SUBJ")) {
        validateObservation(entryRelationship.getObservation(), cond);
      } else if (entryRelationship.getTypeCode().value().equals("RSON")) {
        validateObservationWithTriggerCodes(entryRelationship.getObservation(), cond);
      }
    }
  }

  public static void validateObservation(POCDMT000040Observation observation, Condition cond) {
    assertEquals(observation.getClassCode().get(0), "OBS");
    assertEquals(observation.getMoodCode().value(), "EVN");

    validateTemplateID(observation.getTemplateId().get(0), "2.16.840.1.113883.10.20.22.4.4", null);
    validateTemplateID(
        observation.getTemplateId().get(1), "2.16.840.1.113883.10.20.22.4.4", "2015-08-01");

    // validate Identifier
    validateIdentifier(cond.getIdentifier(), observation.getId(), cond.getId());

    // validateCodeWithTranslation(codes, code);
    validateCode(
        observation.getCode(), "282291009", "2.16.840.1.113883.6.96", "SNOMED-CT", "Diagnosis");
    validateCode(
        observation.getCode().getTranslation().get(0),
        "29308-4",
        "2.16.840.1.113883.6.1",
        "LOINC",
        "Diagnosis");

    // validate statuscode
    assertEquals(observation.getStatusCode().getCode(), "completed");

    // validate effective date time
    validateConditionEffectiveDtTm(cond, observation.getEffectiveTime());

    // validate the value
    CD code = (CD) observation.getValue().get(0);
    validateCodeWithTranslation(cond.getCode(), code);
  }

  public static void validateObservationWithTriggerCodes(
      POCDMT000040Observation observation, Condition cond) {
    assertEquals(observation.getClassCode().get(0), "OBS");
    assertEquals(observation.getMoodCode().value(), "EVN");

    assertEquals(observation.isNegationInd(), false);

    validateTemplateID(observation.getTemplateId().get(0), "2.16.840.1.113883.10.20.22.4.4", null);
    validateTemplateID(
        observation.getTemplateId().get(1), "2.16.840.1.113883.10.20.22.4.4", "2015-08-01");
    validateTemplateID(
        observation.getTemplateId().get(2), "2.16.840.1.113883.10.20.15.2.3.3", "2016-12-01");

    // validateIdentifier();
    validateIdentifier(cond.getIdentifier(), observation.getId(), cond.getId());

    // validate Code
    validateCode(
        observation.getCode(), "282291009", "2.16.840.1.113883.6.96", "SNOMED-CT", "Diagnosis");
    validateCode(
        observation.getCode().getTranslation().get(0),
        "29308-4",
        "2.16.840.1.113883.6.1",
        "LOINC",
        "Diagnosis");

    // validate statuscode
    assertEquals(observation.getStatusCode().getCode(), "completed");

    // validate effective date time
    validateConditionEffectiveDtTm(cond, observation.getEffectiveTime());

    // validate the value
    validateValueCDWithValueSetAndVersion(cond, observation);
    // code.get
  }

  public static void validateValueCDWithValueSetAndVersion(
      Condition cond, POCDMT000040Observation observation) {

    PatientExecutionState state = null;
    // LaunchDetails details = new LaunchDetails();
    // details.setStatus(
    //
    // "{\"patientId\":\"12742571\",\"encounterId\":\"97953900\",\"matchTriggerStatus\":{\"actionId\":\"match-trigger\",\"jobStatus\":\"COMPLETED\",\"triggerMatchStatus\":true,\"matchedCodes\":[{\"matchedCodes\":[\"http://hl7.org/fhir/sid/icd-10-cm|U07.1\",\"http://snomed.info/sct|840539006\"],\"valueSet\":\"2.16.840.1.113762.1.4.1146.1123\",\"valueSetVersion\":\"1\",\"matchedPath\":\"Condition.code\"}]},\"createEicrStatus\":{\"actionId\":\"create-eicr\",\"jobStatus\":\"SCHEDULED\",\"eicrCreated\":false,\"eICRId\":\"\"},\"periodicUpdateStatus\":[{\"actionId\":\"periodic-update-eicr\",\"jobStatus\":\"NOT_STARTED\",\"eicrUpdated\":false,\"eICRId\":\"\"}],\"periodicUpdateJobStatus\":\"SCHEDULED\",\"closeOutEicrStatus\":{\"actionId\":\"\",\"jobStatus\":\"NOT_STARTED\",\"eicrClosed\":false,\"eICRId\":\"\"},\"validateEicrStatus\":[],\"submitEicrStatus\":[],\"rrStatus\":[],\"eicrsReadyForValidation\":[],\"eicrsForRRCheck\":[],\"eicrsReadyForSubmission\":[]}");
    state = ApplicationUtils.getDetailStatus(launchDetails);
    List<MatchedTriggerCodes> mtcs = state.getMatchTriggerStatus().getMatchedCodes();

    for (MatchedTriggerCodes mtc : mtcs) {
      if (mtc.hasMatchedTriggerCodes("Condition")) {
        Set<String> matchedCodes = mtc.getMatchedCodes();
        if (matchedCodes != null && matchedCodes.size() > 0) {
          matchedCodes
              .stream()
              .filter(Objects::nonNull)
              .findFirst()
              .ifPresent(
                  matchCode -> {
                    String[] parts = matchCode.split("\\|");

                    Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(parts[0]);

                    CD code = (CD) observation.getValue().get(0);
                    assertEquals(parts[1], code.getCode());
                    assertEquals(csd.getValue0(), code.getCodeSystem());
                    assertEquals(csd.getValue1(), code.getCodeSystemName());
                    assertEquals("2.16.840.1.114222.4.11.7508", code.getValueSet());
                    assertEquals("19/05/2016", code.getValueSetVersion());
                  });
        }
      }
    }
  }

  public static void validateCode(
      CD codeObj, String code, String codeSystem, String codeSystemName, String displayName) {
    assertEquals(codeObj.getCode(), code);
    assertEquals(codeObj.getCodeSystem(), codeSystem);
    assertEquals(codeObj.getCodeSystemName(), codeSystemName);
    assertEquals(codeObj.getDisplayName(), displayName);
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
    // resonForVisitSection.getTitle().getAny());

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
      } else if (r4Encounter.getReasonCodeFirstRep().getCodingFirstRep() != null
          && !StringUtils.isEmpty(
              r4Encounter.getReasonCodeFirstRep().getCodingFirstRep().getDisplay())) {
        assertEquals(
            rowColValue, r4Encounter.getReasonCodeFirstRep().getCodingFirstRep().getDisplay());
      } else {
        assertEquals(rowColValue, "Unknown Reason For Visit");
      }
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
    String date = TestUtils.convertToString(r4Encounter.getPeriod().getStart(), "yyyyMMddHHmmss");
    if (date == null) {
      date = "Unknown";
    }
    List<Pair<String, String>> rowValues = new ArrayList<>();

    Pair<String, String> row = new Pair<>(encounterName, date);
    rowValues.add(row);

    validateTableBody(encounterTable, rowValues);

    validateEncounterEntry(encounterSection.getEntry(), r4Encounter);
  }

  private static void validateEncounterEntry(
      List<POCDMT000040Entry> entries, Encounter r4Encounter) {

    for (POCDMT000040Entry encounterEntry : entries) {

      validateTemplateID(
          encounterEntry.getEncounter().getTemplateId().get(0),
          "2.16.840.1.113883.10.20.22.4.49",
          null);
      validateTemplateID(
          encounterEntry.getEncounter().getTemplateId().get(1),
          "2.16.840.1.113883.10.20.22.4.49",
          "2015-08-01");
      // Identifier
      validateIdentifier(
          r4Encounter.getIdentifier(), encounterEntry.getEncounter().getId(), r4Encounter.getId());

      // Code
      validateCodeWithTranslation(
          r4Encounter.getType().get(0), encounterEntry.getEncounter().getCode());

      // Effective DtTm
      String end = null;
      String start = null;
      if (r4Encounter.getPeriod() != null) {
        end = TestUtils.convertToString(r4Encounter.getPeriod().getEnd(), "yyyyMMdd");
        start = TestUtils.convertToString(r4Encounter.getPeriod().getStart(), "yyyyMMdd");
      }
      validateEffectiveDtTm(encounterEntry.getEncounter().getEffectiveTime(), end, start);
    }
  }

  public static void validatePresentIllnessSection(
      List<Condition> conditions, POCDMT000040Section illnessSection) {

    validateTemplateID(
        illnessSection.getTemplateId().get(0), "1.3.6.1.4.1.19376.1.5.3.1.3.4", null);

    validateCode(
        illnessSection.getCode(),
        "10164-2",
        "2.16.840.1.113883.6.1",
        "LOINC",
        "History of Present Illness");

    // TODO Title
    // Narrative Text of type Table
    StrucDocText docText = illnessSection.getText();
    StrucDocTable table = (StrucDocTable) ((JAXBElement<?>) docText.getContent().get(1)).getValue();

    validateTableBorderAndWidth(table, "1", "100%");

    List<String> header = new ArrayList<>();
    header.add("Narrative Text");
    validateTableHeadingTitles(table, header);

    List<StrucDocTr> trs = table.getTbody().get(0).getTr();
    assertFalse(trs.isEmpty());
    // Only one row for every condition
    // assertEquals(conditions.size(), trs.size());

    // Only one column
    assertEquals(1, trs.get(0).getThOrTd().size());

    StrucDocTd col1 = (StrucDocTd) trs.get(0).getThOrTd().get(0);

    StrucDocContent rowCol1 =
        (StrucDocContent) (((JAXBElement<?>) col1.getContent().get(1)).getValue());
    String rowColValue = (String) rowCol1.getContent().get(0);
    String text = "Unknown History of Present Illness";

    if (conditions != null && conditions.size() > 0) {
      int idx = 1;
      for (Condition prob : conditions) {
        rowCol1 = (StrucDocContent) (((JAXBElement<?>) col1.getContent().get(idx)).getValue());
        rowColValue = (String) rowCol1.getContent().get(0);
        String probDisplayName = "Unknown";
        if (prob.getCode() != null && !StringUtils.isEmpty(prob.getCode().getText())) {

          probDisplayName = prob.getCode().getText();
          assertEquals(rowColValue, probDisplayName);
        } else if (prob.getCode().getCodingFirstRep() != null
            && !StringUtils.isEmpty(prob.getCode().getCodingFirstRep().getDisplay())) {

          probDisplayName = prob.getCode().getCodingFirstRep().getDisplay();
          assertEquals(rowColValue, probDisplayName);
        }
        idx++;
      }
    } else {
      assertEquals(rowColValue, text);
    }
  }
}
