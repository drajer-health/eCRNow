package com.drajer.test.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cdafromr4.CdaFhirUtilities;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.Assertion.AssertCdaElement;
import java.util.*;
import javax.xml.bind.JAXBElement;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Address.AddressUse;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Patient.ContactComponent;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.codesystems.ConditionClinical;
import org.hl7.v3.*;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ValidationUtils {

  private static LaunchDetails launchDetails;

  private static final Logger logger = LoggerFactory.getLogger(ValidationUtils.class);

  private static final String YYYY_MM_DD = "yyyyMMdd";

  public static void setLaunchDetails(LaunchDetails launchDetails) {
    ValidationUtils.launchDetails = launchDetails;
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
      AssertCdaElement.assertNullFlavor(streetAddressLine, "NI");
      adIdx++;
    }

    // city
    AdxpCity city = (AdxpCity) ad.getContent().get(adIdx);
    assertNotNull(city);
    adIdx++;
    if (addr.getCity() != null) {
      assertEquals(addr.getCity(), city.getPartType().get(0));
    } else {
      AssertCdaElement.assertNullFlavor(city, "NI");
    }

    // state
    AdxpState state = (AdxpState) ad.getContent().get(adIdx);
    assertNotNull(state);
    adIdx++;
    if (addr.getState() != null) {
      assertEquals(addr.getState(), state.getPartType().get(0));
    } else {
      AssertCdaElement.assertNullFlavor(state, "NI");
    }

    // postal code
    AdxpPostalCode postCode = (AdxpPostalCode) ad.getContent().get(adIdx);
    assertNotNull(postCode);
    adIdx++;
    if (addr.getPostalCode() != null) {
      assertEquals(addr.getPostalCode(), postCode.getPartType().get(0));
    } else {
      AssertCdaElement.assertNullFlavor(postCode, "NI");
    }

    // country
    AdxpCountry country = (AdxpCountry) ad.getContent().get(adIdx);
    assertNotNull(country);
    if (addr.getCountry() != null) {
      assertEquals(addr.getCountry(), country.getPartType().get(0));
    } else {
      AssertCdaElement.assertNullFlavor(country, "NI");
    }
  }

  public static void validateIdentifier(
      List<Identifier> r4Identifiers, List<II> cdaIdentifiers, String entityId) {

    int idx = 0;
    if (entityId != null && !entityId.isEmpty()) {
      AssertCdaElement.assertID(
          cdaIdentifiers.get(idx++), launchDetails.getAssigningAuthorityId(), entityId);
    }

    for (Identifier id : r4Identifiers) {

      String root;
      if (id.getSystem() != null && id.getValue() != null) {
        if (id.getSystem().contains("urn:oid")) {
          root = id.getSystem().replace("urn:oid:", "");
        } else {
          root = launchDetails.getAssigningAuthorityId();
        }

        AssertCdaElement.assertID(cdaIdentifiers.get(idx++), root, id.getValue());
      }
    }
  }

  public static void validateIdentifierByType(
      List<Identifier> r4Identifiers, II cdaIdentifier, String type, String entityId) {

    boolean typeIdPresent = false;

    for (Identifier id : r4Identifiers) {

      String root;
      if (id.getType() != null) {

        List<Coding> codings = id.getType().getCoding();

        for (Coding coding : codings) {

          if (coding.getSystem() != null && coding.getCode() != null) {

            if (coding.getCode().equalsIgnoreCase(type)) {
              if (ValueSetMapping.IndentifierTypeURL.contains(coding.getSystem())) {
                if (id.getSystem() != null && id.getValue() != null) {
                  if (id.getSystem().contains("urn:oid")) {
                    root = id.getSystem().replace("urn:oid:", "");
                  } else {
                    root = launchDetails.getAssigningAuthorityId();
                  }

                  AssertCdaElement.assertID(cdaIdentifier, root, id.getValue());
                  typeIdPresent = true;
                  break;
                }
              }
            }
          }
        }
      }
    }

    if (typeIdPresent == false && entityId != null && !entityId.isEmpty()) {
      AssertCdaElement.assertID(cdaIdentifier, launchDetails.getAssigningAuthorityId(), entityId);
    }
  }

  public static void validateTelecoms(List<ContactPoint> r4Telecom, List<TEL> cdaTelecom) {

    assertNotNull(cdaTelecom);
    assertTrue(cdaTelecom.size() > 0);
    ContactPoint phone = null;
    ContactPoint email = null;
    int idx = 0;

    if (r4Telecom != null && r4Telecom.size() > 0) {

      for (ContactPoint r4Tel : r4Telecom) {

        if (r4Tel.getSystem() != null) {
          if (r4Tel.getSystem() == ContactPoint.ContactPointSystem.PHONE) {
            if (!StringUtils.isEmpty(r4Tel.getValue())) {
              // TODO - Code supports only one instance of phone.
              // If it supports multiple this needs to be changes to list.
              if (phone == null) {
                phone = r4Tel;
              }
            }
          } else if (r4Tel.getSystem() == ContactPoint.ContactPointSystem.EMAIL) {
            if (!StringUtils.isEmpty(r4Tel.getValue())) {
              if (email == null) {
                // TODO - Code supports only one instance of email.
                // If it supports multiple this needs to be changes to list.
                email = r4Tel;
              }
            }
          }
        }
      }

      // TODO - Order matters here phone and then email.
      // If code changes this needs to be changed.
      if (phone != null) {
        AssertCdaElement.assertTelecomPhone(phone, cdaTelecom.get(idx++));
      }
      if (email != null) {
        AssertCdaElement.assertTelecomPhone(phone, cdaTelecom.get(idx++));
      }

    } else {

      AssertCdaElement.assertNullFlavor(cdaTelecom.get(0), "NI");
    }
  }

  public static void validateConditionEffectiveDtTm(Condition cond, IVLTS effDtTm) {
    String onset = null;
    String abatement = null;
    if (cond.getOnset() != null && cond.getOnset() instanceof DateTimeType) {
      DateTimeType dt = (DateTimeType) cond.getOnset();
      onset = TestUtils.dateToString(dt.getValue(), "yyyyMMdd");
    }
    if (cond.getAbatement() != null && cond.getAbatement() instanceof DateTimeType) {
      DateTimeType dt = (DateTimeType) cond.getAbatement();
      abatement = TestUtils.dateToString(dt.getValue(), "yyyyMMdd");
    }
    AssertCdaElement.assertEffectiveDtTm(effDtTm, abatement, onset);
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
        AssertCdaElement.assertNullFlavor(code, "NI");
      }
    } else {
      AssertCdaElement.assertNullFlavor(code, "NI");
    }
  }

  public static void validateName(List<HumanName> r4name, PN cdaName) {}

  public static void validateNarrativeTextWithTable(StrucDocText narrativeText) {

    StrucDocTable table =
        (StrucDocTable) ((JAXBElement<?>) narrativeText.getContent().get(0)).getValue();

    assertNotNull(table);
    AssertCdaElement.assertTableBorderAndWidth(table, "1", "100%");
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
          (StrucDocContent) (((JAXBElement<?>) col1.getContent().get(0)).getValue());
      assertEquals(rowValues.get(idx).getValue0(), (String) rowCol1.getContent().get(0));

      // Column2
      StrucDocTd col2 = (StrucDocTd) tds.get(1);
      StrucDocContent rowCol2 =
          (StrucDocContent) (((JAXBElement<?>) col2.getContent().get(0)).getValue());
      assertEquals(rowValues.get(idx).getValue1(), (String) rowCol2.getContent().get(0));
    }
  }

  public static void validateProblemSection(
      List<Condition> conditions, POCDMT000040Section problemSection) {

    // TemplateID
    // Only one templateID should be present as per spec.
    AssertCdaElement.assertTemplateID(
        problemSection.getTemplateId().get(0), "2.16.840.1.113883.10.20.22.2.5.1", null);
    AssertCdaElement.assertTemplateID(
        problemSection.getTemplateId().get(1), "2.16.840.1.113883.10.20.22.2.5.1", "2015-08-01");

    // Code
    AssertCdaElement.assertCodeCE(
        problemSection.getCode(), "11450-4", "2.16.840.1.113883.6.1", "LOINC", "PROBLEM LIST");

    // Title - To Do
    // assertEquals(Arrays.asList("PROBLEMS - DIAGNOSES"),
    // problemSection.getTitle().getAny());

    // Narrative Text of type Table
    StrucDocText docText = problemSection.getText();
    StrucDocTable table = (StrucDocTable) ((JAXBElement<?>) docText.getContent().get(0)).getValue();

    AssertCdaElement.assertTableBorderAndWidth(table, "1", "100%");

    List<String> header = new ArrayList<>();
    header.add("Problem or Diagnosis");
    header.add("Problem Status");
    AssertCdaElement.assertTableHeader(table, header);

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
    /*
     * int idx = 0; for (Condition cond : conditions) {
     * validateConditionEntries(cond, entries.get(idx++)); }
     */
  }

  public static void validateConditionEntries(Condition cond, POCDMT000040Entry entry) {

    assertEquals("DRIV", entry.getTypeCode().value());
    assertEquals("ACT", entry.getAct().getClassCode().value());
    assertEquals("EVN", entry.getAct().getMoodCode().value());

    // validate template id
    AssertCdaElement.assertTemplateID(
        entry.getAct().getTemplateId().get(0), "2.16.840.1.113883.10.20.22.4.3", null);
    AssertCdaElement.assertTemplateID(
        entry.getAct().getTemplateId().get(1), "2.16.840.1.113883.10.20.22.4.3", "2015-08-01");

    // TO-DO assertion for Identifier(Random UUID is generated in code and
    // populated.)
    // validateIdentifier();

    AssertCdaElement.assertCodeCD(
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
      assertEquals("active", entry.getAct().getStatusCode().getCode());
    } else {
      assertEquals("completed", entry.getAct().getStatusCode().getCode());
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
    assertEquals("OBS", observation.getClassCode().get(0));
    assertEquals("EVN", observation.getMoodCode().value());

    AssertCdaElement.assertTemplateID(
        observation.getTemplateId().get(0), "2.16.840.1.113883.10.20.22.4.4", null);
    AssertCdaElement.assertTemplateID(
        observation.getTemplateId().get(1), "2.16.840.1.113883.10.20.22.4.4", "2015-08-01");

    // validate Identifier
    validateIdentifier(cond.getIdentifier(), observation.getId(), cond.getId());

    // validateCodeWithTranslation(codes, code);
    AssertCdaElement.assertCodeCD(
        observation.getCode(), "282291009", "2.16.840.1.113883.6.96", "SNOMED-CT", "Diagnosis");
    AssertCdaElement.assertCodeCD(
        observation.getCode().getTranslation().get(0),
        "29308-4",
        "2.16.840.1.113883.6.1",
        "LOINC",
        "Diagnosis");

    // validate statuscode
    assertEquals("completed", observation.getStatusCode().getCode());

    // validate effective date time
    validateConditionEffectiveDtTm(cond, observation.getEffectiveTime());

    // validate the value
    CD code = (CD) observation.getValue().get(0);
    validateCodeWithTranslation(cond.getCode(), code);
  }

  public static void validateObservationWithTriggerCodes(
      POCDMT000040Observation observation, Condition cond) {
    assertEquals("OBS", observation.getClassCode().get(0));
    assertEquals("EVN", observation.getMoodCode().value());

    assertEquals(false, observation.isNegationInd());

    AssertCdaElement.assertTemplateID(
        observation.getTemplateId().get(0), "2.16.840.1.113883.10.20.22.4.4", null);
    AssertCdaElement.assertTemplateID(
        observation.getTemplateId().get(1), "2.16.840.1.113883.10.20.22.4.4", "2015-08-01");
    AssertCdaElement.assertTemplateID(
        observation.getTemplateId().get(2), "2.16.840.1.113883.10.20.15.2.3.3", "2016-12-01");

    // validateIdentifier();
    validateIdentifier(cond.getIdentifier(), observation.getId(), cond.getId());

    // validate Code
    AssertCdaElement.assertCodeCD(
        observation.getCode(), "282291009", "2.16.840.1.113883.6.96", "SNOMED-CT", "Diagnosis");
    AssertCdaElement.assertCodeCD(
        observation.getCode().getTranslation().get(0),
        "29308-4",
        "2.16.840.1.113883.6.1",
        "LOINC",
        "Diagnosis");

    // validate statuscode
    assertEquals("completed", observation.getStatusCode().getCode());

    // validate effective date time
    validateConditionEffectiveDtTm(cond, observation.getEffectiveTime());

    // validate the value
    validateValueCDWithValueSetAndVersion(cond, observation);
    // code.get
  }

  public static void validateValueCDWithValueSetAndVersion(
      Condition cond, POCDMT000040Observation observation) {

    // TODO - Currently relying on MatchedCodes generated from code.
    // Need to find better way to extract from r4 resources.
    PatientExecutionState state = null;
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

  public static void validateReasonForVisitSection(
      Encounter r4Encounter, POCDMT000040Section resonForVisitSection) {

    // TemplateID
    AssertCdaElement.assertTemplateID(
        resonForVisitSection.getTemplateId().get(0), "2.16.840.1.113883.10.20.22.2.12", null);
    // Code
    AssertCdaElement.assertCodeCE(
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
    StrucDocTable table = (StrucDocTable) ((JAXBElement<?>) docText.getContent().get(0)).getValue();

    AssertCdaElement.assertTableBorderAndWidth(table, "1", "100%");

    List<String> header = new ArrayList<>();
    header.add("text");
    AssertCdaElement.assertTableHeader(table, header);

    List<StrucDocTr> trs = table.getTbody().get(0).getTr();
    assertFalse(trs.isEmpty());
    // Only one row
    assertEquals(1, trs.size());
    // Only one column
    assertEquals(1, trs.get(0).getThOrTd().size());

    StrucDocTd col1 = (StrucDocTd) trs.get(0).getThOrTd().get(0);
    StrucDocContent rowCol1 =
        (StrucDocContent) (((JAXBElement<?>) col1.getContent().get(0)).getValue());
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
        assertEquals("Unknown Reason For Visit", rowColValue);
      }
    } else {
      assertEquals("Unknown Reason For Visit", rowColValue);
    }
  }

  public static void validateEncounterSection(
      Encounter r4Encounter, POCDMT000040Section encounterSection) {

    AssertCdaElement.assertTemplateID(
        encounterSection.getTemplateId().get(0), "2.16.840.1.113883.10.20.22.2.22.1", null);

    AssertCdaElement.assertTemplateID(
        encounterSection.getTemplateId().get(1), "2.16.840.1.113883.10.20.22.2.22.1", "2015-08-01");

    AssertCdaElement.assertCodeCE(
        encounterSection.getCode(),
        "46240-8",
        "2.16.840.1.113883.6.1",
        "LOINC",
        "History of Encounters");

    // TODO Title

    StrucDocText encounterText = encounterSection.getText();
    StrucDocTable encounterTable =
        (StrucDocTable) ((JAXBElement<?>) (encounterText.getContent().get(0))).getValue();

    AssertCdaElement.assertTableBorderAndWidth(encounterTable, "1", "100%");
    AssertCdaElement.assertTableHeader(
        encounterTable,
        new ArrayList<String>(Arrays.asList("Encounter Reason", "Date of Encounter")));

    String encounterName = r4Encounter.getTypeFirstRep().getCodingFirstRep().getDisplay();
    String date = TestUtils.dateToString(r4Encounter.getPeriod().getStart(), "yyyyMMddHHmmss");
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

      AssertCdaElement.assertTemplateID(
          encounterEntry.getEncounter().getTemplateId().get(0),
          "2.16.840.1.113883.10.20.22.4.49",
          null);
      AssertCdaElement.assertTemplateID(
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
        end = TestUtils.dateToString(r4Encounter.getPeriod().getEnd(), "yyyyMMdd");
        start = TestUtils.dateToString(r4Encounter.getPeriod().getStart(), "yyyyMMdd");
      }
      AssertCdaElement.assertEffectiveDtTm(
          encounterEntry.getEncounter().getEffectiveTime(), end, start);
    }
  }

  public static void validatePresentIllnessSection(
      List<Condition> conditions, POCDMT000040Section illnessSection) {

    AssertCdaElement.assertTemplateID(
        illnessSection.getTemplateId().get(0), "1.3.6.1.4.1.19376.1.5.3.1.3.4", null);

    AssertCdaElement.assertCodeCE(
        illnessSection.getCode(),
        "10164-2",
        "2.16.840.1.113883.6.1",
        "LOINC",
        "History of Present Illness");

    // TODO Title
    // Narrative Text of type Table
    StrucDocText docText = illnessSection.getText();
    StrucDocTable table = (StrucDocTable) ((JAXBElement<?>) docText.getContent().get(0)).getValue();

    AssertCdaElement.assertTableBorderAndWidth(table, "1", "100%");

    List<String> header = new ArrayList<>();
    header.add("Narrative Text");
    AssertCdaElement.assertTableHeader(table, header);

    List<StrucDocTr> trs = table.getTbody().get(0).getTr();
    List<StrucDocTr> unique = new ArrayList<>();

    // Populating a list with all the unique rows. Can be removed once the bug is
    // fixed
    for (StrucDocTr tableRow : trs) {
      StrucDocTd col1 = (StrucDocTd) tableRow.getThOrTd().get(0);

      StrucDocContent rowCol1 =
          (StrucDocContent) (((JAXBElement<?>) col1.getContent().get(0)).getValue());
      String rowColValue = (String) rowCol1.getContent().get(0);

      if (unique.isEmpty()) {
        unique.add(tableRow);
      } else {
        Iterator<StrucDocTr> itr = unique.iterator();
        while (itr.hasNext()) {
          StrucDocTr un = (StrucDocTr) itr.next();
          StrucDocTd column = (StrucDocTd) un.getThOrTd().get(0);
          StrucDocContent rowColumn =
              (StrucDocContent) (((JAXBElement<?>) column.getContent().get(0)).getValue());
          String rowColumnValue = (String) rowColumn.getContent().get(0);

          if (rowColValue.equals(rowColumnValue)) {
            break;
          }
          if (!itr.hasNext()) {
            unique.add(tableRow);
            break;
          }
        }
      }
    }

    // Populating the list with all the row values. Once the bug is fixed replace
    // "unique" with
    // "trs"(List<StrucDocTr>)
    List<String> rowValues = new ArrayList<>();
    for (StrucDocTr uniqueTableRow : unique) {
      StrucDocTd col1 = (StrucDocTd) uniqueTableRow.getThOrTd().get(0);
      StrucDocContent rowCol1 =
          (StrucDocContent) (((JAXBElement<?>) col1.getContent().get(0)).getValue());
      String rowColValue = (String) rowCol1.getContent().get(0);
      rowValues.add(rowColValue);
    }

    // Validation
    if (conditions != null && conditions.size() > 0) {
      for (Condition prob : conditions) {
        if (prob.getCode() != null && !StringUtils.isEmpty(prob.getCode().getText())) {
          assertTrue(rowValues.contains(prob.getCode().getText()));
        } else if (prob.getCode().getCodingFirstRep() != null
            && !StringUtils.isEmpty(prob.getCode().getCodingFirstRep().getDisplay())) {
          assertTrue(rowValues.contains(prob.getCode().getCodingFirstRep().getDisplay()));
        } else {
          assertTrue(rowValues.contains("Unknown"));
        }
      }
    } else {
      assertTrue(rowValues.contains("Unknown History of Present Illness"));
    }
  }

  public static void validateSocialHistory(
      List<Extension> listExtensions, POCDMT000040Section socialHistorySection) {
    AssertCdaElement.assertTemplateID(
        socialHistorySection.getTemplateId().get(0), "2.16.840.1.113883.10.20.22.2.17", null);
    AssertCdaElement.assertTemplateID(
        socialHistorySection.getTemplateId().get(1),
        "2.16.840.1.113883.10.20.22.2.17",
        "2015-08-01");
    AssertCdaElement.assertCodeCE(
        socialHistorySection.getCode(),
        "29762-2",
        "2.16.840.1.113883.6.1",
        "LOINC",
        "Social History");
    StrucDocText docText = socialHistorySection.getText();
    StrucDocTable table = (StrucDocTable) ((JAXBElement<?>) docText.getContent().get(0)).getValue();
    AssertCdaElement.assertTableBorderAndWidth(table, "1", "100%");

    AssertCdaElement.assertTableHeader(
        table,
        new ArrayList<String>(
            Arrays.asList("Social History Observation", "Social History Observation Result")));
    for (Extension extension : listExtensions) {
      if (extension.getUrl() != null
          && extension
              .getUrl()
              .contentEquals("http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex")) {

        List<Pair<String, String>> rowValues = new ArrayList<>();

        String obsRes = ((CodeType) extension.getValue()).getValue();
        String socCont = "Birth Sex";

        Pair<String, String> row = new Pair<>(obsRes, socCont);
        rowValues.add(row);

        validateTableBody(table, rowValues);
      }
    }

    validateSocialHistoryEntry(socialHistorySection.getEntry(), listExtensions);
  }

  private static void validateSocialHistoryEntry(
      List<POCDMT000040Entry> entry, List<Extension> listExtensions) {
    AssertCdaElement.assertTemplateID(
        entry.get(0).getObservation().getTemplateId().get(0),
        "2.16.840.1.113883.10.20.22.4.200",
        "2016-06-01");
    // Id validation not done as it is a system generated value
    AssertCdaElement.assertCodeCD(
        entry.get(0).getObservation().getCode(),
        "76689-9",
        "2.16.840.1.113883.6.1",
        "LOINC",
        "Birth Sex");
    for (Extension extension : listExtensions) {
      if (extension.getUrl() != null
          && extension
              .getUrl()
              .contentEquals("http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex")) {
        String code = ((CodeType) extension.getValue()).getValue();
        AssertCdaElement.assertCodeCS(
            entry.get(0).getObservation().getStatusCode(), "completed", null, null, null);
        AssertCdaElement.assertCodeCD(
            (CD) entry.get(0).getObservation().getValue().get(0),
            code,
            "2.16.840.1.113883.5.1",
            "Administrative Gender",
            null);
      }
    }
  }

  public static void validateHeader(
      Patient r4Patient,
      Encounter r4Encounter,
      List<Practitioner> r4Practitioner,
      Organization r4Organization,
      POCDMT000040ClinicalDocument eICR) {

    POCDMT000040PatientRole patientRole = eICR.getRecordTarget().get(0).getPatientRole();
    validatePatientRole(r4Patient, patientRole);

    Practitioner practitioner = r4Practitioner.get(0);
    POCDMT000040Author pocdmt000040Author = eICR.getAuthor().get(0);

    validateAuthor(r4Encounter, practitioner, pocdmt000040Author);

    // validate custodian
    POCDMT000040Custodian pocdmt000040Custodian = eICR.getCustodian();
    validateCustodian(r4Organization, pocdmt000040Custodian);

    // validate componentOf
    POCDMT000040EncompassingEncounter encompassingEncounter =
        eICR.getComponentOf().getEncompassingEncounter();
    validateEncompassingEncounter(r4Encounter, encompassingEncounter, practitioner, r4Organization);
  }

  public static void validatePatientRole(Patient r4Patient, POCDMT000040PatientRole patientRole) {

    // Identifier
    validateIdentifierByType(
        r4Patient.getIdentifier(), patientRole.getId().get(0), "MR", r4Patient.getId());

    // TODO- Address
    // validateAddress(r4Patient.getAddress(), patientRole.getAddr());

    // Telecom
    validateTelecoms(r4Patient.getTelecom(), patientRole.getTelecom());

    // Patient
    POCDMT000040Patient cdaPatient = patientRole.getPatient();
    List<JAXBElement<?>> cdaPateintcontent = cdaPatient.getContent();
    int cntIdx = 0;

    // TODO - Patient.Name
    int nameIdx = cntIdx++;
    // validateName(r4Patient.getName(), cdaPateintcontent.get(nameIdx));

    // administrativeGenderCode
    int genderIdx = cntIdx++;
    String r4GenderCode = ValueSetMapping.gender.get(r4Patient.getGender().name());
    CE cdaGenderCode = (CE) cdaPateintcontent.get(genderIdx).getValue();
    if (r4Patient.getGender() != null) {
      if (r4GenderCode != null) {
        AssertCdaElement.assertCodeCE(
            cdaGenderCode, r4GenderCode, "2.16.840.1.113883.5.1", null, null);
      } else {
        AssertCdaElement.assertCodeCE(
            cdaGenderCode,
            ValueSetMapping.gender.get("UNKNOWN"),
            "2.16.840.1.113883.5.1",
            null,
            null);
      }
    } else {
      AssertCdaElement.assertNullFlavor(cdaGenderCode, "NI");
    }

    // birthTime
    int birthIdx = cntIdx++;
    String r4BirthDate = TestUtils.dateToString(r4Patient.getBirthDate(), "yyyyMMdd");
    TS cdaBirthDate = (TS) cdaPateintcontent.get(birthIdx).getValue();
    assertEquals(r4BirthDate, cdaBirthDate.getValue());

    // sdtc:deceasedInd
    int deceasedIndIdx = cntIdx++;
    BL cdaDeceasedInd = (BL) cdaPateintcontent.get(deceasedIndIdx).getValue();

    if (r4Patient.getDeceased() == null || r4Patient.getDeceased().isEmpty()) {
      assertEquals(false, cdaDeceasedInd.isValue().booleanValue());
    } else {
      // sdtc:deceasedTime
      int deceasedDateIdx = cntIdx++;
      TS cdaDeceasedDate = (TS) cdaPateintcontent.get(deceasedDateIdx).getValue();

      if (r4Patient.getDeceased() instanceof DateTimeType) {
        assertEquals(true, cdaDeceasedInd.isValue().booleanValue());
        DateTimeType r4DeceasedDate = (DateTimeType) r4Patient.getDeceased();

        assertEquals(
            TestUtils.dateToString(r4DeceasedDate.getValue(), "yyyyMMdd"),
            cdaDeceasedDate.getValue());
      } else {
        assertEquals(r4Patient.getDeceased(), cdaDeceasedInd.isValue().booleanValue());
        AssertCdaElement.assertNullFlavor(cdaDeceasedDate, "NI");
      }
    }

    // Race
    int raceIdx = cntIdx++;
    CE cdaRace = (CE) cdaPateintcontent.get(raceIdx).getValue();
    Coding r4Race =
        CdaFhirUtilities.getCodingExtension(
            r4Patient.getExtension(),
            CdaGeneratorConstants.FHIR_USCORE_RACE_EXT_URL,
            CdaGeneratorConstants.OMB_RACE_CATEGORY_URL);
    if (r4Race != null) {
      AssertCdaElement.assertCodeCE(
          cdaRace,
          r4Race.getCode(),
          "2.16.840.1.113883.6.238",
          "Race & Ethnicity - CDC",
          r4Race.getDisplay());
    } else {
      AssertCdaElement.assertNullFlavor(cdaRace, "NI");
    }

    // Ethnicity
    int ethnicIdx = cntIdx++;
    CE cdaEthnic = (CE) cdaPateintcontent.get(ethnicIdx).getValue();
    Coding r4Ethnic =
        CdaFhirUtilities.getCodingExtension(
            r4Patient.getExtension(),
            CdaGeneratorConstants.FHIR_USCORE_ETHNICITY_EXT_URL,
            CdaGeneratorConstants.OMB_RACE_CATEGORY_URL);
    if (r4Ethnic != null) {
      AssertCdaElement.assertCodeCE(
          cdaEthnic,
          r4Ethnic.getCode(),
          "2.16.840.1.113883.6.238",
          "Race & Ethnicity - CDC",
          r4Ethnic.getDisplay());
    } else {
      AssertCdaElement.assertNullFlavor(cdaEthnic, "NI");
    }
    // Guardian
    int guardianIdx = cntIdx++;
    POCDMT000040Guardian cdaGuardian =
        (POCDMT000040Guardian) cdaPateintcontent.get(guardianIdx).getValue();

    if (r4Patient.getContact() != null) {
      ContactComponent r4Guardian = CdaFhirUtilities.getGuardianContact(r4Patient.getContact());
      validateGuardian(r4Guardian, cdaGuardian);
    }

    // Language
    int languageIdx = cntIdx++;
    POCDMT000040LanguageCommunication cdaCommunication =
        (POCDMT000040LanguageCommunication) cdaPateintcontent.get(languageIdx).getValue();
    CS languageCode = cdaCommunication.getLanguageCode();

    assertNotNull(languageCode);
    if (r4Patient.getCommunication() != null) {
      Coding r4Coding = r4Patient.getCommunication().get(0).getLanguage().getCodingFirstRep();
      AssertCdaElement.assertCodeCS(languageCode, r4Coding.getCode(), null, null, null);
    } else {
      AssertCdaElement.assertNullFlavor(languageCode, "NI");
    }
  }

  public static void validateGuardian(
      ContactComponent r4Guardian, POCDMT000040Guardian cdaGuardian) {

    // Telecom TODO - uncomment this once telecom defect is fixed.
    // validateTelecoms(r4Guardian.getTelecom(), cdaGuardian.getTelecom());

    // Address TODO - Uncomment once parser issue is fixed.
    // validateAddress(r4Guardian.getAddress(), cdaGuardian.getAddr());

  }

  public static void validateAuthor(
      Encounter encounter, Practitioner practitioner, POCDMT000040Author cdaAuthor) {

    // time
    String cdaTime = cdaAuthor.getTime().getValue();
    assertFalse(cdaTime.isEmpty());
    Period period = encounter.getPeriod();
    Date start = period.getStart();
    if (start != null) {
      String startDate = TestUtils.dateToString(start, YYYY_MM_DD);
      assertEquals(startDate, cdaTime);
    }

    POCDMT000040AssignedAuthor assignedAuthor = cdaAuthor.getAssignedAuthor();

    // TODO - (Issue - https://github.com/drajer-health/eCRNow/issues/53)
    // Identifier
    Identifier r4Identifier = FhirR4Utils.getIdentiferByType(practitioner.getIdentifier(), "NPI");
    if (r4Identifier != null) {
      AssertCdaElement.assertID(
          assignedAuthor.getId().get(0), "2.16.840.1.113883.4.6", r4Identifier.getValue());
    } else {
      AssertCdaElement.assertID(assignedAuthor.getId().get(0), "2.16.840.1.113883.4.6", null);
    }

    // TODO - Address
    // validateAddress(practitioner.getAddress(), assignedAuthor.getAddr());

    // Telecom - phone
    ContactPoint r4Telecom =
        FhirR4Utils.getTelecomByType(
            practitioner.getTelecom(), ContactPoint.ContactPointSystem.PHONE);
    if (r4Telecom != null) {
      AssertCdaElement.assertTelecomPhone(
          assignedAuthor.getTelecom().get(0), r4Telecom.getValue(), r4Telecom.getUse().toCode());
    } else {
      AssertCdaElement.assertNullFlavor(assignedAuthor.getTelecom().get(0), "NA");
    }

    POCDMT000040Person assignedPerson = assignedAuthor.getAssignedPerson();
    // TODO - Name
    // validateName(practitioner.getName(), assignedPerson.getName());

  }

  public static void validateCustodian(
      Organization r4Organization, POCDMT000040Custodian cdaCustodian) {

    assertNotNull(cdaCustodian);
    assertTrue(cdaCustodian.getNullFlavor().isEmpty());

    POCDMT000040AssignedCustodian assignedCustodian = cdaCustodian.getAssignedCustodian();
    assertNotNull(assignedCustodian);
    assertTrue(assignedCustodian.getNullFlavor().isEmpty());

    POCDMT000040CustodianOrganization cdaOrganization =
        assignedCustodian.getRepresentedCustodianOrganization();
    assertNotNull(cdaOrganization);
    assertTrue(cdaOrganization.getNullFlavor().isEmpty());

    // Identifier
    if (r4Organization.getIdentifierFirstRep() != null) {
      AssertCdaElement.assertID(
          cdaOrganization.getId().get(0),
          launchDetails.getAssigningAuthorityId(),
          r4Organization.getIdentifierFirstRep().getValue());
    } else {
      AssertCdaElement.assertID(
          cdaOrganization.getId().get(0),
          launchDetails.getAssigningAuthorityId(),
          r4Organization.getId());
    }

    // TODO - Name

    // Telecom
    ContactPoint r4Telecom =
        FhirR4Utils.getTelecomByType(
            r4Organization.getTelecom(), ContactPoint.ContactPointSystem.PHONE);
    if (r4Telecom != null) {
      AssertCdaElement.assertTelecomPhone(
          cdaOrganization.getTelecom(), r4Telecom.getValue(), r4Telecom.getUse().toCode());
    } else {
      AssertCdaElement.assertNullFlavor(cdaOrganization.getTelecom(), "NA");
    }

    // TODO - Address
    // validateAddress(r4Organization.getAddressFirstRep(), cdaOrganization.getAddr());

  }

  public static void validateEncompassingEncounter(
      Encounter r4Encounter,
      POCDMT000040EncompassingEncounter cdaEncounter,
      Practitioner r4Practitioner,
      Organization r4Organization) {

    // Identifier
    validateIdentifier(r4Encounter.getIdentifier(), cdaEncounter.getId(), r4Encounter.getId());

    // TODO - Code
    // Bug - https://github.com/drajer-health/eCRNow/issues/77
    // Coding encounterCode = r4Encounter.getClass_();
    // AssertCdaElement.assertCodeCE(cdaEncounter.getCode(), encounterCode.getCode(),
    // "2.16.840.1.113883.1.11.13955", "v3-ActEncounterCode", encounterCode.getDisplay());

    // TODO - Effective Time
    // Check on nullflavor for high date
    IVLTS cdaEffectiveTime = cdaEncounter.getEffectiveTime();
    assertNotNull(cdaEffectiveTime);
    assertTrue(cdaEffectiveTime.getNullFlavor().isEmpty());

    String lowDate = TestUtils.dateToString(r4Encounter.getPeriod().getStart(), YYYY_MM_DD);
    String highDate = TestUtils.dateToString(r4Encounter.getPeriod().getEnd(), YYYY_MM_DD);
    AssertCdaElement.assertEffectiveDtTm(cdaEffectiveTime, highDate, lowDate);

    // ResponsibleParty.AssignedEntity
    POCDMT000040AssignedEntity assignedEntity =
        cdaEncounter.getResponsibleParty().getAssignedEntity();
    validateAssignedEntity(r4Practitioner, r4Organization, assignedEntity);

    // TODO
    // validateLocation(r4Location, r4Organization, cdaEncounter.getLocation());
  }

  public static void validateAssignedEntity(
      Practitioner practitioner,
      Organization r4Organization,
      POCDMT000040AssignedEntity cdaAssignedEntity) {

    // TODO - Identifier (Issue - https://github.com/drajer-health/eCRNow/issues/53)
    /*Identifier r4Identifier = FhirR4Utils.getIdentiferByType(practitioner.getIdentifier(), "NPI");
    if(r4Identifier != null) {
    	AssertCdaElement.assertID(cdaAssignedEntity.getId().get(0), "2.16.840.1.113883.4.6", r4Identifier.getValue());
    } else {
    	AssertCdaElement.assertID(cdaAssignedEntity.getId().get(0), "2.16.840.1.113883.4.6", null);
    }*/

    // TODO - Address
    // validateAddress(practitioner.getAddress(), assignedAuthor.getAddr());

    // Telecom - phone
    ContactPoint r4Telecom =
        FhirR4Utils.getTelecomByType(
            practitioner.getTelecom(), ContactPoint.ContactPointSystem.PHONE);
    if (r4Telecom != null) {
      AssertCdaElement.assertTelecomPhone(
          cdaAssignedEntity.getTelecom().get(0), r4Telecom.getValue(), r4Telecom.getUse().toCode());
    } else {
      AssertCdaElement.assertNullFlavor(cdaAssignedEntity.getTelecom().get(0), "NA");
    }

    POCDMT000040Person assignedPerson = cdaAssignedEntity.getAssignedPerson();
    // TODO - Name
    // validateName(practitioner.getName(), assignedPerson.getName());

    // RepresentedOrganization
    POCDMT000040Organization cdaOrganization = cdaAssignedEntity.getRepresentedOrganization();
    // TODO - Name

    // TODO - Address
    // validateAddress(r4Organization.getAddressFirstRep(), cdaOrganization.getAddr());

  }

  public static void validateLocation(
      Location r4Location, Organization r4Organization, POCDMT000040Location cdaLocation) {

    // TODO - healthCareFacility
    // healthCareFacility.id
    // healthCareFacility.code
    // healthCareFacility.location.address

    // ServiceProviderOrganization
    POCDMT000040Organization cdaOrganization =
        cdaLocation.getHealthCareFacility().getServiceProviderOrganization();
    // TODO - Name

    // Telecom
    ContactPoint r4Telecom =
        FhirR4Utils.getTelecomByType(
            r4Organization.getTelecom(), ContactPoint.ContactPointSystem.PHONE);
    if (r4Telecom != null) {
      AssertCdaElement.assertTelecomPhone(
          cdaOrganization.getTelecom().get(0), r4Telecom.getValue(), r4Telecom.getUse().toCode());
    } else {
      AssertCdaElement.assertNullFlavor(cdaOrganization.getTelecom().get(0), "NA");
    }

    // TODO - Address
    // validateAddress(r4Organization.getAddressFirstRep(), cdaOrganization.getAddr());
  }
}
