package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.ActionRepo;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
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
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.codesystems.V3ParticipationType;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaHeaderGenerator {

  private CdaHeaderGenerator() {}

  private static final Properties properties = new Properties();
  private static final Logger logger = LoggerFactory.getLogger(CdaHeaderGenerator.class);

  private static String SW_APP_VERSION = "Version 3.1.X";
  private static String SW_APP_NAME = "ecrNowApp";
  private static final String SPRING_ACTIVE_PROFILE = "spring.active.profile";
  private static final String DEFAULT_PROPERTIES_FILE = "application.properties";

  private static String activeProfile;

  // Map to hold Application Properties
  private static HashMap<String, String> appProps = new HashMap<>();

  static {
    loadProperties();
  }

  public static void loadProperties() {
    String propertiesFileName = getPropertiesFileName();

    try (InputStream input =
        CdaHeaderGenerator.class.getClassLoader().getResourceAsStream(propertiesFileName)) {
      if (input != null) {
        Properties properties = new Properties();
        properties.load(input);
        appProps.putAll((Map) properties);

      } else {
        logger.error("Properties file {} not found in classpath!", propertiesFileName);
      }
    } catch (IOException e) {
      logger.error("Error loading properties file :{} ", e);
    }
  }

  private static String getPropertiesFileName() {
    String activeProfile = System.getProperty(SPRING_ACTIVE_PROFILE);
    return (activeProfile != null && !activeProfile.isEmpty())
        ? "application-" + activeProfile + ".properties"
        : DEFAULT_PROPERTIES_FILE;
  }

  public static String createCdaHeader(R4FhirData data, LaunchDetails details, Eicr ecr) {

    StringBuilder eICRHeader = new StringBuilder();

    if (data != null) {

      eICRHeader.append(CdaGeneratorUtils.getXmlHeaderForClinicalDocument());

      // Set the clinical document id.
      String docId = CdaGeneratorUtils.getGuid();
      eICRHeader.append(CdaGeneratorUtils.getXmlForII(docId));
      ecr.setEicrDocId(docId);
      ecr.setxCorrelationId(docId);

      // Set the other eICR details.
      ecr.setFhirServerUrl(details.getEhrServerURL());
      ecr.setLaunchPatientId(details.getLaunchPatientId());
      ecr.setEncounterId(details.getEncounterId());
      ecr.setSetId(details.getSetId());
      ecr.setDocVersion(details.getVersionNumber());
      ecr.setxRequestId(details.getxRequestId());

      eICRHeader.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.PH_DOC_CODE,
              CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
              CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
              CdaGeneratorConstants.PH_DOC_DISPLAY_NAME));

      eICRHeader.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.PH_DOC_DISPLAY_NAME));

      eICRHeader.append(
          CdaGeneratorUtils.getXmlForEffectiveTime(
              CdaGeneratorConstants.EFF_TIME_EL_NAME, CdaGeneratorUtils.getCurrentDateTime()));

      eICRHeader.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CONFIDENTIALITY_EL_NAME,
              CdaGeneratorConstants.CONFIDENTIALITY_CODE,
              CdaGeneratorConstants.CONFIDENTIALITY_CODE_SYTEM));

      eICRHeader.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.LANGUAGE_CODE_EL_NAME, CdaGeneratorConstants.LANGUAGE_CODE));

      eICRHeader.append(
          CdaGeneratorUtils.getXmlForIIWithElName(
              CdaGeneratorConstants.SET_ID_EL_NAME,
              details.getAssigningAuthorityId(),
              String.valueOf(details.getSetId())));

      Integer vernum = ActionRepo.getInstance().getEicrRRService().getMaxVersionId(ecr);

      if (vernum == 0) {
        eICRHeader.append(
            CdaGeneratorUtils.getXmlForValue(
                CdaGeneratorConstants.VERSION_EL_NAME, Integer.toString(ecr.getDocVersion())));
      } else {
        // Setup version number for ecr.
        ecr.setDocVersion(vernum + 1);
        eICRHeader.append(
            CdaGeneratorUtils.getXmlForValue(
                CdaGeneratorConstants.VERSION_EL_NAME, Integer.toString(ecr.getDocVersion())));
      }

      Bundle bundle = data.getData();
      if (bundle != null) {

        List<BundleEntryComponent> entries = bundle.getEntry();

        for (BundleEntryComponent ent : entries) {

          // Populate Patient
          if (ent.getResource() instanceof Patient) {

            Patient p = (Patient) ent.getResource();
            eICRHeader.append(getPatientDetails(p, details));

            break;
          }
        }

        HashMap<V3ParticipationType, List<Practitioner>> prs = getSortedPractitionerList(data);

        eICRHeader.append(getAuthorXml(data, data.getEncounter(), prs));

        // Add EHR information if available
        if (appProps != null
            && appProps.containsKey("ehr.product.name")
            && appProps.containsKey("ehr.product.version")) {
          eICRHeader.append(
              getAdditionalAuthorXml(
                  appProps.get("ehr.product.name"), appProps.get("ehr.product.version")));
        }

        // Add software version always
        eICRHeader.append(getAdditionalAuthorXml(SW_APP_NAME, SW_APP_VERSION));

        // Add System Integrator / Implementer information if available
        if (appProps != null
            && appProps.containsKey("ecrnow.implementer.name")
            && appProps.containsKey("ecrnow.implementer.version")) {
          eICRHeader.append(
              getAdditionalAuthorXml(
                  appProps.get("ecrnow.implementer.name"),
                  appProps.get("ecrnow.implementer.version")));
        }

        eICRHeader.append(getCustodianXml(details, data));

        eICRHeader.append(getParticipantXml(details, data, data.getPatient()));

        eICRHeader.append(getEncompassingEncounter(data.getEncounter(), prs, details, data));
      } else {
        String msg = "No Fhir Data Bundle retrieved to CREATE EICR.";
        logger.error(msg);

        throw new RuntimeException(msg);
      }
    } else {
      String msg = "No existing Fhir Data for Creating EICR.";
      logger.error(msg);

      throw new RuntimeException(msg);
    }

    return eICRHeader.toString();
  }

  private static String getParticipantXml(LaunchDetails details, R4FhirData data, Patient patient) {

    logger.info("LaunchDetails :{} R4FhirData:{}", details, data);

    StringBuilder s = new StringBuilder("");
    if (patient != null && patient.getContact() != null) {

      List<ContactComponent> ccs = patient.getContact();

      for (ContactComponent cc : ccs) {

        if (cc.getRelationship() != null) {

          Coding c = getTranslatableCodeableConceptCoding(cc.getRelationship());

          if (c != null) {
            s.append(getParticipantXml(cc, c));
          }
        }
      }
    }

    return s.toString();
  }

  public static String getParticipantXml(ContactComponent cc, Coding c) {

    StringBuilder s = new StringBuilder(200);

    s.append(
        CdaGeneratorUtils.getXmlForStartElementWithTypeCode(
            CdaGeneratorConstants.PARTICIPANT_EL_NAME, CdaGeneratorConstants.TYPE_CODE_IND));

    String relationship = CdaGeneratorConstants.getCodeForContactRelationship(c.getCode());

    s.append(
        CdaGeneratorUtils.getXmlForStartElementWithClassCode(
            CdaGeneratorConstants.ASSOCIATED_ENTITY_EL_NAME, relationship));

    if (cc.hasAddress()) {
      s.append(CdaFhirUtilities.getAddressXml(cc.getAddress()));
    }

    if (cc.hasTelecom()) {
      s.append(CdaFhirUtilities.getTelecomXml(cc.getTelecom(), false));
    }

    s.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSOCIATED_PERSON_EL_NAME));
    s.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.NAME_EL_NAME));

    List<HumanName> names = new ArrayList<>();
    if (cc.getName() != null) {
      names.add(cc.getName());
    }
    s.append(CdaFhirUtilities.getNameXml(names));
    s.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));

    s.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSOCIATED_PERSON_EL_NAME));

    s.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSOCIATED_ENTITY_EL_NAME));
    s.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PARTICIPANT_EL_NAME));

    return s.toString();
  }

  public static Coding getTranslatableCodeableConceptCoding(List<CodeableConcept> cds) {

    if (cds != null) {

      for (CodeableConcept cd : cds) {

        List<Coding> codes = cd.getCoding();

        Coding c = getTranslatableCoding(codes);

        if (c != null) {

          return c;
        }
      }
    }

    return null;
  }

  public static Coding getTranslatableCoding(List<Coding> codes) {

    if (codes != null) {

      for (Coding c : codes) {

        if (c.getCode() != null) {

          String relationship = CdaGeneratorConstants.getCodeForContactRelationship(c.getCode());

          if (relationship != null && !relationship.isEmpty()) {

            return c;
          }
        }
      }
    }

    return null;
  }

  public static String getLocationXml(Location loc, Organization org, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(500);

    if (loc != null) {

      logger.info("Location data is present, using it to populate LOCATION detail in XML");
      Identifier npi =
          CdaFhirUtilities.getIdentifierForSystem(
              loc.getIdentifier(), CdaGeneratorConstants.FHIR_NPI_URL);

      if (npi != null) {
        sb.append(
            CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA, npi.getValue()));
      } else {
        sb.append(
            CdaGeneratorUtils.getXmlForII(
                details.getAssigningAuthorityId(), loc.getIdElement().getIdPart()));
      }

      if (loc.getType() != null) {

        List<CodeableConcept> cds = loc.getType();

        String typeXml =
            CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
                cds,
                CdaGeneratorConstants.CODE_EL_NAME,
                false,
                CdaGeneratorConstants.FHIR_LOC_ROLE_CODE_TYPE_V3,
                false);

        sb.append(typeXml);
      } else {
        List<Coding> codes = null;
        sb.append(CdaFhirUtilities.getCodingXml(codes, CdaGeneratorConstants.CODE_EL_NAME, ""));
      }

      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.LOCATION_EL_NAME));

      List<Address> addrs = new ArrayList<>();
      addrs.add(loc.getAddress());
      sb.append(CdaFhirUtilities.getAddressXml(addrs, false));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.LOCATION_EL_NAME));

    } else if (org != null) {

      logger.info(
          "Location data is not present, using Organization data to populate LOCATION detail in XML");
      Identifier npi =
          CdaFhirUtilities.getIdentifierForSystem(
              org.getIdentifier(), CdaGeneratorConstants.FHIR_NPI_URL);

      if (npi != null) {
        sb.append(
            CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA, npi.getValue()));
      } else {
        sb.append(
            CdaGeneratorUtils.getXmlForII(
                details.getAssigningAuthorityId(), org.getIdElement().getIdPart()));
      }

      if (org.getType() != null) {

        List<CodeableConcept> cds = org.getType();
        logger.debug("Getting organization Type to populate location code");
        String typeXml =
            CdaFhirUtilities.getCodeableConceptXml(cds, CdaGeneratorConstants.CODE_EL_NAME, false);

        sb.append(typeXml);
      } else {
        List<Coding> codes = null;
        sb.append(CdaFhirUtilities.getCodingXml(codes, CdaGeneratorConstants.CODE_EL_NAME, ""));
      }

      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.LOCATION_EL_NAME));

      if (org.hasAddress()) {
        List<Address> addrs = org.getAddress();
        sb.append(CdaFhirUtilities.getAddressXml(addrs, false));
      } else {
        List<Address> addrs = null;
        sb.append(CdaFhirUtilities.getAddressXml(addrs, false));
      }
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.LOCATION_EL_NAME));

    } else {

      logger.info("Location and Organization data not present, so populate using null flavors");

      sb.append(
          CdaGeneratorUtils.getXmlForII(
              details.getAssigningAuthorityId(), CdaGeneratorConstants.UNKNOWN_VALUE));
      sb.append(
          CdaGeneratorUtils.getXmlForNullCD(
              CdaGeneratorConstants.CODE_EL_NAME, CdaGeneratorConstants.NF_NI));

      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.LOCATION_EL_NAME));
      List<Address> addrs = null;
      sb.append(CdaFhirUtilities.getAddressXml(addrs, false));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.LOCATION_EL_NAME));
    }

    return sb.toString();
  }

  public static String getAdditionalAuthorXml(String manufacturer, String swversion) {

    StringBuilder sb = new StringBuilder(200);

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.AUTHOR_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForEffectiveTime(
            CdaGeneratorConstants.TIME_EL_NAME, CdaGeneratorUtils.getCurrentDateTime()));
    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSIGNED_AUTHOR_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());
    Address addr = null;
    sb.append(CdaFhirUtilities.getAddressXml(addr));
    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(
            CdaGeneratorConstants.ASSIGNED_AUTHORING_DEVICE_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForElementWithAttribute(
            CdaGeneratorConstants.MANU_MODEL_NAME_EL_NAME,
            CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL,
            manufacturer));
    sb.append(
        CdaGeneratorUtils.getXmlForElementWithAttribute(
            CdaGeneratorConstants.SOFTWARE_NAME_EL_NAME,
            CdaGeneratorConstants.DISPLAYNAME_WITH_EQUAL,
            swversion));

    sb.append(
        CdaGeneratorUtils.getXmlForEndElement(
            CdaGeneratorConstants.ASSIGNED_AUTHORING_DEVICE_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_AUTHOR_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.AUTHOR_EL_NAME));

    return sb.toString();
  }

  public static String getAuthorXml(
      R4FhirData data, Encounter en, HashMap<V3ParticipationType, List<Practitioner>> practMap) {

    logger.debug("R4FhirData in getAuthorXml :{}", data);

    StringBuilder sb = new StringBuilder(500);

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.AUTHOR_EL_NAME));

    if (en != null
        && en.getPeriod() != null
        && en.getPeriod().getStartElement() != null
        && en.getPeriod().getStart() != null) {
      sb.append(
          CdaGeneratorUtils.getXmlForEffectiveTime(
              CdaGeneratorConstants.TIME_EL_NAME,
              en.getPeriod().getStart(),
              en.getPeriod().getStartElement().getTimeZone()));
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForEffectiveTime(
              CdaGeneratorConstants.TIME_EL_NAME, CdaGeneratorUtils.getCurrentDateTime()));
    }

    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSIGNED_AUTHOR_EL_NAME));

    List<Practitioner> practs = new ArrayList<>();
    Boolean foundAuthor = false;

    if (practMap != null && !practMap.isEmpty()) {

      if (practMap.containsKey(V3ParticipationType.AUT)) {
        logger.debug("Found Practitioner who is an Author");
        practs.addAll(practMap.get(V3ParticipationType.AUT));
        foundAuthor = true;
      } else if (practMap.containsKey(V3ParticipationType.PPRF)) {
        logger.debug("Found Practitioner who is a PPRF for an Author");
        practs.addAll(practMap.get(V3ParticipationType.PPRF));
        foundAuthor = true;
      } else if (practMap.containsKey(V3ParticipationType.ATND)) {
        logger.debug("Found Practitioner who is an ATND for an Author");
        practs.addAll(practMap.get(V3ParticipationType.ATND));
        foundAuthor = true;
      } else if (practMap.containsKey(V3ParticipationType.SPRF)) {
        logger.debug("Found Practitioner who is an SPRF for an Author");
        practs.addAll(practMap.get(V3ParticipationType.SPRF));
        foundAuthor = true;
      }

      if (!practs.isEmpty()) {
        logger.info(
            "Found {} Practitioner with valid type, adding XML for Practitioner", practs.size());
        Practitioner pr = practs.get(0);
        sb.append(CdaFhirUtilities.getPractitionerXml(pr));
      } else {
        logger.info("Didn't find a Practitioner with valid type");
      }
    } else {
      logger.info("No Practitioner found");
    }

    if (Boolean.FALSE.equals(foundAuthor)) {
      sb.append(CdaFhirUtilities.getPractitionerXml(null));
    }

    // add reprsented organization if it exists
    if (data.getOrganization() != null && data.getOrganization().hasName()) {

      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.REP_ORG_EL_NAME));
      sb.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.NAME_EL_NAME, data.getOrganization().getName()));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.REP_ORG_EL_NAME));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_AUTHOR_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.AUTHOR_EL_NAME));

    return sb.toString();
  }

  public static String getOrganizationXml(
      Organization org, LaunchDetails details, boolean onlyOneTelecom) {

    StringBuilder sb = new StringBuilder(200);
    if (org != null) {

      Identifier id = org.getIdentifierFirstRep();

      if (id != null && !id.isEmpty()) {

        sb.append(
            CdaGeneratorUtils.getXmlForII(
                CdaGeneratorUtils.getRootOid(id.getSystem(), details.getAssigningAuthorityId()),
                id.getValue()));
      } else {
        sb.append(
            CdaGeneratorUtils.getXmlForII(
                details.getAssigningAuthorityId(), org.getIdElement().getIdPart()));
      }

      sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.NAME_EL_NAME, org.getName()));
      sb.append(CdaFhirUtilities.getTelecomXml(org.getTelecom(), onlyOneTelecom));
      sb.append(CdaFhirUtilities.getAddressXml(org.getAddress(), false));

    } else {

      // Code that will replace the code above after testing.
      sb.append(CdaGeneratorUtils.getNFXMLForII(CdaGeneratorConstants.NF_NI));
      sb.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.NAME_EL_NAME, CdaGeneratorConstants.UNKNOWN_VALUE));

      List<ContactPoint> cps = null;
      sb.append(CdaFhirUtilities.getTelecomXml(cps, onlyOneTelecom));

      List<Address> addrs = null;
      sb.append(CdaFhirUtilities.getAddressXml(addrs, false));
    }

    return sb.toString();
  }

  public static String getCustodianXml(LaunchDetails details, R4FhirData data) {

    StringBuilder sb = new StringBuilder(500);

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.CUSTODIAN_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSGND_CUST_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.REP_CUST_ORG_EL_NAME));

    sb.append(getOrganizationXml(data.getOrganization(), details, true));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.REP_CUST_ORG_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSGND_CUST_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.CUSTODIAN_EL_NAME));

    return sb.toString();
  }

  public static String getEncompassingEncounter(
      Encounter en,
      HashMap<V3ParticipationType, List<Practitioner>> practMap,
      LaunchDetails details,
      R4FhirData data) {

    StringBuilder sb = new StringBuilder(2000);

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMPONENT_OF_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENCOMPASSING_ENC_EL_NAME));

    if (en != null) {
      sb.append(
          CdaGeneratorUtils.getXmlForII(
              details.getAssigningAuthorityId(), en.getIdElement().getIdPart()));

      // Add Identifiers
      List<Identifier> ids = en.getIdentifier();
      if (ids != null) {

        for (Identifier id : ids) {

          if (id.getSystem() != null && id.getValue() != null) {

            sb.append(
                CdaGeneratorUtils.getXmlForII(
                    CdaGeneratorUtils.getRootOid(id.getSystem(), details.getAssigningAuthorityId()),
                    id.getValue()));
          }
        }
      }

      sb.append(CdaEncounterGenerator.getEncounterCodeXml(en, ""));
      sb.append(
          CdaFhirUtilities.getPeriodXml(
              en.getPeriod(), CdaGeneratorConstants.EFF_TIME_EL_NAME, false));
    } else {
      sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());
      sb.append(
          CdaGeneratorUtils.getXmlForNullCD(
              CdaGeneratorConstants.CODE_EL_NAME, CdaGeneratorConstants.NF_NI));
      sb.append(
          CdaGeneratorUtils.getXmlForNullEffectiveTime(
              CdaGeneratorConstants.EFF_TIME_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.RESP_PARTY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSIGNED_ENTITY_EL_NAME));

    sb.append(getXmlForRelevantPractitioner(practMap));

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.REP_ORG_EL_NAME));

    sb.append(getOrganizationXml(data.getOrganization(), details, false));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.REP_ORG_EL_NAME));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_ENTITY_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.RESP_PARTY_EL_NAME));

    // Add all practitioners
    sb.append(getXmlForAllRelevantPractitioners(practMap));

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.LOCATION_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.HEALTHCARE_FACILITY_EL_NAME));

    sb.append(getLocationXml(data.getLocation(), data.getOrganization(), details));

    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(
            CdaGeneratorConstants.SERVICE_PROVIDER_ORG_EL_NAME));

    sb.append(getOrganizationXml(data.getOrganization(), details, false));

    sb.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SERVICE_PROVIDER_ORG_EL_NAME));

    sb.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.HEALTHCARE_FACILITY_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.LOCATION_EL_NAME));

    sb.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENCOMPASSING_ENC_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMPONENT_OF_EL_NAME));

    return sb.toString();
  }

  public static HashMap<V3ParticipationType, List<Practitioner>> getSortedPractitionerList(
      R4FhirData data) {

    HashMap<V3ParticipationType, List<Practitioner>> practitionerMap = new HashMap<>();

    // Add Authors
    List<Practitioner> practs =
        CdaFhirUtilities.getPractitionersForType(data, V3ParticipationType.AUT);

    if (!practs.isEmpty()) {

      logger.debug("Found Author Practitioner {}", practs.size());
      practitionerMap.put(V3ParticipationType.AUT, practs);
    }

    // Add PPRF
    practs = CdaFhirUtilities.getPractitionersForType(data, V3ParticipationType.PPRF);

    if (!practs.isEmpty()) {

      logger.debug("Found PPRF Practitioners {}", practs.size());
      practitionerMap.put(V3ParticipationType.PPRF, practs);
    }

    // Add SPRF
    practs = CdaFhirUtilities.getPractitionersForType(data, V3ParticipationType.SPRF);

    if (!practs.isEmpty()) {

      logger.debug("Found SPRF Practitioners {}", practs.size());
      practitionerMap.put(V3ParticipationType.SPRF, practs);
    }

    // Add PPRF
    practs = CdaFhirUtilities.getPractitionersForType(data, V3ParticipationType.ATND);

    if (!practs.isEmpty()) {

      logger.debug("Found ATND Practitioners {}", practs.size());
      practitionerMap.put(V3ParticipationType.ATND, practs);
    }

    return practitionerMap;
  }

  public static String getXmlForRelevantPractitioner(
      HashMap<V3ParticipationType, List<Practitioner>> practMap) {

    StringBuilder practXml = new StringBuilder();

    List<Practitioner> practs = new ArrayList<>();
    Boolean foundPrimaryPerformer = false;

    if (practMap != null && !practMap.isEmpty()) {

      if (practMap.containsKey(V3ParticipationType.PPRF)) {
        logger.debug("Found Practitioner who is a Primary Performer");
        practs.addAll(practMap.get(V3ParticipationType.PPRF));
        foundPrimaryPerformer = true;
      } else if (practMap.containsKey(V3ParticipationType.ATND)) {
        logger.debug("Found Practitioner who is an ATND for a Primary performer");
        practs.addAll(practMap.get(V3ParticipationType.ATND));
        foundPrimaryPerformer = true;
      } else if (practMap.containsKey(V3ParticipationType.SPRF)) {
        logger.debug("Found Practitioner who is an SPRF for a Pimary Performer");
        practs.addAll(practMap.get(V3ParticipationType.SPRF));
        foundPrimaryPerformer = true;
      }

      if (!practs.isEmpty()) {
        logger.info(
            "Found {} Practitioner with valid type, adding XML for Practitioner", practs.size());
        Practitioner pr = practs.get(0);
        practXml.append(CdaFhirUtilities.getPractitionerXml(pr));
      } else {
        logger.info("Didn't find a Practitioner with valid type");
      }
    } else {
      logger.info("No Practitioner found");
    }

    if (Boolean.FALSE.equals(foundPrimaryPerformer)) {
      practXml.append(CdaFhirUtilities.getPractitionerXml(null));
    }

    return practXml.toString();
  }

  public static String getXmlForAllRelevantPractitioners(
      HashMap<V3ParticipationType, List<Practitioner>> practs) {

    StringBuilder sb = new StringBuilder();

    if (practs != null && !practs.isEmpty()) {

      for (Map.Entry<V3ParticipationType, List<Practitioner>> pr : practs.entrySet()) {

        List<Practitioner> prs = pr.getValue();

        for (Practitioner p : prs) {

          if (pr.getKey() == V3ParticipationType.ATND
              || pr.getKey() == V3ParticipationType.DIS
              || pr.getKey() == V3ParticipationType.ADM
              || pr.getKey() == V3ParticipationType.REF
              || pr.getKey() == V3ParticipationType.CON) {

            sb.append(
                CdaGeneratorUtils.getXmlForStartElementWithTypeCode(
                    CdaGeneratorConstants.ENCOUNTER_PARTICIPANT_EL_NAME, pr.getKey().toString()));
            sb.append(
                CdaGeneratorUtils.getXmlForStartElement(
                    CdaGeneratorConstants.ASSIGNED_ENTITY_EL_NAME));

            sb.append(CdaFhirUtilities.getPractitionerXml(p));

            sb.append(
                CdaGeneratorUtils.getXmlForEndElement(
                    CdaGeneratorConstants.ASSIGNED_ENTITY_EL_NAME));
            sb.append(
                CdaGeneratorUtils.getXmlForEndElement(
                    CdaGeneratorConstants.ENCOUNTER_PARTICIPANT_EL_NAME));
          }
        }
      }
    }

    return sb.toString();
  }

  public static String getPatientDetails(Patient p, LaunchDetails details) {

    StringBuilder patientDetails = new StringBuilder();

    patientDetails.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.RECORD_TARGET_EL_NAME));
    patientDetails.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.PATIENT_ROLE_EL_NAME));

    List<Identifier> ids =
        CdaFhirUtilities.getIdentifierForType(
            p.getIdentifier(), CdaFhirEnumConstants.FHIR_ID_TYPE_MR);

    Boolean addOnce = true;

    if (ids != null && !ids.isEmpty()) {

      for (Identifier id : ids) {

        if (!StringUtils.isEmpty(id.getSystem()) && !StringUtils.isEmpty(id.getValue())) {

          logger.debug("Found Identifier with Type MR");

          String system =
              CdaGeneratorUtils.getRootOid(id.getSystem(), details.getAssigningAuthorityId());

          patientDetails.append(CdaGeneratorUtils.getXmlForII(system, id.getValue()));

        } else {

          logger.debug("Using Resource Identifier as id");

          if (Boolean.TRUE.equals(addOnce)) {
            patientDetails.append(
                CdaGeneratorUtils.getXmlForII(
                    details.getAssigningAuthorityId(), p.getIdElement().getIdPart()));
            addOnce = false;
          }
        }
      }

    } else {
      logger.debug("Using Resource Identifier as id");
      patientDetails.append(
          CdaGeneratorUtils.getXmlForII(
              details.getAssigningAuthorityId(), p.getIdElement().getIdPart()));
    }

    // Add Address.
    patientDetails.append(CdaFhirUtilities.getAddressXml(p.getAddress(), true));

    // Add Telecom (Phone)
    patientDetails.append(CdaFhirUtilities.getTelecomXml(p.getTelecom(), false));

    // Add patient
    patientDetails.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.PATIENT_EL_NAME));

    String nameUse = CdaFhirUtilities.getCodeForNameUse(p.getName());
    patientDetails.append(
        CdaGeneratorUtils.getXmlForStartElementWithAttribute(
            CdaGeneratorConstants.NAME_EL_NAME, CdaGeneratorConstants.USE_ATTR_NAME, nameUse));
    patientDetails.append(CdaFhirUtilities.getNameXml(p.getName()));
    patientDetails.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));

    patientDetails.append(CdaFhirUtilities.getGenderXml(p.getGenderElement().getValue()));

    patientDetails.append(
        CdaFhirUtilities.getDateTypeXml(
            p.getBirthDateElement(), CdaGeneratorConstants.BIRTH_TIME_EL_NAME));

    patientDetails.append(getDeceasedXml(p));

    if (p.hasMaritalStatus()) {
      patientDetails.append(CdaFhirUtilities.getMaritalStatusXml(p.getMaritalStatus()));
    }

    Coding religion =
        CdaFhirUtilities.getCodingExtension(
            p.getExtension(), CdaGeneratorConstants.FHIR_RELIGION_EXT_URL);

    if (religion != null) {
      patientDetails.append(CdaFhirUtilities.getReligiousAffiliationXml(religion));
    }

    Coding race =
        CdaFhirUtilities.getCodingExtension(
            p.getExtension(),
            CdaGeneratorConstants.FHIR_USCORE_RACE_EXT_URL,
            CdaGeneratorConstants.OMB_RACE_CATEGORY_URL);

    if (race != null && race.hasCode()) {
      if (!isCodingNullFlavor(race)) {
        patientDetails.append(
            CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.RACE_CODE_EL_NAME,
                race.getCode(),
                CdaGeneratorConstants.RACE_CODE_SYSTEM,
                CdaGeneratorConstants.RACE_CODE_SYSTEM_NAME,
                race.getDisplay()));
      } else {
        patientDetails.append(
            CdaGeneratorUtils.getXmlForNullCD(
                CdaGeneratorConstants.RACE_CODE_EL_NAME, race.getCode()));
      }
    } else {
      patientDetails.append(
          CdaGeneratorUtils.getXmlForNullCD(
              CdaGeneratorConstants.RACE_CODE_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    patientDetails.append(
        generateXmlForDetailedRaceAndEthnicityCodes(
            p.getExtension(),
            CdaGeneratorConstants.FHIR_USCORE_RACE_EXT_URL,
            CdaGeneratorConstants.OMB_RACE_DETAILED_URL,
            CdaGeneratorConstants.SDTC_DETAILED_RACE_CODE));

    Coding ethnicity =
        CdaFhirUtilities.getCodingExtension(
            p.getExtension(),
            CdaGeneratorConstants.FHIR_USCORE_ETHNICITY_EXT_URL,
            CdaGeneratorConstants.OMB_RACE_CATEGORY_URL);

    if (ethnicity != null && ethnicity.hasCode()) {
      if (!isCodingNullFlavor(ethnicity)) {

        patientDetails.append(
            CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.ETHNIC_CODE_EL_NAME,
                ethnicity.getCode(),
                CdaGeneratorConstants.RACE_CODE_SYSTEM,
                CdaGeneratorConstants.RACE_CODE_SYSTEM_NAME,
                ethnicity.getDisplay()));
      } else {
        patientDetails.append(
            CdaGeneratorUtils.getXmlForNullCD(
                CdaGeneratorConstants.ETHNIC_CODE_EL_NAME, ethnicity.getCode()));
      }
    } else {
      patientDetails.append(
          CdaGeneratorUtils.getXmlForNullCD(
              CdaGeneratorConstants.ETHNIC_CODE_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    patientDetails.append(
        generateXmlForDetailedRaceAndEthnicityCodes(
            p.getExtension(),
            CdaGeneratorConstants.FHIR_USCORE_ETHNICITY_EXT_URL,
            CdaGeneratorConstants.OMB_ETHNICITY_DETAILED_URL,
            CdaGeneratorConstants.SDTC_DETAILED_ETHNIC_GROUP_CODE));

    // Adding Guardian
    if (p.getContact() != null && !p.getContact().isEmpty()) {

      ContactComponent guardianContact = CdaFhirUtilities.getGuardianContact(p.getContact());

      if (guardianContact != null) {

        patientDetails.append(
            CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.GUARDIAN_EL_NAME));

        if (guardianContact.hasRelationship()) {
          Coding coding =
              CdaFhirUtilities.getSingleCodingForCodeSystems(
                  guardianContact.getRelationship(),
                  CdaGeneratorConstants.FHIR_CONTACT_RELATIONSHIP_CODESYSTEM);

          if (coding != null) {
            patientDetails.append(
                CdaFhirUtilities.getCodingXmlForCodeSystem(
                    Collections.singletonList(coding),
                    CdaGeneratorConstants.CODE_EL_NAME,
                    CdaGeneratorConstants.FHIR_LOC_ROLE_CODE_TYPE_V3,
                    false,
                    ""));
          }
        }

        // Add address if found
        List<Address> addrs = new ArrayList<>();
        if (guardianContact.getAddress() != null) {

          logger.debug("Adding Address for Guardian");
          addrs.add(guardianContact.getAddress());
          patientDetails.append(CdaFhirUtilities.getAddressXml(addrs, false));
        } else {
          patientDetails.append(CdaFhirUtilities.getAddressXml(addrs, false));
        }

        // Add Telecom
        patientDetails.append(CdaFhirUtilities.getTelecomXml(guardianContact.getTelecom(), false));

        patientDetails.append(
            CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.GUARDIAN_PERSON_EL_NAME));

        patientDetails.append(
            CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.NAME_EL_NAME));

        List<HumanName> names = new ArrayList<>();
        names.add(guardianContact.getName());

        patientDetails.append(CdaFhirUtilities.getNameXml(names));

        patientDetails.append(
            CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));
        patientDetails.append(
            CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.GUARDIAN_PERSON_EL_NAME));

        patientDetails.append(
            CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.GUARDIAN_EL_NAME));
      }
    }

    patientDetails.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.LANGUAGE_COMM_EL_NAME));
    Pair<Coding, Boolean> language =
        CdaFhirUtilities.getLanguageForCodeSystem(
            p.getCommunication(), CdaGeneratorConstants.FHIR_LANGUAGE_CODESYSTEM_URL);

    if (language != null
        && language.getValue0() != null
        && language.getValue0().getCode() != null) {
      patientDetails.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.LANGUAGE_CODE_EL_NAME, language.getValue0().getCode()));

      // Add preferred indicator.
      if (language.getValue1()) {
        patientDetails.append(
            CdaGeneratorUtils.getXmlForValue(CdaGeneratorConstants.LANGUAGE_PREF_IND, "true"));
      }
    } else {
      patientDetails.append(
          CdaGeneratorUtils.getXmlForNullCD(
              CdaGeneratorConstants.LANGUAGE_CODE_EL_NAME, CdaGeneratorConstants.NF_NI));
    }
    patientDetails.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.LANGUAGE_COMM_EL_NAME));

    patientDetails.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PATIENT_EL_NAME));
    patientDetails.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PATIENT_ROLE_EL_NAME));
    patientDetails.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.RECORD_TARGET_EL_NAME));

    return patientDetails.toString();
  }

  public static String getDeceasedXml(Patient p) {

    StringBuilder patientDetails = new StringBuilder(200);

    if (p.getDeceased() != null) {

      if (p.hasDeceasedBooleanType()
          && Boolean.TRUE.equals(p.getDeceasedBooleanType().getValue())) {

        patientDetails.append(
            CdaGeneratorUtils.getXmlForValue(
                CdaGeneratorConstants.SDTC_DECEASED_IND, CdaGeneratorConstants.CCDA_TRUE));

        patientDetails.append(
            CdaGeneratorUtils.getXmlForNullEffectiveTime(
                CdaGeneratorConstants.SDTC_DECEASED_TIME, CdaGeneratorConstants.NF_NI));
      } else if (p.hasDeceasedDateTimeType() && p.getDeceased() instanceof DateTimeType) {

        patientDetails.append(
            CdaGeneratorUtils.getXmlForValue(
                CdaGeneratorConstants.SDTC_DECEASED_IND, CdaGeneratorConstants.CCDA_TRUE));

        DateTimeType d = (DateTimeType) p.getDeceased();
        patientDetails.append(
            CdaFhirUtilities.getDateTimeTypeXml(d, CdaGeneratorConstants.SDTC_DECEASED_TIME));
      } else {
        patientDetails.append(
            CdaGeneratorUtils.getXmlForValue(
                CdaGeneratorConstants.SDTC_DECEASED_IND, CdaGeneratorConstants.CCDA_FALSE));
      }
    } else if (p.getDeceased() == null || (p.getDeceased() != null && p.getDeceased().isEmpty())) {
      patientDetails.append(
          CdaGeneratorUtils.getXmlForValue(
              CdaGeneratorConstants.SDTC_DECEASED_IND, CdaGeneratorConstants.CCDA_FALSE));
    }

    return patientDetails.toString();
  }

  private static boolean isCodingNullFlavor(Coding coding) {

    if (coding != null
        && coding.getCode() != null
        && (coding.getCode().contentEquals("ASKU") || coding.getCode().contentEquals("UNK"))) {
      return true;
    } else return false;
  }

  public static String generateXmlForDetailedRaceAndEthnicityCodes(
      List<Extension> extensions, String extensionUrl, String categoryUrl, String xmlElementName) {
    StringBuilder sb = new StringBuilder();

    List<Coding> detailedCodings =
        CdaFhirUtilities.getAllCodingsFromExtension(extensions, extensionUrl, categoryUrl);

    if (detailedCodings.isEmpty()) {
      return "";
    }
    for (Coding detailedCoding : detailedCodings) {
      if (detailedCoding.hasCode()) {
        String code = detailedCoding.getCode();
        if (isCodingNullFlavor(detailedCoding)) {
          sb.append(CdaGeneratorUtils.getXmlForNullCD(xmlElementName, code));
        } else {
          sb.append(
              CdaGeneratorUtils.getXmlForCD(
                  xmlElementName,
                  code,
                  CdaGeneratorConstants.RACE_CODE_SYSTEM,
                  CdaGeneratorConstants.RACE_CODE_SYSTEM_NAME,
                  detailedCoding.getDisplay()));
        }
      }
    }
    return sb.toString();
  }
}
