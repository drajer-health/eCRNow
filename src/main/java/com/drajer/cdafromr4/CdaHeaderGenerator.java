package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.ActionRepo;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Address.AddressUse;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Patient.ContactComponent;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.codesystems.V3ParticipationType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaHeaderGenerator {

  private CdaHeaderGenerator() {}

  private static final Logger logger = LoggerFactory.getLogger(CdaHeaderGenerator.class);

  public static String createCdaHeader(R4FhirData data, LaunchDetails details, Eicr ecr) {

    StringBuilder eICRHeader = new StringBuilder();

    if (data != null) {

      eICRHeader.append(CdaGeneratorUtils.getXmlHeaderForClinicalDocument());

      // Set the clinical document id.
      String docId = CdaGeneratorUtils.getGuid();
      eICRHeader.append(CdaGeneratorUtils.getXmlForII(docId));
      ecr.setEicrDocId(docId);
      ecr.setxCoorrelationId(docId);

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

        eICRHeader.append(getCustodianXml(details, data));

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

  public static String getPractitionerXml(Practitioner pr) {

    StringBuilder sb = new StringBuilder(500);

    if (pr != null) {

      Identifier npi =
          CdaFhirUtilities.getIdentifierForSystem(
              pr.getIdentifier(), CdaGeneratorConstants.FHIR_NPI_URL);

      if (npi != null) {
        sb.append(
            CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA, npi.getValue()));
      } else {
        sb.append(CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA));
      }

      sb.append(CdaFhirUtilities.getAddressXml(pr.getAddress()));
      sb.append(CdaFhirUtilities.getTelecomXml(pr.getTelecom()));

      sb.append(
          CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSIGNED_PERSON_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.NAME_EL_NAME));

      List<HumanName> hns = pr.getName();
      sb.append(CdaFhirUtilities.getNameXml(hns));

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));
      sb.append(
          CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_PERSON_EL_NAME));

    } else {

      sb.append(CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA));

      List<Address> addrs = null;
      sb.append(CdaFhirUtilities.getAddressXml(addrs));

      List<ContactPoint> cps = null;
      sb.append(CdaFhirUtilities.getTelecomXml(cps));

      sb.append(
          CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSIGNED_PERSON_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.NAME_EL_NAME));

      List<HumanName> hns = null;
      sb.append(CdaFhirUtilities.getNameXml(hns));

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));
      sb.append(
          CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_PERSON_EL_NAME));
    }

    return sb.toString();
  }

  public static String getLocationXml(Location loc, Organization org, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(500);

    if (loc != null) {

      logger.info(" Location is not null, so get the location populated");
      Identifier npi =
          CdaFhirUtilities.getIdentifierForSystem(
              loc.getIdentifier(), CdaGeneratorConstants.FHIR_NPI_URL);

      if (npi != null) {
        sb.append(
            CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA, npi.getValue()));
      } else {
        sb.append(CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), loc.getId()));
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
        sb.append(CdaFhirUtilities.getCodingXml(codes, CdaGeneratorConstants.CODE_EL_NAME));
      }

      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.LOCATION_EL_NAME));

      List<Address> addrs = new ArrayList<>();
      addrs.add(loc.getAddress());
      sb.append(CdaFhirUtilities.getAddressXml(addrs));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.LOCATION_EL_NAME));

    } else if (org != null) {

      logger.info(" Location is null, so get the location populated using Organization details ");
      Identifier npi =
          CdaFhirUtilities.getIdentifierForSystem(
              org.getIdentifier(), CdaGeneratorConstants.FHIR_NPI_URL);

      if (npi != null) {
        sb.append(
            CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA, npi.getValue()));
      } else {
        sb.append(CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), org.getId()));
      }

      if (org.getType() != null) {

        List<CodeableConcept> cds = org.getType();
        logger.debug(" Getting organization Type to populate location code ");
        String typeXml =
            CdaFhirUtilities.getCodeableConceptXml(cds, CdaGeneratorConstants.CODE_EL_NAME, false);

        sb.append(typeXml);
      } else {
        List<Coding> codes = null;
        sb.append(CdaFhirUtilities.getCodingXml(codes, CdaGeneratorConstants.CODE_EL_NAME));
      }

      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.LOCATION_EL_NAME));

      if (org.getAddress() != null) {
        List<Address> addrs = org.getAddress();
        sb.append(CdaFhirUtilities.getAddressXml(addrs));
      } else {
        List<Address> addrs = null;
        sb.append(CdaFhirUtilities.getAddressXml(addrs));
      }
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.LOCATION_EL_NAME));

    } else {

      logger.info(" Location is null, Organization is null, so populate using null flavors ");

      sb.append(
          CdaGeneratorUtils.getXmlForII(
              details.getAssigningAuthorityId(), CdaGeneratorConstants.UNKNOWN_VALUE));
      sb.append(
          CdaGeneratorUtils.getXmlForNullCD(
              CdaGeneratorConstants.CODE_EL_NAME, CdaGeneratorConstants.NF_NI));

      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.LOCATION_EL_NAME));
      List<Address> addrs = null;
      sb.append(CdaFhirUtilities.getAddressXml(addrs));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.LOCATION_EL_NAME));
    }

    return sb.toString();
  }

  public static String getAuthorXml(
      R4FhirData data, Encounter en, HashMap<V3ParticipationType, List<Practitioner>> practMap) {

    StringBuilder sb = new StringBuilder(500);

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.AUTHOR_EL_NAME));

    if (en != null && en.getPeriod().getStart() != null) {
      sb.append(
          CdaGeneratorUtils.getXmlForEffectiveTime(
              CdaGeneratorConstants.TIME_EL_NAME, en.getPeriod().getStart()));
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

        logger.info(" Found Practitioner who is an Author ");
        practs.addAll(practMap.get(V3ParticipationType.AUT));
        foundAuthor = true;
      } else if (practMap.containsKey(V3ParticipationType.PPRF)) {

        logger.info(" Found Practitioner who is a PPRF for an Author");
        practs.addAll(practMap.get(V3ParticipationType.PPRF));
        foundAuthor = true;
      } else if (practMap.containsKey(V3ParticipationType.ATND)) {
        logger.info(" Found Practitioner who is an ATND for an Author ");
        practs.addAll(practMap.get(V3ParticipationType.ATND));
        foundAuthor = true;
      } else if (practMap.containsKey(V3ParticipationType.SPRF)) {
        logger.info(" Found Practitioner who is an SPRF for an Author");
        practs.addAll(practMap.get(V3ParticipationType.SPRF));
        foundAuthor = true;
      }

      logger.info(" Practs size = {}", practs.size());
      if (!practs.isEmpty()) {

        logger.info(" Practs is not empty, adding XML for Practitioner ");
        Practitioner pr = practs.get(0);
        sb.append(getPractitionerXml(pr));
      } else {
        logger.info(" Practs is null or empty after author checking ");
      }
    } else {
      logger.info(" Pract Map is null or empty ");
    }

    if (!foundAuthor) {
      logger.info(" Did not find author ");
      sb.append(getPractitionerXml(null));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_AUTHOR_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.AUTHOR_EL_NAME));

    return sb.toString();
  }

  public static String getOrganizationXml(Organization org, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(200);
    if (org != null) {

      Identifier id = org.getIdentifierFirstRep();

      if (id != null && !id.isEmpty()) {

        sb.append(
            CdaGeneratorUtils.getXmlForII(
                CdaGeneratorUtils.getRootOid(id.getSystem(), details.getAssigningAuthorityId()),
                id.getValue()));
      } else {
        sb.append(CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), org.getId()));
      }

      sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.NAME_EL_NAME, org.getName()));
      sb.append(CdaFhirUtilities.getTelecomXml(org.getTelecom()));
      sb.append(CdaFhirUtilities.getAddressXml(org.getAddress()));

    } else {

      // Code that will replace the code above after testing.
      sb.append(CdaGeneratorUtils.getNFXMLForII(CdaGeneratorConstants.NF_NI));
      sb.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.NAME_EL_NAME, CdaGeneratorConstants.UNKNOWN_VALUE));

      List<ContactPoint> cps = null;
      sb.append(CdaFhirUtilities.getTelecomXml(cps));

      List<Address> addrs = null;
      sb.append(CdaFhirUtilities.getAddressXml(addrs));
    }

    return sb.toString();
  }

  public static String getCustodianXml(LaunchDetails details, R4FhirData data) {

    StringBuilder sb = new StringBuilder(500);

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.CUSTODIAN_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSGND_CUST_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.REP_CUST_ORG_EL_NAME));

    sb.append(getOrganizationXml(data.getOrganization(), details));

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
      sb.append(CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), en.getId()));

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

      sb.append(CdaEncounterGenerator.getEncounterCodeXml(en));
      sb.append(
          CdaFhirUtilities.getPeriodXml(en.getPeriod(), CdaGeneratorConstants.EFF_TIME_EL_NAME));
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

    sb.append(getOrganizationXml(data.getOrganization(), details));

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

    sb.append(getOrganizationXml(data.getOrganization(), details));

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

      logger.info(" Found Author Practitioner {}", practs.size());
      practitionerMap.put(V3ParticipationType.AUT, practs);
    }

    // Add PPRF
    practs = CdaFhirUtilities.getPractitionersForType(data, V3ParticipationType.PPRF);

    if (!practs.isEmpty()) {

      logger.info(" Found PPRF Practitioners {}", practs.size());
      practitionerMap.put(V3ParticipationType.PPRF, practs);
    }

    // Add SPRF
    practs = CdaFhirUtilities.getPractitionersForType(data, V3ParticipationType.SPRF);

    if (!practs.isEmpty()) {

      logger.info(" Found SPRF Practitioners {}", practs.size());
      practitionerMap.put(V3ParticipationType.SPRF, practs);
    }

    // Add PPRF
    practs = CdaFhirUtilities.getPractitionersForType(data, V3ParticipationType.ATND);

    if (!practs.isEmpty()) {

      logger.info(" Found ATND Practitioners {}", practs.size());
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

        logger.info(" Found Practitioner who is a Primary Performer ");
        practs.addAll(practMap.get(V3ParticipationType.PPRF));
        foundPrimaryPerformer = true;
      } else if (practMap.containsKey(V3ParticipationType.ATND)) {
        logger.info(" Found Practitioner who is an ATND for a Primary performer ");
        practs.addAll(practMap.get(V3ParticipationType.ATND));
        foundPrimaryPerformer = true;
      } else if (practMap.containsKey(V3ParticipationType.SPRF)) {
        logger.info(" Found Practitioner who is an SPRF for a Pimary Performer");
        practs.addAll(practMap.get(V3ParticipationType.SPRF));
        foundPrimaryPerformer = true;
      }

      logger.info(" Practs size = {}", practs.size());
      if (!practs.isEmpty()) {

        logger.info(" Practs is not empty, adding XML for Practitioner ");
        Practitioner pr = practs.get(0);
        practXml.append(getPractitionerXml(pr));
      } else {
        logger.info(" Practs is null or empty after author checking ");
      }
    } else {
      logger.info(" Pract Map is null or empty ");
    }

    if (!foundPrimaryPerformer) {
      logger.info(" Did not find author ");
      practXml.append(getPractitionerXml(null));
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

            sb.append(getPractitionerXml(p));

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

          logger.debug(" Found Identifier with Type MR ");

          String system =
              CdaGeneratorUtils.getRootOid(id.getSystem(), details.getAssigningAuthorityId());

          patientDetails.append(CdaGeneratorUtils.getXmlForII(system, id.getValue()));

        } else {

          logger.debug(" Using Resource Identifier as id ");

          if (addOnce) {
            patientDetails.append(
                CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), p.getId()));
            addOnce = false;
          }
        }
      }

    } else {
      logger.info(" Using Resource Identifier as id ");
      patientDetails.append(
          CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), p.getId()));
    }

    // Add Address.
    patientDetails.append(CdaFhirUtilities.getAddressXml(p.getAddress()));

    // Add Telecom (Phone)
    patientDetails.append(CdaFhirUtilities.getTelecomXml(p.getTelecom()));

    // Add Telecom (Email)
    patientDetails.append(CdaFhirUtilities.getEmailXml(p.getTelecom()));

    // Add patient
    patientDetails.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.PATIENT_EL_NAME));

    patientDetails.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.NAME_EL_NAME));
    patientDetails.append(CdaFhirUtilities.getNameXml(p.getName()));
    patientDetails.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));

    patientDetails.append(CdaFhirUtilities.getGenderXml(p.getGenderElement().getValue()));
    patientDetails.append(
        CdaGeneratorUtils.getXmlForEffectiveTime(
            CdaGeneratorConstants.BIRTH_TIME_EL_NAME, p.getBirthDate()));

    if (p.getDeceased() == null || (p.getDeceased() != null && p.getDeceased().isEmpty())) {
      patientDetails.append(
          CdaGeneratorUtils.getXmlForValue(
              CdaGeneratorConstants.SDTC_DECEASED_IND, CdaGeneratorConstants.CCDA_FALSE));
    } else {
      patientDetails.append(
          CdaGeneratorUtils.getXmlForValue(
              CdaGeneratorConstants.SDTC_DECEASED_IND, CdaGeneratorConstants.CCDA_TRUE));

      if (p.getDeceased() instanceof DateTimeType) {
        DateTimeType d = (DateTimeType) p.getDeceased();
        patientDetails.append(
            CdaGeneratorUtils.getXmlForEffectiveTime(
                CdaGeneratorConstants.SDTC_DECEASED_TIME, d.getValue()));
      } else {
        patientDetails.append(
            CdaGeneratorUtils.getXmlForNullEffectiveTime(
                CdaGeneratorConstants.SDTC_DECEASED_TIME, CdaGeneratorConstants.NF_NI));
      }
    }

    Coding race =
        CdaFhirUtilities.getCodingExtension(
            p.getExtension(),
            CdaGeneratorConstants.FHIR_USCORE_RACE_EXT_URL,
            CdaGeneratorConstants.OMB_RACE_CATEGORY_URL);

    if (race != null && race.getCode() != null) {
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
              CdaGeneratorConstants.RACE_CODE_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    Coding ethnicity =
        CdaFhirUtilities.getCodingExtension(
            p.getExtension(),
            CdaGeneratorConstants.FHIR_USCORE_ETHNICITY_EXT_URL,
            CdaGeneratorConstants.OMB_RACE_CATEGORY_URL);

    if (ethnicity != null && ethnicity.getCode() != null) {
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
              CdaGeneratorConstants.ETHNIC_CODE_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    // Adding Guardian
    if (p.getContact() != null && !p.getContact().isEmpty()) {

      ContactComponent guardianContact = CdaFhirUtilities.getGuardianContact(p.getContact());

      if (guardianContact != null) {

        patientDetails.append(
            CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.GUARDIAN_EL_NAME));

        // Add address if found
        if (guardianContact.getAddress() != null) {

          logger.info(" Adding Address for Guardian");
          List<Address> addrs = new ArrayList<>();
          addrs.add(guardianContact.getAddress());
          patientDetails.append(CdaFhirUtilities.getAddressXml(addrs));
        }

        // Add Telecom
        patientDetails.append(CdaFhirUtilities.getTelecomXml(guardianContact.getTelecom()));
        patientDetails.append(CdaFhirUtilities.getEmailXml(guardianContact.getTelecom()));

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
    Coding language =
        CdaFhirUtilities.getLanguageForCodeSystem(
            p.getCommunication(), CdaGeneratorConstants.FHIR_LANGUAGE_CODESYSTEM_URL);

    if (language != null && language.getCode() != null) {
      patientDetails.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.LANGUAGE_CODE_EL_NAME, language.getCode()));
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

  public static List<Address> getAddressDetails() {
    List<Address> addrs = new ArrayList<>();
    Address addr = new Address();
    List<StringType> addrLine = new ArrayList<>();
    addrLine.add(new StringType("0987 Facility Drive"));
    addr.setLine(addrLine);
    addr.setCity("alt Lake City");
    addr.setState("UT");
    addr.setCountry("US");
    addr.setPostalCode("84101");
    addr.setUse(AddressUse.WORK);
    addrs.add(addr);

    return addrs;
  }
}
