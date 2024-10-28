package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.composite.AddressDt;
import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.composite.CodingDt;
import ca.uhn.fhir.model.dstu2.composite.ContactPointDt;
import ca.uhn.fhir.model.dstu2.composite.HumanNameDt;
import ca.uhn.fhir.model.dstu2.composite.IdentifierDt;
import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.model.dstu2.resource.Location;
import ca.uhn.fhir.model.dstu2.resource.Organization;
import ca.uhn.fhir.model.dstu2.resource.Patient;
import ca.uhn.fhir.model.dstu2.resource.Patient.Contact;
import ca.uhn.fhir.model.dstu2.resource.Practitioner;
import ca.uhn.fhir.model.dstu2.valueset.IdentifierTypeCodesEnum;
import ca.uhn.fhir.model.primitive.DateTimeDt;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.dstu2.model.BooleanType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Dstu2CdaHeaderGenerator {

  private static final Logger logger = LoggerFactory.getLogger(Dstu2CdaHeaderGenerator.class);

  public static String createCdaHeader(
      Dstu2FhirData data, LaunchDetails details, Eicr ecr, String version) {

    StringBuilder eICRHeader = new StringBuilder();

    if (data != null) {

      eICRHeader.append(CdaGeneratorUtils.getXmlHeaderForClinicalDocument(version));

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

      eICRHeader.append(
          CdaGeneratorUtils.getXmlForValue(
              CdaGeneratorConstants.VERSION_EL_NAME, Integer.toString(details.getVersionNumber())));

      Bundle bundle = data.getData();
      if (bundle != null) {

        List<Entry> entries = bundle.getEntry();

        for (Entry ent : entries) {

          // Populate Patient
          if (ent.getResource() instanceof Patient) {

            Patient p = (Patient) ent.getResource();
            eICRHeader.append(getPatientDetails(data, p, details));

            break;
          }
        }

        eICRHeader.append(getAuthorXml(data.getPractitioner(), data.getEncounter()));

        eICRHeader.append(getCustodianXml(data.getOrganization(), data.getLocation(), details));

        eICRHeader.append(getParticipantXml(details, data, data.getPatient()));

        eICRHeader.append(
            getEncompassingEncounter(
                data.getEncounter(),
                data.getPractitioner(),
                data.getLocation(),
                data.getOrganization(),
                details));
      }
    }

    return eICRHeader.toString();
  }

  private static String getParticipantXml(
      LaunchDetails details, Dstu2FhirData data, Patient patient) {
    logger.info("LaunchDetails :{} Dstu2FhirData:{} in getParticipantXml", details, data);

    StringBuilder s = new StringBuilder("");
    if (patient != null && patient.getContact() != null) {

      List<Contact> ccs = patient.getContact();

      for (Contact cc : ccs) {

        if (cc.getRelationship() != null) {

          CodingDt c = getTranslatableCodeableConceptCoding(cc.getRelationship());

          if (c != null) {
            s.append(getParticipantXml(cc, c));
          }
        }
      }
    }

    return s.toString();
  }

  public static String getParticipantXml(Contact cc, CodingDt c) {

    StringBuilder s = new StringBuilder(200);

    s.append(
        CdaGeneratorUtils.getXmlForStartElementWithTypeCode(
            CdaGeneratorConstants.PARTICIPANT_EL_NAME, CdaGeneratorConstants.TYPE_CODE_IND));

    String relationship = CdaGeneratorConstants.getCodeForContactRelationship(c.getCode());

    s.append(
        CdaGeneratorUtils.getXmlForStartElementWithClassCode(
            CdaGeneratorConstants.ASSOCIATED_ENTITY_EL_NAME, relationship));

    if (cc.getAddress() != null) {
      List<AddressDt> addrs = new ArrayList<>();
      addrs.add(cc.getAddress());
      s.append(Dstu2CdaFhirUtilities.getAddressXml(addrs));
    }

    if (cc.getTelecom() != null) {
      s.append(Dstu2CdaFhirUtilities.getTelecomXml(cc.getTelecom()));
    }

    s.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSOCIATED_PERSON_EL_NAME));
    s.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.NAME_EL_NAME));

    List<HumanNameDt> names = new ArrayList<>();
    if (cc.getName() != null) {
      names.add(cc.getName());
    }
    s.append(Dstu2CdaFhirUtilities.getNameXml(names));
    s.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));

    s.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSOCIATED_PERSON_EL_NAME));

    s.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSOCIATED_ENTITY_EL_NAME));
    s.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PARTICIPANT_EL_NAME));

    return s.toString();
  }

  public static CodingDt getTranslatableCodeableConceptCoding(List<CodeableConceptDt> cds) {

    if (cds != null) {

      for (CodeableConceptDt cd : cds) {

        List<CodingDt> codes = cd.getCoding();

        CodingDt c = getTranslatableCoding(codes);

        if (c != null) {

          return c;
        }
      }
    }

    return null;
  }

  public static CodingDt getTranslatableCoding(List<CodingDt> codes) {

    if (codes != null) {

      for (CodingDt c : codes) {

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

  public static String getPractitionerXml(Practitioner pr) {

    StringBuilder sb = new StringBuilder(500);

    if (pr != null) {

      IdentifierDt npi =
          Dstu2CdaFhirUtilities.getIdentifierForSystem(
              pr.getIdentifier(), CdaGeneratorConstants.FHIR_NPI_URL);

      if (npi != null) {
        sb.append(
            CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA, npi.getValue()));
      } else {
        sb.append(CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA));
      }

      sb.append(Dstu2CdaFhirUtilities.getAddressXml(pr.getAddress()));
      sb.append(Dstu2CdaFhirUtilities.getTelecomXml(pr.getTelecom()));

      sb.append(
          CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSIGNED_PERSON_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.NAME_EL_NAME));

      List<HumanNameDt> hns = new ArrayList<>();
      hns.add(pr.getName());
      sb.append(Dstu2CdaFhirUtilities.getNameXml(hns));

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));
      sb.append(
          CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_PERSON_EL_NAME));

    } else {

      sb.append(CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA));

      List<AddressDt> addrs = null;
      sb.append(Dstu2CdaFhirUtilities.getAddressXml(addrs));

      List<ContactPointDt> cps = null;
      sb.append(Dstu2CdaFhirUtilities.getTelecomXml(cps));

      sb.append(
          CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSIGNED_PERSON_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.NAME_EL_NAME));

      List<HumanNameDt> hns = null;
      sb.append(Dstu2CdaFhirUtilities.getNameXml(hns));

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));
      sb.append(
          CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_PERSON_EL_NAME));
    }

    return sb.toString();
  }

  public static String getLocationXml(Location loc, Organization org, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(500);

    if (loc != null) {

      IdentifierDt npi =
          Dstu2CdaFhirUtilities.getIdentifierForSystem(
              loc.getIdentifier(), CdaGeneratorConstants.FHIR_NPI_URL);

      if (npi != null) {
        sb.append(
            CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA, npi.getValue()));
      } else {
        sb.append(
            CdaGeneratorUtils.getXmlForII(
                details.getAssigningAuthorityId(), loc.getId().getValue()));
      }

      if (loc.getType() != null && loc.getType().getCoding() != null) {

        List<CodingDt> types = loc.getType().getCoding();

        String typeXml =
            Dstu2CdaFhirUtilities.getCodingXmlForCodeSystem(
                types,
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.FHIR_LOC_ROLE_CODE_TYPE_V3,
                false,
                "");
        sb.append(typeXml);
      } else {
        List<CodingDt> codes = null;
        sb.append(Dstu2CdaFhirUtilities.getCodingXml(codes, CdaGeneratorConstants.CODE_EL_NAME));
      }

      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.LOCATION_EL_NAME));

      List<AddressDt> addrs = new ArrayList<>();
      addrs.add(loc.getAddress());
      sb.append(Dstu2CdaFhirUtilities.getAddressXml(addrs));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.LOCATION_EL_NAME));

    } else if (org != null) {

      IdentifierDt npi =
          Dstu2CdaFhirUtilities.getIdentifierForSystem(
              org.getIdentifier(), CdaGeneratorConstants.FHIR_NPI_URL);

      if (npi != null) {
        sb.append(
            CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA, npi.getValue()));
      } else {
        sb.append(
            CdaGeneratorUtils.getXmlForII(
                details.getAssigningAuthorityId(), org.getId().getValue()));
      }

      if (org.getType() != null && org.getType().getCoding() != null) {

        List<CodingDt> types = org.getType().getCoding();

        String typeXml =
            Dstu2CdaFhirUtilities.getCodingXmlForCodeSystem(
                types,
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.FHIR_LOC_ROLE_CODE_TYPE_V3,
                false,
                "");
        sb.append(typeXml);
      } else {
        List<CodingDt> codes = null;
        sb.append(Dstu2CdaFhirUtilities.getCodingXml(codes, CdaGeneratorConstants.CODE_EL_NAME));
      }

      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.LOCATION_EL_NAME));

      List<AddressDt> addrs = org.getAddress();
      sb.append(Dstu2CdaFhirUtilities.getAddressXml(addrs));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.LOCATION_EL_NAME));

    } else {

      sb.append(
          CdaGeneratorUtils.getXmlForII(
              details.getAssigningAuthorityId(), CdaGeneratorConstants.UNKNOWN_VALUE));
      sb.append(
          CdaGeneratorUtils.getXmlForNullCD(
              CdaGeneratorConstants.CODE_EL_NAME, CdaGeneratorConstants.NF_NI));

      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.LOCATION_EL_NAME));
      List<AddressDt> addrs = null;
      sb.append(Dstu2CdaFhirUtilities.getAddressXml(addrs));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.LOCATION_EL_NAME));
    }

    return sb.toString();
  }

  public static String getAuthorXml(Practitioner pr, Encounter en) {

    StringBuilder sb = new StringBuilder(500);

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.AUTHOR_EL_NAME));

    if (en != null
        && en.getPeriod() != null
        && en.getPeriod().getStartElement() != null
        && en.getPeriod().getStart() != null) {
      sb.append(
          Dstu2CdaFhirUtilities.getDateTimeTypeXml(
              en.getPeriod().getStartElement(), CdaGeneratorConstants.TIME_EL_NAME));
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForEffectiveTime(
              CdaGeneratorConstants.TIME_EL_NAME, CdaGeneratorUtils.getCurrentDateTime()));
    }

    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSIGNED_AUTHOR_EL_NAME));

    sb.append(getPractitionerXml(pr));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_AUTHOR_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.AUTHOR_EL_NAME));

    return sb.toString();
  }

  public static String getOrganizationXml(
      Organization org, Location location, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(200);
    if (org != null) {

      IdentifierDt id = org.getIdentifierFirstRep();

      if (id != null && !id.isEmpty()) {
        sb.append(CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), id.getValue()));
      } else {
        sb.append(
            CdaGeneratorUtils.getXmlForII(
                details.getAssigningAuthorityId(), org.getId().getValue()));
      }

      sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.NAME_EL_NAME, org.getName()));
      sb.append(Dstu2CdaFhirUtilities.getTelecomXml(org.getTelecom()));
      sb.append(Dstu2CdaFhirUtilities.getAddressXml(org.getAddress()));

    } else if (location != null) {

      IdentifierDt id = location.getIdentifierFirstRep();

      if (id != null && !id.isEmpty()) {
        sb.append(CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), id.getValue()));
      } else {
        sb.append(
            CdaGeneratorUtils.getXmlForII(
                details.getAssigningAuthorityId(), location.getId().getValue()));
      }

      sb.append(
          CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.NAME_EL_NAME, location.getName()));
      sb.append(Dstu2CdaFhirUtilities.getTelecomXml(location.getTelecom()));

      List<AddressDt> addrs = new ArrayList<>();
      if (location.getAddress() != null) {
        addrs.add(location.getAddress());
      }
      sb.append(Dstu2CdaFhirUtilities.getAddressXml(addrs));

    } else {

      // Code that will replace the code above after testing.
      sb.append(CdaGeneratorUtils.getNFXMLForII(CdaGeneratorConstants.NF_NI));
      sb.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.NAME_EL_NAME, CdaGeneratorConstants.UNKNOWN_VALUE));

      List<ContactPointDt> cps = null;
      sb.append(Dstu2CdaFhirUtilities.getTelecomXml(cps));

      List<AddressDt> addrs = null;
      sb.append(Dstu2CdaFhirUtilities.getAddressXml(addrs));
    }

    return sb.toString();
  }

  public static String getCustodianXml(Organization org, Location location, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(500);

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.CUSTODIAN_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSGND_CUST_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.REP_CUST_ORG_EL_NAME));

    sb.append(getOrganizationXml(org, location, details));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.REP_CUST_ORG_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSGND_CUST_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.CUSTODIAN_EL_NAME));

    return sb.toString();
  }

  public static String getEncompassingEncounter(
      Encounter en, Practitioner pr, Location loc, Organization org, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(2000);

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMPONENT_OF_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENCOMPASSING_ENC_EL_NAME));

    if (en != null) {
      sb.append(
          CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), en.getId().getIdPart()));

      // Add all the encounter identifiers to the Ids
      List<IdentifierDt> ids = en.getIdentifier();
      if (ids != null) {

        for (IdentifierDt id : ids) {

          if (id.getSystem() != null && id.getValue() != null) {

            sb.append(
                CdaGeneratorUtils.getXmlForII(
                    CdaGeneratorUtils.getRootOid(id.getSystem(), details.getAssigningAuthorityId()),
                    id.getValue()));
          }
        }
      }

      sb.append(Dstu2CdaFhirUtilities.getEncounterClassCodeXml(en.getClassElementElement()));

      sb.append(
          Dstu2CdaFhirUtilities.getPeriodXml(
              en.getPeriod(), CdaGeneratorConstants.EFF_TIME_EL_NAME));
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

    sb.append(getPractitionerXml(pr));

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.REP_ORG_EL_NAME));

    sb.append(getOrganizationXml(org, loc, details));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.REP_ORG_EL_NAME));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_ENTITY_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.RESP_PARTY_EL_NAME));

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.LOCATION_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.HEALTHCARE_FACILITY_EL_NAME));

    sb.append(getLocationXml(loc, org, details));

    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(
            CdaGeneratorConstants.SERVICE_PROVIDER_ORG_EL_NAME));

    sb.append(getOrganizationXml(org, loc, details));

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

  public static String getPatientDetails(Dstu2FhirData data, Patient p, LaunchDetails details) {
    logger.info("Dstu2FhirData in getPatientDetails:{}", data);

    StringBuilder patientDetails = new StringBuilder();

    patientDetails.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.RECORD_TARGET_EL_NAME));
    patientDetails.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.PATIENT_ROLE_EL_NAME));

    IdentifierDt id =
        Dstu2CdaFhirUtilities.getIdentifierForType(p.getIdentifier(), IdentifierTypeCodesEnum.MR);

    if (id != null) {

      if (!StringUtils.isEmpty(id.getSystem()) && !StringUtils.isEmpty(id.getValue())) {

        logger.info(" Found Identifier with Type MR ");

        String system =
            CdaGeneratorUtils.getRootOid(id.getSystem(), details.getAssigningAuthorityId());
        patientDetails.append(CdaGeneratorUtils.getXmlForII(system, id.getValue()));
      } else {

        logger.info(" Using Resource Identifier as id ");

        patientDetails.append(
            CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), p.getId().toString()));
      }

    } else {
      logger.info(" Using Resource Identifier as id ");
      patientDetails.append(
          CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), p.getId().toString()));
    }

    // Add Address.
    patientDetails.append(Dstu2CdaFhirUtilities.getAddressXml(p.getAddress()));

    // Add Telecom
    patientDetails.append(Dstu2CdaFhirUtilities.getTelecomXml(p.getTelecom()));

    // Add patient
    patientDetails.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.PATIENT_EL_NAME));

    patientDetails.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.NAME_EL_NAME));
    patientDetails.append(Dstu2CdaFhirUtilities.getNameXml(p.getName()));
    patientDetails.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));

    patientDetails.append(Dstu2CdaFhirUtilities.getGenderXml(p.getGenderElement()));
    patientDetails.append(
        Dstu2CdaFhirUtilities.getDateTypeXml(
            p.getBirthDateElement(), CdaGeneratorConstants.BIRTH_TIME_EL_NAME));

    patientDetails.append(getDeceasedXml(p));

    CodingDt race =
        Dstu2CdaFhirUtilities.getCodingExtension(
            p.getUndeclaredExtensions(),
            CdaGeneratorConstants.FHIR_ARGO_RACE_EXT_URL,
            CdaGeneratorConstants.OMB_RACE_CATEGORY_URL);

    // Check for CodeableConceptDt with DAF extension.
    if (race == null) {
      CodeableConceptDt racedt =
          Dstu2CdaFhirUtilities.getCodeableConceptExtension(
              p.getUndeclaredExtensions(), CdaGeneratorConstants.DAF_RACE_EXT_URL);

      if (racedt != null && racedt.getCodingFirstRep() != null) {

        race = racedt.getCodingFirstRep();
      }
    }

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

    CodingDt ethnicity =
        Dstu2CdaFhirUtilities.getCodingExtension(
            p.getUndeclaredExtensions(),
            CdaGeneratorConstants.FHIR_ARGO_ETHNICITY_EXT_URL,
            CdaGeneratorConstants.OMB_RACE_CATEGORY_URL);

    // Check for CodeableConceptDt with DAF extension.
    if (ethnicity == null) {
      CodeableConceptDt ethnicitydt =
          Dstu2CdaFhirUtilities.getCodeableConceptExtension(
              p.getUndeclaredExtensions(), CdaGeneratorConstants.DAF_ETHNICITY_EXT_URL);

      if (ethnicitydt != null && ethnicitydt.getCodingFirstRep() != null) {

        ethnicity = ethnicitydt.getCodingFirstRep();
      }
    }

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

    // Adding Guardian details for patient
    if (p.getContact() != null && !p.getContact().isEmpty()) {

      // Add Guardian element
      Contact guardianContact = Dstu2CdaFhirUtilities.getGuardianContact(p.getContact());

      if (guardianContact != null) {

        patientDetails.append(
            CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.GUARDIAN_EL_NAME));

        // Add address
        List<AddressDt> addrs = new ArrayList<>();
        if (guardianContact.getAddress() != null) {

          addrs.add(guardianContact.getAddress());
          patientDetails.append(Dstu2CdaFhirUtilities.getAddressXml(addrs));
        } else {
          patientDetails.append(Dstu2CdaFhirUtilities.getAddressXml(addrs));
        }

        // Add Telecom
        patientDetails.append(Dstu2CdaFhirUtilities.getTelecomXml(guardianContact.getTelecom()));

        patientDetails.append(Dstu2CdaFhirUtilities.getEmailXml(guardianContact.getTelecom()));

        // Add Name
        patientDetails.append(
            CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.GUARDIAN_PERSON_EL_NAME));
        patientDetails.append(
            CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.NAME_EL_NAME));
        patientDetails.append(Dstu2CdaFhirUtilities.getNameXml(guardianContact.getName()));
        patientDetails.append(
            CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));
        patientDetails.append(
            CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.GUARDIAN_PERSON_EL_NAME));

        patientDetails.append(
            CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.GUARDIAN_EL_NAME));
      }
    }

    // Add language communication
    patientDetails.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.LANGUAGE_COMM_EL_NAME));
    CodingDt language = Dstu2CdaFhirUtilities.getLanguage(p.getCommunication());

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

  public static String getDeceasedXml(Patient p) {

    StringBuilder patientDetails = new StringBuilder(200);

    if (p.getDeceased() != null) {

      if (p.getDeceased() instanceof BooleanType
          && Boolean.TRUE.equals(((BooleanType) p.getDeceased()).getValue())) {

        patientDetails.append(
            CdaGeneratorUtils.getXmlForValue(
                CdaGeneratorConstants.SDTC_DECEASED_IND, CdaGeneratorConstants.CCDA_TRUE));

        patientDetails.append(
            CdaGeneratorUtils.getXmlForNullEffectiveTime(
                CdaGeneratorConstants.SDTC_DECEASED_TIME, CdaGeneratorConstants.NF_NI));
      } else if (p.getDeceased() instanceof DateTimeDt) {

        patientDetails.append(
            CdaGeneratorUtils.getXmlForValue(
                CdaGeneratorConstants.SDTC_DECEASED_IND, CdaGeneratorConstants.CCDA_TRUE));

        DateTimeDt d = (DateTimeDt) p.getDeceased();
        patientDetails.append(
            Dstu2CdaFhirUtilities.getDateTimeTypeXml(d, CdaGeneratorConstants.SDTC_DECEASED_TIME));
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
}
