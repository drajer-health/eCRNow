package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.api.ExtensionDt;
import ca.uhn.fhir.model.api.IDatatype;
import ca.uhn.fhir.model.base.composite.BaseQuantityDt;
import ca.uhn.fhir.model.dstu2.composite.AddressDt;
import ca.uhn.fhir.model.dstu2.composite.BoundCodeableConceptDt;
import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.composite.CodingDt;
import ca.uhn.fhir.model.dstu2.composite.ContactPointDt;
import ca.uhn.fhir.model.dstu2.composite.HumanNameDt;
import ca.uhn.fhir.model.dstu2.composite.IdentifierDt;
import ca.uhn.fhir.model.dstu2.composite.PeriodDt;
import ca.uhn.fhir.model.dstu2.composite.QuantityDt;
import ca.uhn.fhir.model.dstu2.composite.TimingDt;
import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.model.dstu2.resource.Encounter.Participant;
import ca.uhn.fhir.model.dstu2.resource.Location;
import ca.uhn.fhir.model.dstu2.resource.Organization;
import ca.uhn.fhir.model.dstu2.resource.Patient.Communication;
import ca.uhn.fhir.model.dstu2.resource.Patient.Contact;
import ca.uhn.fhir.model.dstu2.resource.Practitioner;
import ca.uhn.fhir.model.dstu2.valueset.AddressUseEnum;
import ca.uhn.fhir.model.dstu2.valueset.AdministrativeGenderEnum;
import ca.uhn.fhir.model.dstu2.valueset.ContactPointSystemEnum;
import ca.uhn.fhir.model.dstu2.valueset.EncounterClassEnum;
import ca.uhn.fhir.model.dstu2.valueset.IdentifierTypeCodesEnum;
import ca.uhn.fhir.model.dstu2.valueset.ParticipantTypeEnum;
import ca.uhn.fhir.model.primitive.BoundCodeDt;
import ca.uhn.fhir.model.primitive.CodeDt;
import ca.uhn.fhir.model.primitive.DateDt;
import ca.uhn.fhir.model.primitive.DateTimeDt;
import ca.uhn.fhir.model.primitive.InstantDt;
import ca.uhn.fhir.model.primitive.StringDt;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import java.util.*;
import org.apache.commons.lang3.StringUtils;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Dstu2CdaFhirUtilities {

  public static final Logger logger = LoggerFactory.getLogger(Dstu2CdaFhirUtilities.class);

  public static IdentifierDt getIdentifierForType(
      List<IdentifierDt> ids, IdentifierTypeCodesEnum type) {

    if (ids != null && !ids.isEmpty()) {

      for (IdentifierDt id : ids) {

        if (id.getType() != null) {

          List<CodingDt> codings = id.getType().getCoding();

          if (codings != null && !codings.isEmpty()) {

            for (CodingDt coding : codings) {

              if (coding.getSystem() != null
                  && coding.getSystem().contentEquals(CdaGeneratorConstants.FHIR_IDTYPE_SYSTEM)
                  && coding.getCode() != null
                  && coding.getCode().contentEquals(type.getCode())) {

                logger.info(" Found the Identifier for Patient for type :{}", type);
                return id;
              }
            }
          }
        }
      }
    }

    logger.info(" Did not find the Identifier for the patient for type :" + type.toString());

    return null;
  }

  public static IdentifierDt getIdentifierForSystem(List<IdentifierDt> ids, String system) {

    if (StringUtils.isBlank(system)) {

      logger.info("Skipping fetching identifier due to null or blank system");
      return null;
    }
    if (ids != null && !ids.isEmpty()) {

      for (IdentifierDt id : ids) {

        if (id.getSystem() != null && system != null && id.getSystem().contentEquals(system)) {

          logger.info(" Found the Identifier for System:{} ", system);

          return id;
        }
      }
    }

    return null;
  }

  public static CodeableConceptDt getCodeableConceptExtension(
      List<ExtensionDt> exts, String extUrl) {

    if (exts != null && !exts.isEmpty()) {

      for (ExtensionDt ext : exts) {

        if (ext.getUrl() != null && ext.getUrl().contentEquals(extUrl)) {
          logger.info("Extension URl :{}", ext.getUrl());

          // if the top level extension has CodingDt then we will use it.
          if ((ext.getValue() instanceof CodeableConceptDt)) {

            logger.info("Found Extension at top level");
            return (CodeableConceptDt) ext.getValue();
          }
        }
      }
    }

    return null;
  }

  public static CodingDt getCodingExtension(
      List<ExtensionDt> exts, String extUrl, String subextUrl) {

    if (exts != null && !exts.isEmpty()) {

      for (ExtensionDt ext : exts) {

        if (ext.getUrl() != null && ext.getUrl().contentEquals(extUrl)) {

          // if the top level extension has CodingDt then we will use it.
          if (ext.getValue() instanceof CodingDt) {

            logger.info(" Found Extension at top level ");
            return (CodingDt) ext.getValue();

          } else if (ext.getValue() == null) {

            // get child extensions.
            List<ExtensionDt> subExts = ext.getUndeclaredExtensionsByUrl(subextUrl);

            for (ExtensionDt subext : subExts) {

              if ((subext.getValue() instanceof CodingDt)) {

                logger.info(" Found Extension nested as children ");
                return (CodingDt) subext.getValue();
              }
            }
          }
        }
      }
    }

    logger.info(" Did not find the Extension or sub extensions for the Url:{} ", extUrl);

    return null;
  }

  public static CodeDt getCodeExtension(List<ExtensionDt> exts, String extUrl) {

    if (exts != null && !exts.isEmpty()) {

      for (ExtensionDt ext : exts) {

        if (ext.getUrl() != null && ext.getUrl().contentEquals(extUrl)) {
          logger.info("Extention url exist.");

          // if the top level extension has CodingDt then we will use it.
          if ((ext.getValue() instanceof CodeDt)) {

            logger.info(" Found Extension at top level ");
            return (CodeDt) ext.getValue();
          }
        }
      }
    }

    logger.info(" Did not find the Extension or sub extensions for the Url:{}", extUrl);
    return null;
  }

  public static CodingDt getLanguage(List<Communication> comms) {

    if (comms != null && !comms.isEmpty()) {

      for (Communication comm : comms) {

        if (comm.getLanguage() != null
            && comm.getLanguage().getCodingFirstRep() != null
            && comm.getLanguage().getCodingFirstRep().getCode() != null) {

          return comm.getLanguage().getCodingFirstRep();
        }
      }
    }

    logger.info(" Did not find the communication language ");
    return null;
  }

  public static String getAddressXml(List<AddressDt> addrs) {

    StringBuilder addrString = new StringBuilder(200);

    if (addrs != null && !addrs.isEmpty()) {

      for (AddressDt addr : addrs) {

        if (addr.getUseElement().getValueAsEnum() == AddressUseEnum.HOME
            || addr.getUseElement().getValueAsEnum() == AddressUseEnum.WORK) {

          logger.info(" Found Home or Work Address ");

          addrString.append(
              CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ADDR_EL_NAME));

          // Address Line
          if (!StringUtils.isEmpty(addr.getLineFirstRep().getValueAsString())) {
            addrString.append(
                CdaGeneratorUtils.getXmlForText(
                    CdaGeneratorConstants.ST_ADDR_LINE_EL_NAME, addr.getLineFirstRep().getValue()));
          } else {
            addrString.append(
                CdaGeneratorUtils.getXmlForNFText(
                    CdaGeneratorConstants.ST_ADDR_LINE_EL_NAME, CdaGeneratorConstants.NF_NI));
          }

          // City
          if (!StringUtils.isEmpty(addr.getCity())) {
            addrString.append(
                CdaGeneratorUtils.getXmlForText(
                    CdaGeneratorConstants.CITY_EL_NAME, addr.getCity()));
          } else {
            addrString.append(
                CdaGeneratorUtils.getXmlForNFText(
                    CdaGeneratorConstants.CITY_EL_NAME, CdaGeneratorConstants.NF_NI));
          }

          // State
          if (!StringUtils.isEmpty(addr.getState())) {
            addrString.append(
                CdaGeneratorUtils.getXmlForText(
                    CdaGeneratorConstants.STATE_EL_NAME, addr.getState()));
          } else {
            addrString.append(
                CdaGeneratorUtils.getXmlForNFText(
                    CdaGeneratorConstants.STATE_EL_NAME, CdaGeneratorConstants.NF_NI));
          }

          // Postal Code
          if (!StringUtils.isEmpty(addr.getPostalCode())) {
            addrString.append(
                CdaGeneratorUtils.getXmlForText(
                    CdaGeneratorConstants.POSTAL_CODE_EL_NAME, addr.getPostalCode()));
          } else {
            addrString.append(
                CdaGeneratorUtils.getXmlForNFText(
                    CdaGeneratorConstants.POSTAL_CODE_EL_NAME, CdaGeneratorConstants.NF_NI));
          }

          // Country
          if (!StringUtils.isEmpty(addr.getCountry())) {
            addrString.append(
                CdaGeneratorUtils.getXmlForText(
                    CdaGeneratorConstants.COUNTRY_EL_NAME, addr.getCountry()));
          } else {
            addrString.append(
                CdaGeneratorUtils.getXmlForNFText(
                    CdaGeneratorConstants.COUNTRY_EL_NAME, CdaGeneratorConstants.NF_NI));
          }

          addrString.append(
              CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ADDR_EL_NAME));

          break;
        }
      }
    } else {

      logger.info(" Did not find the Address ");
      addrString.append(
          CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ADDR_EL_NAME));

      addrString.append(
          CdaGeneratorUtils.getXmlForNFText(
              CdaGeneratorConstants.ST_ADDR_LINE_EL_NAME, CdaGeneratorConstants.NF_NI));
      addrString.append(
          CdaGeneratorUtils.getXmlForNFText(
              CdaGeneratorConstants.CITY_EL_NAME, CdaGeneratorConstants.NF_NI));
      addrString.append(
          CdaGeneratorUtils.getXmlForNFText(
              CdaGeneratorConstants.STATE_EL_NAME, CdaGeneratorConstants.NF_NI));
      addrString.append(
          CdaGeneratorUtils.getXmlForNFText(
              CdaGeneratorConstants.POSTAL_CODE_EL_NAME, CdaGeneratorConstants.NF_NI));
      addrString.append(
          CdaGeneratorUtils.getXmlForNFText(
              CdaGeneratorConstants.COUNTRY_EL_NAME, CdaGeneratorConstants.NF_NI));

      addrString.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ADDR_EL_NAME));
    }

    return addrString.toString();
  }

  public static String getTelecomXml(List<ContactPointDt> tels) {

    StringBuilder telString = new StringBuilder(200);

    if (tels != null && !tels.isEmpty()) {

      for (ContactPointDt tel : tels) {

        if (tel.getSystem() != null
            && tel.getSystemElement().getValueAsEnum() == ContactPointSystemEnum.PHONE
            && !StringUtils.isEmpty(tel.getValue())) {

          logger.info(" Found Telcom Number ");
          telString.append(
              CdaGeneratorUtils.getXmlForTelecom(
                  CdaGeneratorConstants.TEL_EL_NAME,
                  tel.getValue(),
                  CdaGeneratorConstants.getCodeForTelecomUse(tel.getUse()),
                  false));

          break;
        }
      }
    } else {

      logger.info(" Did not find the Telecom ");
      telString.append(
          CdaGeneratorUtils.getXmlForNFText(
              CdaGeneratorConstants.TEL_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    return telString.toString();
  }

  public static String getEmailXml(List<ContactPointDt> tels) {

    StringBuilder telString = new StringBuilder(200);

    if (tels != null && !tels.isEmpty()) {

      for (ContactPointDt tel : tels) {

        if (tel.getSystem() != null
            && tel.getSystemElement().getValueAsEnum() == ContactPointSystemEnum.EMAIL
            && !StringUtils.isEmpty(tel.getValue())) {

          logger.info(" Found Email  ");
          telString.append(
              CdaGeneratorUtils.getXmlForEmail(
                  CdaGeneratorConstants.TEL_EL_NAME,
                  tel.getValue(),
                  CdaGeneratorConstants.getCodeForTelecomUse(tel.getUse())));

          break;
        }
      }
    } else {

      logger.info(" Did not find the Email ");
      telString.append(
          CdaGeneratorUtils.getXmlForNFText(
              CdaGeneratorConstants.TEL_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    return telString.toString();
  }

  public static Contact getGuardianContact(List<Contact> contactList) {
    if (contactList != null && !contactList.isEmpty()) {
      for (Contact contact : contactList) {
        if (contact.getRelationship() != null && !contact.getRelationship().isEmpty()) {
          for (CodeableConceptDt code : contact.getRelationship()) {

            if (code.getText() != null
                && (code.getText().equalsIgnoreCase(CdaGeneratorConstants.GUARDIAN_EL_NAME)
                    || code.getText()
                        .equalsIgnoreCase(CdaGeneratorConstants.GUARDIAN_PERSON_EL_NAME))) {
              return contact;
            } else {
              List<CodingDt> cs = code.getCoding();

              for (CodingDt c : cs) {

                if (c.getSystem() != null
                    && (c.getSystem()
                            .contentEquals(
                                CdaGeneratorConstants.FHIR_CONTACT_RELATIONSHIP_CODESYSTEM)
                        || c.getSystem()
                            .contentEquals(
                                CdaGeneratorConstants.DSTU2_FHIR_CONTACT_RELATIONSHIP_CODESYSTEM))
                    && c.getCode() != null
                    && (c.getCode().contentEquals(CdaGeneratorConstants.GUARDIAN_VALUE)
                        || c.getCode().contentEquals(CdaGeneratorConstants.GUARDIAN_EL_NAME)
                        || c.getCode().contentEquals(CdaGeneratorConstants.EMERGENCY_VALUE)
                        || c.getCode().contentEquals(CdaGeneratorConstants.GUARDIAN_PERSON_EL_NAME)
                        || c.getCode().contentEquals(CdaGeneratorConstants.FHIR_GUARDIAN_VALUE)
                        || c.getCode()
                            .contentEquals(CdaGeneratorConstants.FHIR_EMERGENCY_CONTACT_VALUE))) {
                  return contact;
                }
              }
            }
          }
        }
      }
    }
    return null;
  }

  public static Organization getOrganization(List<Entry> entries, Encounter en) {

    if (en.getServiceProvider().getReference().hasIdPart()) {

      Entry ent =
          getResourceEntryForId(
              en.getServiceProvider().getReference().getIdPart(), "Organization", entries);

      if (ent != null) {

        logger.info(
            " Found organization for Id:{} ", en.getServiceProvider().getReference().getIdPart());

        return (Organization) ent.getResource();
      }
    }

    logger.info(" Did not find the organization resource for encounter ");
    return null;
  }

  public static Location getLocation(List<Entry> entries, Encounter en) {

    Encounter.Location loc = en.getLocationFirstRep();

    if (loc.getLocation().getReference().hasIdPart()) {

      Entry ent =
          getResourceEntryForId(loc.getLocation().getReference().getIdPart(), "Location", entries);

      if (ent != null) {

        logger.info(" Found Location for Id {}", loc.getLocation().getReference().getIdPart());
        return (Location) ent.getResource();
      }
    }

    logger.info(" Did not find the location resource for encounter ");
    return null;
  }

  public static Practitioner getPractitioner(List<Entry> entries, Encounter en) {

    List<Participant> participants = en.getParticipant();

    if (participants != null && !participants.isEmpty()) {

      for (Participant part : participants) {

        if (part.getIndividual().getReference().hasIdPart()) {

          logger.info(" Individual is present ");

          List<BoundCodeableConceptDt<ParticipantTypeEnum>> types = part.getType();

          for (BoundCodeableConceptDt<ParticipantTypeEnum> conc : types) {

            List<CodingDt> typeCodes = conc.getCoding();

            for (CodingDt cd : typeCodes) {

              if (cd.getCode().contentEquals(ParticipantTypeEnum.PPRF.getCode())) {

                // Found the participant.
                // Look for the Practitioner.

                Entry ent =
                    getResourceEntryForId(
                        part.getIndividual().getReference().getIdPart(), "Practitioner", entries);

                if (ent != null) {

                  logger.info(
                      " Found Practitioner for Id :{}",
                      part.getIndividual().getReference().getIdPart());
                  return (Practitioner) ent.getResource();
                } else {
                  logger.info(
                      " Did not find the practitioner for : {}",
                      part.getIndividual().getReference().getIdPart());
                }
              }
            }
          }
        }
      }
    }

    logger.info(" Did not find the practitioner for encounter ");
    return null;
  }

  public static Entry getResourceEntryForId(String id, String type, List<Entry> entries) {

    for (Entry ent : entries) {

      if (ent.getResource() != null
          &&
          //  ent.getResource() != null &&
          //   ent.getResource().fhirType().contentEquals(type) &&
          ent.getResource().getId() != null
          && ent.getResource().getId().getIdPart().contentEquals(id)) {

        logger.info(" Found entry for ID : {} Type : {}", id, type);
        return ent;
      }
    }

    logger.info(" Did not find entry for ID {}  Type :{} ", id, type);
    return null;
  }

  public static String getCodeableConceptXml(
      List<CodeableConceptDt> cds, String cdName, Boolean valueTrue) {

    StringBuilder sb = new StringBuilder(500);
    List<CodingDt> codes = new ArrayList<>();

    if (cds != null && !cds.isEmpty()) {

      CodeableConceptDt cd = cds.get(0);

      List<CodingDt> codings = cd.getCoding();

      if (codings != null && !codings.isEmpty()) {

        Boolean found = false;
        Boolean first = true;

        for (CodingDt code : codings) {

          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(code.getSystem());

          if (!StringUtils.isEmpty(csd.getValue0())) {
            codes.add(code);
          } else {
            Pair<String, String> csd2 =
                CdaGeneratorConstants.getCodeSystemFromUrlForDstu2(code.getSystem());

            if (!StringUtils.isEmpty(csd2.getValue0())) codes.add(code);
          }
        }
      }
    }

    if (Boolean.FALSE.equals(valueTrue)) sb.append(getCodingXml(codes, cdName));
    else sb.append(getCodingXmlForValue(codes, cdName));

    return sb.toString();
  }

  public static String getCodingXmlForCodeSystem(
      List<CodingDt> codes,
      String cdName,
      String codeSystemUrl,
      Boolean csOptional,
      String contentRef) {

    StringBuilder sb = new StringBuilder(200);
    StringBuilder translations = new StringBuilder(200);

    Boolean foundCodeForCodeSystem = false;

    if (codes != null && !codes.isEmpty()) {

      for (CodingDt c : codes) {

        Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

        if (!csd.getValue0().isEmpty()
            && c.getSystem().contentEquals(codeSystemUrl)
            && Boolean.TRUE.equals(!foundCodeForCodeSystem)) {

          logger.debug("Found the Coding for Codesystem {}", codeSystemUrl);
          sb.append(
              CdaGeneratorUtils.getXmlForCDWithoutEndTag(
                  cdName, c.getCode(), csd.getValue0(), csd.getValue1(), c.getDisplay()));

          if (!contentRef.isEmpty())
            sb.append(CdaGeneratorUtils.getXmlForOriginalTextWithReference(contentRef));

          foundCodeForCodeSystem = true;
        } else if (!csd.getValue0().isEmpty() && !csd.getValue1().isEmpty()) {

          logger.debug(
              "Found the Coding for a different Codesystem {} for Translation ", csd.getValue0());
          translations.append(
              CdaGeneratorUtils.getXmlForCD(
                  CdaGeneratorConstants.TRANSLATION_EL_NAME,
                  c.getCode(),
                  csd.getValue0(),
                  csd.getValue1(),
                  c.getDisplay()));
        } else {
          logger.debug(
              " Did not find the code system mapping from FHIR to CDA for {}", c.getSystem());
        }
      }

      // At least one code is there so...close the tag
      if (Boolean.FALSE.equals(foundCodeForCodeSystem)) {

        // If we dont find the preferred code system, then add NF of OTH along with translations.
        sb.append(
            CdaGeneratorUtils.getXmlForNullCDWithoutEndTag(cdName, CdaGeneratorConstants.NF_OTH));
      }

      logger.debug(" Sb = {}", sb);
      sb.append(translations);
      sb.append(CdaGeneratorUtils.getXmlForEndElement(cdName));

    } else {
      sb.append(CdaGeneratorUtils.getXmlForNullCD(cdName, CdaGeneratorConstants.NF_NI));
    }

    if (Boolean.TRUE.equals(foundCodeForCodeSystem) || (!csOptional)) {
      return sb.toString();
    } else {
      return new StringBuilder("").toString();
    }
  }

  public static String getCodingXml(List<CodingDt> codes, String cdName) {

    StringBuilder sb = new StringBuilder(200);

    if (codes != null && !codes.isEmpty()) {

      Boolean first = true;
      for (CodingDt c : codes) {

        if (Boolean.TRUE.equals(first)) {

          first = false;
          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

          if (csd != null && (csd.getValue0().isEmpty() || csd.getValue1().isEmpty())) {
            logger.debug(" Try using the DSTU2 map ");
            csd = CdaGeneratorConstants.getCodeSystemFromUrlForDstu2(c.getSystem());
          }
          if (csd != null) {

            sb.append(
                CdaGeneratorUtils.getXmlForCDWithoutEndTag(
                    cdName, c.getCode(), csd.getValue0(), csd.getValue1(), c.getDisplay()));
          }
        } else {

          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());
          sb.append(
              CdaGeneratorUtils.getXmlForCD(
                  CdaGeneratorConstants.TRANSLATION_EL_NAME,
                  c.getCode(),
                  csd.getValue0(),
                  csd.getValue1(),
                  c.getDisplay()));
        }
      }

      // At least one code is there so...close the tag
      sb.append(CdaGeneratorUtils.getXmlForEndElement(cdName));
    } else {
      sb.append(CdaGeneratorUtils.getXmlForNullCD(cdName, CdaGeneratorConstants.NF_NI));
    }

    return sb.toString();
  }

  public static String getCodingXmlForValue(List<CodingDt> codes, String cdName) {

    StringBuilder sb = new StringBuilder(200);

    if (!codes.isEmpty()) {

      Boolean first = true;
      for (CodingDt c : codes) {

        if (Boolean.TRUE.equals(first)) {

          first = false;
          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

          if (csd != null && (csd.getValue0().isEmpty() || csd.getValue1().isEmpty())) {
            logger.debug(" Try using the DSTU2 map ");
            csd = CdaGeneratorConstants.getCodeSystemFromUrlForDstu2(c.getSystem());
          }
          if (csd != null) {
            sb.append(
                CdaGeneratorUtils.getXmlForValueCDWithoutEndTag(
                    c.getCode(), csd.getValue0(), csd.getValue1(), c.getDisplay()));
          }

        } else {

          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());
          sb.append(
              CdaGeneratorUtils.getXmlForCD(
                  CdaGeneratorConstants.TRANSLATION_EL_NAME,
                  c.getCode(),
                  csd.getValue0(),
                  csd.getValue1(),
                  c.getDisplay()));
        }
      }

      // At least one code is there so...close the tag
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.VAL_EL_NAME));
    } else {
      sb.append(CdaGeneratorUtils.getXmlForNullValueCD(cdName, CdaGeneratorConstants.NF_NI));
    }

    return sb.toString();
  }

  public static String getPeriodXmlForValueElement(PeriodDt dt, String elName) {

    if (dt != null) {

      DateTimeDt start = null;
      DateTimeDt end = null;

      if (dt.getStartElement() != null && dt.getStartElement().hasValue()) {
        start = dt.getStartElement();
      }
      if (dt.getEndElement() != null && dt.getEndElement().hasValue()) {
        end = dt.getEndElement();
      }
      return getEffectiveTimeXmlForValueElement(start, end, elName);

    } else {

      String start = "";
      String end = "";
      return CdaGeneratorUtils.getXmlForValueIVLWithTS(elName, start, end);
    }
  }

  public static String getEffectiveTimeXmlForValueElement(
      DateTimeDt start, DateTimeDt end, String elName) {

    String retXml = "";
    if (start != null && end != null) {

      Date s = start.getValue();
      TimeZone stz = start.getTimeZone();

      String startStr = CdaGeneratorUtils.getStringForDateTime(s, stz);

      Date e = end.getValue();
      TimeZone etz = end.getTimeZone();

      String endStr = CdaGeneratorUtils.getStringForDateTime(e, etz);

      retXml = CdaGeneratorUtils.getXmlForValueIVLWithTS(elName, startStr, endStr);

    } else if (start != null) {

      Date s = start.getValue();
      TimeZone stz = start.getTimeZone();

      String startStr = CdaGeneratorUtils.getStringForDateTime(s, stz);

      String endStr = "";

      retXml = CdaGeneratorUtils.getXmlForValueIVLWithTS(elName, startStr, endStr);

    } else if (end != null) {

      String startStr = "";

      Date e = end.getValue();
      TimeZone etz = end.getTimeZone();

      String endStr = CdaGeneratorUtils.getStringForDateTime(e, etz);

      retXml = CdaGeneratorUtils.getXmlForValueIVLWithTS(elName, startStr, endStr);

    } else {

      String startStr = "";
      String endStr = "";
      retXml = CdaGeneratorUtils.getXmlForValueIVLWithTS(elName, startStr, endStr);
    }

    return retXml;
  }

  public static String getDateTimeTypeXml(DateTimeDt dt, String elName) {

    if (dt != null) {

      return CdaGeneratorUtils.getXmlForEffectiveTime(elName, dt.getValue(), dt.getTimeZone());
    } else {
      return CdaGeneratorUtils.getXmlForEffectiveTime(elName, null, null);
    }
  }

  public static String getDateTypeXml(DateDt dt, String elName) {

    if (dt != null) {

      return CdaGeneratorUtils.getXmlForEffectiveTime(elName, dt.getValue(), null);
    } else {
      return CdaGeneratorUtils.getXmlForEffectiveTime(elName, null, null);
    }
  }

  public static String getDisplayStringForDateTimeType(DateTimeDt dt) {

    if (dt != null) {

      return CdaGeneratorUtils.getStringForDateTime(dt.getValue(), dt.getTimeZone());
    } else {
      return CdaGeneratorConstants.UNKNOWN_VALUE;
    }
  }

  public static Date getDateForDataType(IDatatype dateType) {

    Date effectiveDateTime = null;

    if (dateType instanceof DateDt) {

      DateDt eDate = (DateDt) dateType;
      effectiveDateTime = eDate.getValue();
    } else if (dateType instanceof DateTimeDt) {
      DateTimeDt eDate = (DateTimeDt) dateType;
      effectiveDateTime = eDate.getValue();
    } else if (dateType instanceof PeriodDt) {
      PeriodDt pDate = (PeriodDt) dateType;

      if (pDate.getStart() != null) effectiveDateTime = pDate.getStart();
      else if (pDate.getEnd() != null) effectiveDateTime = pDate.getEnd();
    }

    return effectiveDateTime;
  }

  public static String getPeriodXml(PeriodDt period, String elName) {

    StringBuilder sb = new StringBuilder(200);

    if (period != null) {

      sb.append(CdaGeneratorUtils.getXmlForStartElement(elName));
      sb.append(
          Dstu2CdaFhirUtilities.getDateTimeTypeXml(
              period.getStartElement(), CdaGeneratorConstants.TIME_LOW_EL_NAME));
      sb.append(
          Dstu2CdaFhirUtilities.getDateTimeTypeXml(
              period.getEndElement(), CdaGeneratorConstants.TIME_HIGH_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(elName));

    } else {
      sb.append(CdaGeneratorUtils.getXmlForNullEffectiveTime(elName, CdaGeneratorConstants.NF_NI));
    }

    return sb.toString();
  }

  public static String getQuantityXml(QuantityDt dt, String elName, Boolean valFlag) {

    StringBuilder sb = new StringBuilder(200);

    if (dt != null) {

      sb.append(
          CdaGeneratorUtils.getXmlForQuantity(
              elName, dt.getValue().toString(), dt.getUnit(), valFlag));

    } else {
      sb.append(CdaGeneratorUtils.getXmlForNullValuePQ(CdaGeneratorConstants.NF_NI));
    }

    return sb.toString();
  }

  public static String getEncounterClassCodeXml(BoundCodeDt<EncounterClassEnum> encClass) {

    String s = "";

    if (encClass != null
        && (encClass.getValueAsEnum() == EncounterClassEnum.AMBULATORY
            || encClass.getValueAsEnum() == EncounterClassEnum.OUTPATIENT)) {

      s +=
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.CDA_AMBULATORY_ENCOUNTER_CLASS,
              CdaGeneratorConstants.ACT_CODE_SYSTEM);
    } else if (encClass != null && (encClass.getValueAsEnum() == EncounterClassEnum.INPATIENT)) {

      s +=
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.CDA_INPATIENT_ENCOUNTER_CLASS,
              CdaGeneratorConstants.ACT_CODE_SYSTEM);
    } else if (encClass != null && (encClass.getValueAsEnum() == EncounterClassEnum.EMERGENCY)) {

      s +=
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.CDA_EMERGENCY_ENCOUNTER_CLASS,
              CdaGeneratorConstants.ACT_CODE_SYSTEM);
    } else {

      logger.info(" Did not find the encounter class for the patient ");
      s +=
          CdaGeneratorUtils.getXmlForNullCD(
              CdaGeneratorConstants.CODE_EL_NAME, CdaGeneratorConstants.NF_NI);
    }

    return s;
  }

  public static String getGenderXml(BoundCodeDt<AdministrativeGenderEnum> gender) {

    String s = "";

    if (gender != null && (gender.getValueAsEnum() == AdministrativeGenderEnum.MALE)) {

      s +=
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.ADMIN_GENDER_CODE_EL_NAME,
              CdaGeneratorConstants.CDA_MALE_CODE,
              CdaGeneratorConstants.ADMIN_GEN_CODE_SYSTEM);
    } else if (gender != null && (gender.getValueAsEnum() == AdministrativeGenderEnum.FEMALE)) {

      s +=
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.ADMIN_GENDER_CODE_EL_NAME,
              CdaGeneratorConstants.CDA_FEMALE_CODE,
              CdaGeneratorConstants.ADMIN_GEN_CODE_SYSTEM);
    } else if (gender != null) {

      s +=
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.ADMIN_GENDER_CODE_EL_NAME,
              CdaGeneratorConstants.CDA_UNK_GENDER,
              CdaGeneratorConstants.ADMIN_GEN_CODE_SYSTEM);
    } else {

      logger.info(" Did not find the gender for the patient ");
      s +=
          CdaGeneratorUtils.getXmlForNullCD(
              CdaGeneratorConstants.ADMIN_GENDER_CODE_EL_NAME, CdaGeneratorConstants.NF_NI);
    }

    return s;
  }

  public static String getNameXml(List<HumanNameDt> names) {

    StringBuilder nameString = new StringBuilder(200);

    if (names != null && !names.isEmpty()) {

      Optional<HumanNameDt> hName = names.stream().findFirst();
      if (hName.isPresent()) {
        HumanNameDt name = hName.get();

        List<StringDt> ns = name.getGiven();

        for (StringDt n : ns) {

          if (!StringUtils.isEmpty(n.getValue()))
            nameString.append(
                CdaGeneratorUtils.getXmlForText(
                    CdaGeneratorConstants.FIRST_NAME_EL_NAME, name.getGivenFirstRep().getValue()));
        }

        // If Empty create NF
        if (StringUtils.isEmpty(nameString)) {
          nameString.append(
              CdaGeneratorUtils.getXmlForNFText(
                  CdaGeneratorConstants.FIRST_NAME_EL_NAME, CdaGeneratorConstants.NF_NI));
        }

        if (name.getFamilyFirstRep() != null
            && !StringUtils.isEmpty(name.getFamilyFirstRep().getValue())) {
          nameString.append(
              CdaGeneratorUtils.getXmlForText(
                  CdaGeneratorConstants.LAST_NAME_EL_NAME, name.getFamilyFirstRep().getValue()));
        } else {
          nameString.append(
              CdaGeneratorUtils.getXmlForNFText(
                  CdaGeneratorConstants.LAST_NAME_EL_NAME, CdaGeneratorConstants.NF_NI));
        }
      }
    } else {

      logger.info(" Did not find the Name for the patient ");
      nameString.append(
          CdaGeneratorUtils.getXmlForNFText(
              CdaGeneratorConstants.FIRST_NAME_EL_NAME, CdaGeneratorConstants.NF_NI));
      nameString.append(
          CdaGeneratorUtils.getXmlForNFText(
              CdaGeneratorConstants.LAST_NAME_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    return nameString.toString();
  }

  public static String getNameXml(HumanNameDt name) {

    StringBuilder nameString = new StringBuilder(200);
    if (name != null) {

      List<StringDt> ns = name.getGiven();

      for (StringDt n : ns) {

        if (!StringUtils.isEmpty(n.getValue()))
          nameString.append(
              CdaGeneratorUtils.getXmlForText(
                  CdaGeneratorConstants.FIRST_NAME_EL_NAME, name.getGivenFirstRep().getValue()));
      }

      // If Empty create NF
      if (StringUtils.isEmpty(nameString)) {
        nameString.append(
            CdaGeneratorUtils.getXmlForNFText(
                CdaGeneratorConstants.FIRST_NAME_EL_NAME, CdaGeneratorConstants.NF_NI));
      }

      if (name.getFamilyFirstRep() != null
          && !StringUtils.isEmpty(name.getFamilyFirstRep().getValue())) {
        nameString.append(
            CdaGeneratorUtils.getXmlForText(
                CdaGeneratorConstants.LAST_NAME_EL_NAME, name.getFamilyFirstRep().getValue()));
      } else {
        nameString.append(
            CdaGeneratorUtils.getXmlForNFText(
                CdaGeneratorConstants.LAST_NAME_EL_NAME, CdaGeneratorConstants.NF_NI));
      }
    }

    return nameString.toString();
  }

  public static String getStringForIDataType(IDatatype dt) {

    if (dt != null) {

      logger.info(" Printing the class name:{} ", dt.getClass());

      String val = "";
      if (dt instanceof CodingDt) {
        CodingDt cd = (CodingDt) dt;

        if (!StringUtils.isEmpty(cd.getDisplay())) {

          val += cd.getDisplay();
        } else if (cd.getCodeElement() != null && cd.getSystemElement() != null) {

          val +=
              cd.getSystemElement().getValue()
                  + CdaGeneratorConstants.PIPE
                  + cd.getCodeElement().getValue();
        } else {
          val += CdaGeneratorConstants.UNKNOWN_VALUE;
        }

      } else if (dt instanceof CodeableConceptDt) {

        CodeableConceptDt cd = (CodeableConceptDt) dt;

        if (!StringUtils.isEmpty(cd.getText())) {
          val += cd.getText();
        } else if (cd.getCodingFirstRep() != null) {

          if (!StringUtils.isEmpty(cd.getCodingFirstRep().getDisplay())) {

            val += cd.getCodingFirstRep().getDisplay();
          } else if (cd.getCodingFirstRep().getCodeElement() != null
              && cd.getCodingFirstRep().getSystemElement() != null) {

            val +=
                cd.getCodingFirstRep().getSystemElement().getValue()
                    + CdaGeneratorConstants.PIPE
                    + cd.getCodingFirstRep().getCodeElement().getValue();
          } else {
            val += CdaGeneratorConstants.UNKNOWN_VALUE;
          }
        } else {
          val += CdaGeneratorConstants.UNKNOWN_VALUE;
        }

      } else if (dt instanceof BaseQuantityDt) {

        QuantityDt qt = (QuantityDt) dt;

        if (qt.getValueElement() != null && qt.getSystemElement() != null && qt.getUnit() != null) {

          val +=
              qt.getValueElement().getValueAsString()
                  + CdaGeneratorConstants.PIPE
                  + qt.getSystemElement().getValueAsString()
                  + CdaGeneratorConstants.PIPE
                  + qt.getUnit();
        }

      } else if (dt instanceof DateDt) {

        DateDt d = (DateDt) dt;
        val += d.getValueAsString();

      } else if (dt instanceof DateTimeDt) {

        DateTimeDt d = (DateTimeDt) dt;

        val += d.getValueAsString();

      } else if (dt instanceof PeriodDt) {
        PeriodDt pt = (PeriodDt) dt;

        if (pt.getStart() != null && pt.getEnd() != null) {
          val += pt.getStart().toString() + CdaGeneratorConstants.PIPE + pt.getEnd().toString();
        } else if (pt.getStart() != null) {
          val += pt.getStart().toString();
        }
      } else if (dt instanceof CodeDt) {

        CodeDt cd = (CodeDt) dt;

        val += cd.getValue();
      }

      return val;
    }
    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  public static String getIDataTypeXml(IDatatype dt, String elName, Boolean valFlag) {

    String val = "";
    if (dt != null) {

      logger.info(" Printing the class name:{} ", dt.getClass());

      if (dt instanceof CodingDt) {
        CodingDt cd = (CodingDt) dt;

        List<CodingDt> cds = new ArrayList<>();
        cds.add(cd);
        if (Boolean.FALSE.equals(valFlag)) val += getCodingXml(cds, elName);
        else val += getCodingXmlForValue(cds, elName);

      } else if (dt instanceof CodeableConceptDt) {

        CodeableConceptDt cd = (CodeableConceptDt) dt;

        List<CodingDt> cds = cd.getCoding();

        if (Boolean.FALSE.equals(valFlag)) val += getCodingXml(cds, elName);
        else val += getCodingXmlForValue(cds, elName);

      } else if (dt instanceof QuantityDt) {

        QuantityDt qt = (QuantityDt) dt;

        val += getQuantityXml(qt, elName, valFlag);

      } else if (dt instanceof DateTimeDt) {

        DateTimeDt d = (DateTimeDt) dt;

        val += CdaGeneratorUtils.getXmlForEffectiveTime(elName, d.getValue(), d.getTimeZone());

      } else if (dt instanceof PeriodDt) {
        PeriodDt pt = (PeriodDt) dt;

        val += getPeriodXml(pt, elName);
      } else if (dt instanceof CodeDt) {

        CodeDt cd = (CodeDt) dt;
        logger.info("CodeDt:{}", cd);

        if (Boolean.FALSE.equals(valFlag))
          val += CdaGeneratorUtils.getXmlForNullCD(elName, CdaGeneratorConstants.NF_NI);
        else val += CdaGeneratorUtils.getNFXMLForValue(CdaGeneratorConstants.NF_NI);
      } else if (dt instanceof StringDt) {

        StringDt st = (StringDt) dt;
        if (Boolean.FALSE.equals(valFlag))
          val += CdaGeneratorUtils.getXmlForText(elName, st.getValue());
        else val += CdaGeneratorUtils.getXmlForValueString(st.getValue());
      }

      return val;
    }

    if (Boolean.FALSE.equals(valFlag))
      val += CdaGeneratorUtils.getNFXMLForElement(elName, CdaGeneratorConstants.NF_NI);
    else val += CdaGeneratorUtils.getNFXmlForValueString(CdaGeneratorConstants.NF_NI);

    return val;
  }

  public static Pair<String, Boolean> getCodeableConceptDisplayForCodeSystem(
      List<CodeableConceptDt> cds, String codeSystemUrl, Boolean csOptional) {

    Pair<String, Boolean> disp = null;
    CodeableConceptDt cd = null;

    if (cds != null && !cds.isEmpty()) {

      cd = cds.get(0);
      disp = getCodeableConceptDisplayForCodeSystem(cd, codeSystemUrl, csOptional);
    } else {
      disp = new Pair<>("", false);
    }

    if (!StringUtils.isEmpty(disp.getValue0())) return disp;
    else if (cd != null && (csOptional || disp.getValue1()) && !StringUtils.isEmpty(cd.getText()))
      return new Pair<>(cd.getText(), disp.getValue1());
    else return new Pair<>("", disp.getValue1());
  }

  public static Pair<String, Boolean> getCodeableConceptDisplayForCodeSystem(
      CodeableConceptDt cd, String codeSystemUrl, Boolean csOptional) {

    Pair<String, Boolean> disp = null;

    if (cd != null && !cd.getCoding().isEmpty()) {

      disp = getCodingDisplayForCodeSystem(cd.getCoding(), codeSystemUrl, csOptional);
    } else {
      disp = new Pair<>("", false);
    }

    if (!StringUtils.isEmpty(disp.getValue0())) return disp;
    else if (cd != null && (csOptional || disp.getValue1()) && !StringUtils.isEmpty(cd.getText()))
      return new Pair<>(cd.getText(), disp.getValue1());
    else return new Pair<>("", disp.getValue1());
  }

  public static Pair<String, Boolean> getCodingDisplayForCodeSystem(
      List<CodingDt> codings, String codeSystemUrl, Boolean csOptional) {

    String display = "";
    String anyDisplay = "";
    Boolean foundCodeSystem = false;

    if (codings != null && !codings.isEmpty()) {
      for (CodingDt c : codings) {

        if (c.getSystem().contentEquals(codeSystemUrl) && !StringUtils.isEmpty(c.getDisplay())) {

          display = c.getDisplay();
          foundCodeSystem = true;
          break;
        } else if (c.getSystem().contentEquals(codeSystemUrl)) {
          foundCodeSystem = true;
        }

        if (Boolean.TRUE.equals(csOptional) && !StringUtils.isEmpty(c.getDisplay())) {
          anyDisplay = c.getDisplay();
        }
      }
    }

    if (!StringUtils.isEmpty(display)) return new Pair<>(display, foundCodeSystem);
    else if (!StringUtils.isEmpty(anyDisplay)) return new Pair<>(anyDisplay, foundCodeSystem);
    else return new Pair<>(display, foundCodeSystem);
  }

  public static String getMatchingCodeFromCodeableConceptForCodeSystem(
      List<String> matchedCodes, CodeableConceptDt cd, String csUrl) {

    if (cd != null && cd.getCoding() != null && !cd.getCoding().isEmpty()) {
      return getMatchingCodeFromCodingForCodeSystem(matchedCodes, cd.getCoding(), csUrl);
    }

    return "";
  }

  public static String getMatchingCodeFromCodingForCodeSystem(
      List<String> matchedCodes, List<CodingDt> cds, String csUrl) {

    if (matchedCodes != null && cds != null && !cds.isEmpty()) {

      for (CodingDt c : cds) {

        Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

        if (!csd.getValue0().isEmpty()
            && !c.getCode().isEmpty()
            && !c.getSystem().isEmpty()
            && c.getSystem().contentEquals(csUrl)
            && Boolean.TRUE.equals(isCodePresent(matchedCodes, c.getCode()))) {

          logger.debug("Found the Coding for Codesystem {} and Code =  {}", csUrl, c.getCode());

          return c.getCode();
        }
      }
    }

    return "";
  }

  public static Boolean isCodePresent(List<String> matchCodes, String code) {

    for (String cd : matchCodes) {

      if (cd.contentEquals(code)) return true;
    }

    return false;
  }

  public static Boolean isCodePresentInCoding(String matchCode, List<CodingDt> cds) {

    for (CodingDt cd : cds) {

      if (cd.getCode() != null && cd.getCode().contentEquals(matchCode)) return true;
    }

    return false;
  }

  public static String getCodeableConceptXmlForCodeSystem(
      List<CodeableConceptDt> cds,
      String cdName,
      Boolean valueTrue,
      String codeSystemUrl,
      Boolean csOptional) {

    StringBuilder sb = new StringBuilder(500);
    List<CodingDt> codes = getCodingForValidCodeSystems(cds);

    if (Boolean.FALSE.equals(valueTrue))
      sb.append(getCodingXmlForCodeSystem(codes, cdName, codeSystemUrl, csOptional, ""));
    else sb.append(getCodingXmlForValueForCodeSystem(codes, cdName, codeSystemUrl, csOptional));

    return sb.toString();
  }

  public static List<CodingDt> getCodingForValidCodeSystems(List<CodeableConceptDt> cds) {
    List<CodingDt> codes = new ArrayList<>();
    if (cds != null && !cds.isEmpty()) {

      for (CodeableConceptDt cd : cds) {

        List<CodingDt> codings = cd.getCoding();

        if (codings != null && !codings.isEmpty()) {

          logger.debug(" Coding .size = {} ", codings.size());

          for (CodingDt code : codings) {

            logger.debug(" Getting CodeSystem for Url {} ", code.getSystem());
            Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(code.getSystem());

            logger.debug(" Received CodeSystem {} and {}", csd.getValue0(), csd.getValue1());

            if (!StringUtils.isEmpty(csd.getValue0())) {
              codes.add(code);
            }
          } // for all codings
        } // codings not empy
      } // for all codeable concepts
    }
    return codes;
  }

  public static String getCodingXmlForValueForCodeSystem(
      List<CodingDt> codes, String cdName, String codeSystemUrl, Boolean csOptional) {

    StringBuilder sb = new StringBuilder(200);
    StringBuilder translations = new StringBuilder(200);

    Boolean foundCodeForCodeSystem = false;

    if (codes != null && !codes.isEmpty()) {

      for (CodingDt c : codes) {

        Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

        if (!csd.getValue0().isEmpty()
            && !csd.getValue1().isEmpty()
            && c.getSystem().contentEquals(codeSystemUrl)
            && Boolean.TRUE.equals(!foundCodeForCodeSystem)) {

          logger.debug("Found the Coding for Codesystem {}", codeSystemUrl);
          sb.append(
              CdaGeneratorUtils.getXmlForValueCDWithoutEndTag(
                  c.getCode(), csd.getValue0(), csd.getValue1(), c.getDisplay()));

          foundCodeForCodeSystem = true;
        } else if (!csd.getValue0().isEmpty() && !csd.getValue1().isEmpty()) {

          logger.debug("Found the Coding for a different Codesystem {}", csd.getValue0());
          translations.append(
              CdaGeneratorUtils.getXmlForCD(
                  CdaGeneratorConstants.TRANSLATION_EL_NAME,
                  c.getCode(),
                  csd.getValue0(),
                  csd.getValue1(),
                  c.getDisplay()));
        }
      }

      // At least one code is there so...close the tag
      if (Boolean.FALSE.equals(foundCodeForCodeSystem)) {

        // If we dont find the preferred code system, then add NF of OTH along with translations.
        sb.append(
            CdaGeneratorUtils.getXmlForNullValueCDWithoutEndTag(
                cdName, CdaGeneratorConstants.NF_OTH));
      }

      sb.append(translations);
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.VAL_EL_NAME));

    } else {
      sb.append(CdaGeneratorUtils.getXmlForNullValueCD(cdName, CdaGeneratorConstants.NF_NI));
    }

    if (Boolean.TRUE.equals(foundCodeForCodeSystem) || (!csOptional)) {
      return sb.toString();
    } else {
      return new StringBuilder("").toString();
    }
  }

  public static Pair<Date, TimeZone> getActualDate(IDatatype dt) {

    Date d = null;
    TimeZone t = null;
    if (dt instanceof DateTimeDt) {

      DateTimeDt d1 = (DateTimeDt) dt;
      d = d1.getValue();
      t = d1.getTimeZone();

    } else if (dt instanceof PeriodDt) {

      logger.debug("Found an instance of period");
      PeriodDt d1 = (PeriodDt) dt;

      if (d1.getStartElement() != null) {

        d = d1.getStart();
        t = d1.getStartElement().getTimeZone();
      } else if (d1.getEndElement() != null) {
        d = d1.getEnd();
        t = d1.getEndElement().getTimeZone();
      }

    } else if (dt instanceof InstantDt) {

      InstantDt d1 = (InstantDt) dt;
      d = d1.getValue();
      t = d1.getTimeZone();

    } else if (dt instanceof TimingDt) {

      logger.debug(" Found an instance of timing ");
      TimingDt tmg = (TimingDt) (dt);
      if (tmg.getRepeat() != null && tmg.getRepeat().getBounds() != null) {

        logger.debug(" Found the bounds element ");
        return getActualDate(tmg.getRepeat().getBounds());
      }
    }

    return new Pair<>(d, t);
  }

  public static String getStringForType(IDatatype dt) {

    if (dt != null) {

      StringBuilder val = new StringBuilder();
      if (dt instanceof CodingDt) {
        CodingDt cd = (CodingDt) dt;

        val.append(getStringForCoding(cd));

      } else if (dt instanceof CodeableConceptDt) {

        CodeableConceptDt cd = (CodeableConceptDt) dt;

        if (!StringUtils.isEmpty(cd.getText())) {
          val.append(cd.getText());
        } else {
          List<CodingDt> cds = cd.getCoding();
          Boolean first = true;

          for (CodingDt c : cds) {

            if (Boolean.FALSE.equals(first)) {

              val.append(CdaGeneratorConstants.SPACE)
                  .append(CdaGeneratorConstants.PIPE)
                  .append(CdaGeneratorConstants.SPACE);
            }
            first = false;
            val.append(getStringForCoding(c));
          }
        }

      } else if (dt instanceof QuantityDt) {

        QuantityDt qt = (QuantityDt) dt;

        val.append(getStringForQuantity(qt));

      } else if (dt instanceof DateTimeDt) {

        DateTimeDt d = (DateTimeDt) dt;

        val.append(d.getValueAsString());

      } else if (dt instanceof TimingDt) {

        logger.debug("Found an instance of timing for creating string ");
        TimingDt t = (TimingDt) (dt);
        if (t.getRepeat() != null && t.getRepeat().getBounds() != null) {

          logger.debug("Found the bounds element for creating string ");

          String v = getStringForType(t.getRepeat().getBounds());
          val.append(v);
        }

      } else if (dt instanceof PeriodDt) {
        PeriodDt pt = (PeriodDt) dt;

        logger.debug("Found the Period element for creating string");
        if (pt.getStart() != null && pt.getEnd() != null) {
          val.append(pt.getStart().toString())
              .append(CdaGeneratorConstants.PIPE)
              .append(pt.getEnd().toString());
        } else if (pt.getStart() != null) {
          val.append(pt.getStart().toString());
        } else {
          val.append(CdaGeneratorConstants.UNKNOWN_VALUE);
        }
      } else if (dt instanceof CodeDt) {

        CodeDt cd = (CodeDt) dt;

        val.append(cd.getValue());
      } else if (dt instanceof StringDt) {

        StringDt st = (StringDt) dt;

        val.append(st.getValue());
      }

      logger.debug("Printing the class name {} and value {}", dt.getClass(), val);
      return val.toString();
    }
    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  public static String getStringForCoding(CodingDt cd) {

    String val = "";
    if (cd != null) {

      if (!StringUtils.isEmpty(cd.getDisplay())) {
        val += cd.getDisplay();
      } else if (cd.getCodeElement() != null && cd.getSystemElement() != null) {

        val +=
            cd.getSystemElement().getValue()
                + CdaGeneratorConstants.PIPE
                + cd.getCodeElement().getValue();
      }
    }

    return val;
  }

  public static String getStringForQuantity(QuantityDt qt) {

    String val = "";

    if (qt != null
        && qt.getValueElement() != null
        && qt.getSystemElement() != null
        && qt.getUnit() != null) {

      val +=
          qt.getValueElement().getValueAsString()
              + CdaGeneratorConstants.PIPE
              + qt.getSystemElement().getValueAsString()
              + CdaGeneratorConstants.PIPE
              + qt.getUnit();
    } else {
      val += CdaGeneratorConstants.UNKNOWN_VALUE;
    }

    return val;
  }

  public static String getXmlForType(IDatatype dt, String elName, Boolean valFlag) {

    String val = "";
    if (dt != null) {

      if (dt instanceof CodingDt) {
        CodingDt cd = (CodingDt) dt;

        List<CodingDt> cds = new ArrayList<>();
        cds.add(cd);
        if (Boolean.FALSE.equals(valFlag)) val += getCodingXml(cds, elName);
        else val += getCodingXmlForValue(cds, elName);

      } else if (dt instanceof CodeableConceptDt) {

        CodeableConceptDt cd = (CodeableConceptDt) dt;

        List<CodingDt> cds = cd.getCoding();

        if (Boolean.FALSE.equals(valFlag)) val += getCodingXml(cds, elName);
        else val += getCodingXmlForValue(cds, elName);

      } else if (dt instanceof QuantityDt) {

        QuantityDt qt = (QuantityDt) dt;

        val += getQuantityXml(qt, elName, valFlag);

      } else if (dt instanceof DateTimeDt) {

        DateTimeDt d = (DateTimeDt) dt;

        val += CdaGeneratorUtils.getXmlForEffectiveTime(elName, d.getValue(), d.getTimeZone());

      } else if (dt instanceof PeriodDt) {
        PeriodDt pt = (PeriodDt) dt;

        val += getPeriodXml(pt, elName);
      } else if (dt instanceof TimingDt) {

        TimingDt t = (TimingDt) (dt);
        if (t.getRepeat() != null && t.getRepeat().getBounds() != null) {

          logger.debug("Found the bounds element for creating xml");

          String v = getXmlForType(t.getRepeat().getBounds(), elName, valFlag);
          val += v;
        }
      } else if (dt instanceof CodeDt) {

        CodeDt cd = (CodeDt) dt;
        if (Boolean.FALSE.equals(valFlag))
          val += CdaGeneratorUtils.getXmlForCD(elName, cd.getValue());
        else val += CdaGeneratorUtils.getXmlForValueString(cd.getValue());
      } else if (dt instanceof StringDt) {

        StringDt st = (StringDt) dt;
        if (Boolean.FALSE.equals(valFlag))
          val += CdaGeneratorUtils.getXmlForText(elName, st.getValue());
        else val += CdaGeneratorUtils.getXmlForValueString(st.getValue());
      }

      logger.debug("Printing the class name {}", dt.getClass());
      return val;
    }

    if (Boolean.FALSE.equals(valFlag))
      val += CdaGeneratorUtils.getNFXMLForElement(elName, CdaGeneratorConstants.NF_NI);
    else val += CdaGeneratorUtils.getNFXmlForValueString(CdaGeneratorConstants.NF_NI);

    return val;
  }

  public static String getCombinationStringForCodeSystem(
      CodeableConceptDt code, IDatatype value, String codeSystemUrl, Boolean csOptional) {

    Pair<String, Boolean> codeString = new Pair<>("", false);
    if (code != null) {

      codeString = getCodeableConceptDisplayForCodeSystem(code, codeSystemUrl, csOptional);
    }

    Pair<String, Boolean> valueString = new Pair<>("", false);
    if (value instanceof CodeableConceptDt) {

      CodeableConceptDt vCode = (CodeableConceptDt) value;
      valueString = getCodeableConceptDisplayForCodeSystem(vCode, codeSystemUrl, csOptional);
    } else if (value instanceof CodingDt) {
      CodingDt vCd = (CodingDt) value;
      List<CodingDt> cds = new ArrayList<>();
      cds.add(vCd);
      valueString = getCodingDisplayForCodeSystem(cds, codeSystemUrl, csOptional);
    } else if (value instanceof StringDt) {
      StringDt st = (StringDt) value;
      valueString.setAt0(st.getValue());
    }

    return codeString.getValue0() + CdaGeneratorConstants.HYPHEN + valueString.getValue0();
  }

  public static Boolean isCodingPresentForCodeSystem(List<CodingDt> codings, String codeSystemUrl) {

    Boolean foundCodeSystem = false;

    for (CodingDt c : codings) {

      if (c.getSystem().contentEquals(codeSystemUrl)) {

        foundCodeSystem = true;
        break;
      }
    }

    return foundCodeSystem;
  }

  public static String getXmlForTypeForCodeSystem(
      IDatatype dt, String elName, Boolean valFlag, String codeSystemUrl, Boolean csOptional) {

    String val = "";
    if (dt != null) {

      if (dt instanceof CodingDt) {
        CodingDt cd = (CodingDt) dt;

        List<CodingDt> cds = new ArrayList<>();
        cds.add(cd);
        if (Boolean.FALSE.equals(valFlag))
          val += getCodingXmlForCodeSystem(cds, elName, codeSystemUrl, csOptional, "");
        else val += getCodingXmlForValueForCodeSystem(cds, elName, codeSystemUrl, csOptional);

      } else if (dt instanceof CodeableConceptDt) {

        CodeableConceptDt cd = (CodeableConceptDt) dt;

        List<CodingDt> cds = cd.getCoding();

        if (Boolean.FALSE.equals(valFlag))
          val += getCodingXmlForCodeSystem(cds, elName, codeSystemUrl, csOptional, "");
        else val += getCodingXmlForValueForCodeSystem(cds, elName, codeSystemUrl, csOptional);

      } else {

        if (Boolean.FALSE.equals(valFlag))
          val += CdaGeneratorUtils.getXmlForNullCD(elName, CdaGeneratorConstants.NF_NI);
        else val += CdaGeneratorUtils.getNFXMLForValue(CdaGeneratorConstants.NF_NI);
      }

      logger.debug("Printing the class name {}", dt.getClass());
      return val;
    }

    if (Boolean.FALSE.equals(valFlag))
      val += CdaGeneratorUtils.getXmlForNullCD(elName, CdaGeneratorConstants.NF_NI);
    else val += CdaGeneratorUtils.getNFXMLForValue(CdaGeneratorConstants.NF_NI);

    return val;
  }
}
