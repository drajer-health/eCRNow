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
import ca.uhn.fhir.model.dstu2.valueset.IdentifierTypeCodesEnum;
import ca.uhn.fhir.model.dstu2.valueset.ParticipantTypeEnum;
import ca.uhn.fhir.model.primitive.BoundCodeDt;
import ca.uhn.fhir.model.primitive.CodeDt;
import ca.uhn.fhir.model.primitive.DateTimeDt;
import ca.uhn.fhir.model.primitive.StringDt;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.apache.commons.lang3.StringUtils;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Dstu2CdaFhirUtilities {

  public static final Logger logger = LoggerFactory.getLogger(Dstu2CdaFhirUtilities.class);

  public static IdentifierDt getIdentifierForType(
      List<IdentifierDt> ids, IdentifierTypeCodesEnum type) {

    if (ids != null && ids.size() > 0) {

      for (IdentifierDt id : ids) {

        if (id.getType() != null) {

          List<CodingDt> codings = id.getType().getCoding();

          if (codings != null && codings.size() > 0) {

            for (CodingDt coding : codings) {

              if (coding.getSystem() != null
                  && coding.getSystem().contentEquals(CdaGeneratorConstants.FHIR_IDTYPE_SYSTEM)
                  && coding.getCode() != null
                  && coding.getCode().contentEquals(type.getCode())) {

                logger.info(" Found the Identifier for Patient for type " + type.toString());
                return id;
              }
            }
          }
        }
      }
    }

    logger.info(" Did not find the Identifier for the patient for type " + type.toString());
    return null;
  }

  public static IdentifierDt getIdentifierForSystem(List<IdentifierDt> ids, String system) {

    if (ids != null && ids.size() > 0) {

      for (IdentifierDt id : ids) {

        if (id.getSystem() != null && id.getSystem().contentEquals(system)) {

          logger.info(" Found the Identifier for System: " + system);
          return id;
        }
      }
    }

    logger.info(" Did not find the Identifier for  System : " + system);
    return null;
  }

  public static CodingDt getCodingExtension(
      List<ExtensionDt> exts, String extUrl, String subextUrl) {

    if (exts != null && exts.size() > 0) {

      for (ExtensionDt ext : exts) {

        if (ext.getUrl() != null && ext.getUrl().contentEquals(extUrl)) {

          // if the top level extension has CodingDt then we will use it.
          if (ext.getValue() != null && (ext.getValue() instanceof CodingDt)) {

            logger.info(" Found Extension at top level ");
            return (CodingDt) ext.getValue();

          } else if (ext.getValue() == null) {

            // get child extensions.
            List<ExtensionDt> subExts = ext.getUndeclaredExtensionsByUrl(subextUrl);

            for (ExtensionDt subext : subExts) {

              if (subext.getValue() != null && (subext.getValue() instanceof CodingDt)) {

                logger.info(" Found Extension nested as children ");
                return (CodingDt) subext.getValue();
              }
            }
          }
        }
      }
    }

    logger.info(" Did not find the Extension or sub extensions for the Url " + extUrl);
    return null;
  }

  public static CodeDt getCodeExtension(List<ExtensionDt> exts, String extUrl) {

    if (exts != null && exts.size() > 0) {

      for (ExtensionDt ext : exts) {

        if (ext.getUrl() != null && ext.getUrl().contentEquals(extUrl)) {

          // if the top level extension has CodingDt then we will use it.
          if (ext.getValue() != null && (ext.getValue() instanceof CodeDt)) {

            logger.info(" Found Extension at top level ");
            return (CodeDt) ext.getValue();
          }
        }
      }
    }

    logger.info(" Did not find the Extension or sub extensions for the Url " + extUrl);
    return null;
  }

  public static CodingDt getLanguage(List<Communication> comms) {

    if (comms != null && comms.size() > 0) {

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

    if (addrs != null && addrs.size() > 0) {

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

    // logger.info(" Address String = " + addrString.toString());
    return addrString.toString();
  }

  public static String getTelecomXml(List<ContactPointDt> tels) {

    StringBuilder telString = new StringBuilder(200);

    if (tels != null && tels.size() > 0) {

      for (ContactPointDt tel : tels) {

        if (tel.getSystem() != null
            && tel.getSystemElement().getValueAsEnum() == ContactPointSystemEnum.PHONE
            && !StringUtils.isEmpty(tel.getValue())) {

          logger.info(" Found Telcom Number ");
          telString.append(
              CdaGeneratorUtils.getXmlForTelecom(
                  CdaGeneratorConstants.TEL_EL_NAME,
                  tel.getValue(),
                  CdaGeneratorConstants.getCodeForTelecomUse(tel.getUse())));

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

    if (tels != null && tels.size() > 0) {

      for (ContactPointDt tel : tels) {

        if (tel.getSystem() != null
            && tel.getSystemElement().getValueAsEnum() == ContactPointSystemEnum.EMAIL
            && !StringUtils.isEmpty(tel.getValue())) {

          logger.info(" Found Email  ");
          telString.append(
              CdaGeneratorUtils.getXmlForTelecom(
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
    if (contactList != null && contactList.size() > 0) {
      for (Contact contact : contactList) {
        if (contact.getRelationship() != null && contact.getRelationship().size() > 0) {
          for (CodeableConceptDt code : contact.getRelationship()) {
            if (code.getText() != null
                && (code.getText().equalsIgnoreCase(CdaGeneratorConstants.GUARDIAN_EL_NAME)
                    || code.getText()
                        .equalsIgnoreCase(CdaGeneratorConstants.GUARDIAN_PERSON_EL_NAME))) {
              return contact;
            }
          }
        }
      }
    }
    return null;
  }

  /*public static void populateEntriesForEncounter(Bundle bundle, LaunchDetails details, Encounter en, Practitioner pr, Location loc, Organization org) {

  	List<Entry> entries = bundle.getEntry();
  	for(Entry ent : entries) {

  		// Populate Patient
  		if((ent.getResource() instanceof Encounter) &&
  		   (details.getEncounterId().contentEquals(CdaGeneratorConstants.UNKNOWN_VALUE) ||
  		    ent.getResource().getId().getIdPart().contentEquals(details.getEncounterId()))) {

  			en = (Encounter)ent.getResource();

  			logger.info(" Found Encounter for Id:  " + details.getEncounterId() + " Resource Id : "+ en.getId().getIdPart());

  			// For this encounter extract the other resources.
  			pr = getPractitioner(entries, en);
  			loc = getLocation(entries, en);
  			org = getOrganization(entries, en);
  		}
  	}
  }*/

  public static Organization getOrganization(List<Entry> entries, Encounter en) {

    if (!en.getServiceProvider().getReference().hasIdPart()) {

      Entry ent =
          getResourceEntryForId(
              en.getServiceProvider().getReference().getIdPart(), "Organization", entries);

      if (ent != null) {

        logger.info(
            " Found organization for Id " + en.getServiceProvider().getReference().getIdPart());
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

        logger.info(" Found Location for Id " + loc.getLocation().getReference().getIdPart());
        return (Location) ent.getResource();
      }
    }

    logger.info(" Did not find the location resource for encounter ");
    return null;
  }

  public static Practitioner getPractitioner(List<Entry> entries, Encounter en) {

    List<Participant> participants = en.getParticipant();

    if (participants != null && participants.size() > 0) {

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
                      " Found Practitioner for Id "
                          + part.getIndividual().getReference().getIdPart());
                  return (Practitioner) ent.getResource();
                } else {
                  logger.info(
                      " Did not find the practitioner for : "
                          + part.getIndividual().getReference().getIdPart());
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

        logger.info(" Found entry for ID " + id + " Type : " + type);
        return ent;
      }
    }

    logger.info(" Did not find entry for ID " + id + " Type : " + type);
    return null;
  }

  public static String getCodeableConceptXml(
      List<CodeableConceptDt> cds, String cdName, Boolean valueTrue) {

    StringBuilder sb = new StringBuilder(500);
    List<CodingDt> codes = new ArrayList<CodingDt>();

    if (cds != null && cds.size() > 0) {

      CodeableConceptDt cd = cds.get(0);

      List<CodingDt> codings = cd.getCoding();

      if (codings != null && codings.size() > 0) {

        Boolean found = false;
        Boolean first = true;

        for (CodingDt code : codings) {

          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(code.getSystem());

          if (!StringUtils.isEmpty(csd.getValue0())) codes.add(code);
        }
      }
    }

    if (!valueTrue) sb.append(getCodingXml(codes, cdName));
    else sb.append(getCodingXmlForValue(codes, cdName));

    return sb.toString();
  }

  public static String getCodingXml(List<CodingDt> codes, String cdName) {

    StringBuilder sb = new StringBuilder(200);

    if (codes != null && codes.size() > 0) {

      Boolean first = true;
      for (CodingDt c : codes) {

        if (first) {

          first = false;
          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());
          sb.append(
              CdaGeneratorUtils.getXmlForCDWithoutEndTag(
                  cdName, c.getCode(), csd.getValue0(), csd.getValue1(), c.getDisplay()));
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

    if (codes.size() > 0) {

      Boolean first = true;
      for (CodingDt c : codes) {

        if (first) {

          first = false;
          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());
          sb.append(
              CdaGeneratorUtils.getXmlForValueCDWithoutEndTag(
                  c.getCode(), csd.getValue0(), csd.getValue1(), c.getDisplay()));
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

  public static String getPeriodXml(PeriodDt period, String elName) {

    StringBuilder sb = new StringBuilder(200);

    if (period != null) {

      sb.append(CdaGeneratorUtils.getXmlForStartElement(elName));
      sb.append(
          CdaGeneratorUtils.getXmlForEffectiveTime(
              CdaGeneratorConstants.TIME_LOW_EL_NAME, period.getStart()));
      sb.append(
          CdaGeneratorUtils.getXmlForEffectiveTime(
              CdaGeneratorConstants.TIME_HIGH_EL_NAME, period.getEnd()));
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

    if (names != null && names.size() > 0) {

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

      logger.info(" Printing the class name " + dt.getClass());

      String val = "";
      if (dt instanceof CodingDt) {
        CodingDt cd = (CodingDt) dt;

        if (cd.getCodeElement() != null && cd.getSystemElement() != null) {

          val +=
              cd.getSystemElement().getValue()
                  + CdaGeneratorConstants.PIPE
                  + cd.getCodeElement().getValue();
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

    if (dt != null) {

      logger.info(" Printing the class name " + dt.getClass());

      String val = "";
      if (dt instanceof CodingDt) {
        CodingDt cd = (CodingDt) dt;

        List<CodingDt> cds = new ArrayList<CodingDt>();
        cds.add(cd);
        if (!valFlag) val += getCodingXml(cds, elName);
        else val += getCodingXmlForValue(cds, elName);

      } else if (dt instanceof CodeableConceptDt) {

        CodeableConceptDt cd = (CodeableConceptDt) dt;

        List<CodingDt> cds = cd.getCoding();

        if (!valFlag) val += getCodingXml(cds, elName);
        else val += getCodingXmlForValue(cds, elName);

      } else if (dt instanceof QuantityDt) {

        QuantityDt qt = (QuantityDt) dt;

        val += getQuantityXml(qt, elName, valFlag);

      } else if (dt instanceof DateTimeDt) {

        DateTimeDt d = (DateTimeDt) dt;

        val += CdaGeneratorUtils.getXmlForEffectiveTime(elName, d.getValue());

      } else if (dt instanceof PeriodDt) {
        PeriodDt pt = (PeriodDt) dt;

        val += getPeriodXml(pt, elName);
      } else if (dt instanceof CodeDt) {

        CodeDt cd = (CodeDt) dt;

        if (!valFlag) val += CdaGeneratorUtils.getXmlForNullCD(elName, CdaGeneratorConstants.NF_NI);
        else val += CdaGeneratorUtils.getNFXMLForValue(CdaGeneratorConstants.NF_NI);
      }

      return val;
    }
    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  // public CodeableConcept getMedication(MedicationAdmininstration med, )

}
