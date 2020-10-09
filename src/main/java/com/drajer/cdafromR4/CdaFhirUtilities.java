package com.drajer.cdafromR4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Address.AddressUse;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Encounter.EncounterLocationComponent;
import org.hl7.fhir.r4.model.Encounter.EncounterParticipantComponent;
import org.hl7.fhir.r4.model.Enumerations.AdministrativeGender;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Patient.ContactComponent;
import org.hl7.fhir.r4.model.Patient.PatientCommunicationComponent;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.Type;
import org.hl7.fhir.r4.model.codesystems.V3ParticipationType;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaFhirUtilities {

  public static final Logger logger = LoggerFactory.getLogger(CdaFhirUtilities.class);

  public static Identifier getIdentifierForType(List<Identifier> ids, String type) {

    if (ids != null && ids.size() > 0) {

      for (Identifier id : ids) {

        if (id.getType() != null) {

          List<Coding> codings = id.getType().getCoding();

          if (codings != null && codings.size() > 0) {

            for (Coding coding : codings) {

              if (coding.getSystem() != null
                  && (coding
                          .getSystem()
                          .contentEquals(CdaGeneratorConstants.FHIR_IDENTIFIER_TYPE_SYSTEM)
                      || coding.getSystem().contentEquals(CdaGeneratorConstants.FHIR_IDTYPE_SYSTEM))
                  && coding.getCode() != null
                  && coding.getCode().contentEquals(type)) {

                logger.info(" Found the Identifier for Patient for type " + type);
                return id;
              }
            }
          }
        }
      }
    }

    logger.info(" Did not find the Identifier for the patient for type " + type);
    return null;
  }

  public static Patient.ContactComponent getGuardianContact(List<ContactComponent> ccs) {

    if (ccs != null && ccs.size() > 0) {

      for (ContactComponent cc : ccs) {

        if (cc.getRelationship() != null && cc.getRelationship().size() > 0) {

          for (CodeableConcept cd : cc.getRelationship()) {

            if (cd.getText() != null
                && (cd.getText().equalsIgnoreCase(CdaGeneratorConstants.GUARDIAN_EL_NAME)
                    || cd.getText()
                        .equalsIgnoreCase(CdaGeneratorConstants.GUARDIAN_PERSON_EL_NAME))) {

              return cc;
            }
          }
        }
      }
    }

    return null;
  }

  public static Identifier getIdentifierForSystem(List<Identifier> ids, String system) {

    if (ids != null && ids.size() > 0) {

      for (Identifier id : ids) {

        if (id.getSystem() != null && id.getSystem().contentEquals(system)) {

          logger.info(" Found the Identifier for System: " + system);
          return id;
        }
      }
    }

    logger.info(" Did not find the Identifier for  System : " + system);
    return null;
  }

  public static Coding getCodingExtension(List<Extension> exts, String extUrl, String subextUrl) {

    if (exts != null && exts.size() > 0) {

      for (Extension ext : exts) {

        if (ext.getUrl() != null && ext.getUrl().contentEquals(extUrl)) {

          // if the top level extension has Coding then we will use it.
          if (ext.getValue() != null && (ext.getValue() instanceof Coding)) {

            logger.info(" Found Extension at top level ");
            return (Coding) ext.getValue();

          } else if (ext.getValue() == null) {

            // get child extensions.
            List<Extension> subExts = ext.getExtensionsByUrl(subextUrl);

            for (Extension subext : subExts) {

              if (subext.getValue() != null && (subext.getValue() instanceof Coding)) {

                logger.info(" Found Extension nested as children ");
                return (Coding) subext.getValue();
              }
            }
          }
        }
      }
    }

    logger.info(" Did not find the Extension or sub extensions for the Url " + extUrl);
    return null;
  }

  public static CodeType getCodeExtension(List<Extension> exts, String extUrl) {

    if (exts != null && exts.size() > 0) {

      for (Extension ext : exts) {

        if (ext.getUrl() != null && ext.getUrl().contentEquals(extUrl)) {

          // if the top level extension has CodingDt then we will use it.
          if (ext.getValue() != null && (ext.getValue() instanceof CodeType)) {

            logger.info(" Found Extension at top level ");
            return (CodeType) ext.getValue();
          }
        }
      }
    }

    logger.info(" Did not find the Extension or sub extensions for the Url " + extUrl);
    return null;
  }

  public static Coding getLanguage(List<PatientCommunicationComponent> comms) {

    if (comms != null && comms.size() > 0) {

      for (PatientCommunicationComponent comm : comms) {

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

  public static String getAddressXml(List<Address> addrs) {

    StringBuilder addrString = new StringBuilder(200);

    if (addrs != null && addrs.size() > 0) {

      for (Address addr : addrs) {

        if (addr.getUseElement().getValue() == AddressUse.HOME
            || addr.getUseElement().getValue() == AddressUse.WORK) {

          logger.info(" Found Home or Work Address ");

          addrString.append(
              CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ADDR_EL_NAME));

          // Address Line
          List<StringType> lines = addr.getLine();

          if (lines != null && lines.size() > 0) {
            addrString.append(
                CdaGeneratorUtils.getXmlForText(
                    CdaGeneratorConstants.ST_ADDR_LINE_EL_NAME, lines.get(0).getValue()));
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

  public static String getTelecomXml(List<ContactPoint> tels) {

    StringBuilder telString = new StringBuilder(200);

    if (tels != null && tels.size() > 0) {

      for (ContactPoint tel : tels) {

        if (tel.getSystem() != null
            && tel.getSystem() == ContactPoint.ContactPointSystem.PHONE
            && !StringUtils.isEmpty(tel.getValue())) {

          logger.info(" Found Telcom Number ");
          telString.append(
              CdaGeneratorUtils.getXmlForTelecom(
                  CdaGeneratorConstants.TEL_EL_NAME,
                  tel.getValue(),
                  CdaGeneratorConstants.getCodeForTelecomUse(tel.getUse().toCode())));

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

  public static String getEmailXml(List<ContactPoint> tels) {

    StringBuilder telString = new StringBuilder(200);

    if (tels != null && tels.size() > 0) {

      for (ContactPoint tel : tels) {

        if (tel.getSystem() != null
            && tel.getSystem() == ContactPoint.ContactPointSystem.EMAIL
            && !StringUtils.isEmpty(tel.getValue())) {

          logger.info(" Found Email ");
          telString.append(
              CdaGeneratorUtils.getXmlForTelecom(
                  CdaGeneratorConstants.TEL_EL_NAME,
                  tel.getValue(),
                  CdaGeneratorConstants.getCodeForTelecomUse(tel.getUse().toCode())));

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

  /*public static void populateEntriesForEncounter(Bundle bundle, LaunchDetails details, Encounter en, Practitioner pr, Location loc, Organization org) {

  	List<BundleEntryComponent> entries = bundle.getEntry();
  	for(BundleEntryComponent ent : entries) {

  		// Populate Patient
  		if((ent.getResource() instanceof Encounter) &&
  		   (details.getEncounterId().contentEquals(CdaGeneratorConstants.UNKNOWN_VALUE) ||
  		    ent.getResource().getId().contentEquals(details.getEncounterId()))) {

  			en = (Encounter)ent.getResource();

  			logger.info(" Found Encounter for Id:  " + details.getEncounterId() + " Resource Id : "+ en.getId());

  			// For this encounter extract the other resources.
  			pr = getPractitioner(entries, en);
  			loc = getLocation(entries, en);
  			org = getOrganization(entries, en);
  		}
  	}
  }*/

  public static Organization getOrganization(List<BundleEntryComponent> entries, Encounter en) {

    if (en.getServiceProvider().getReference() != null) {

      BundleEntryComponent ent =
          getResourceEntryForId(en.getServiceProvider().getReference(), "Organization", entries);

      if (ent != null) {

        logger.info(" Found organization for Id " + en.getServiceProvider().getReference());
        return (Organization) ent.getResource();
      }
    }

    logger.info(" Did not find the organization resource for encounter ");
    return null;
  }

  public static Location getLocation(List<BundleEntryComponent> entries, Encounter en) {

    EncounterLocationComponent loc = en.getLocationFirstRep();

    if (loc != null && loc.getLocation() != null) {

      BundleEntryComponent ent =
          getResourceEntryForId(loc.getLocation().getReference(), "Location", entries);

      if (ent != null) {

        logger.info(" Found Location for Id " + loc.getLocation().getReference());
        return (Location) ent.getResource();
      }
    }

    logger.info(" Did not find the location resource for encounter ");
    return null;
  }

  public static Practitioner getPractitioner(List<BundleEntryComponent> entries, Encounter en) {

    List<EncounterParticipantComponent> participants = en.getParticipant();

    if (participants != null && participants.size() > 0) {

      for (EncounterParticipantComponent part : participants) {

        if (part.getIndividual().getReference() != null) {

          logger.info(" Individual is present ");

          List<CodeableConcept> types = part.getType();

          for (CodeableConcept conc : types) {

            List<Coding> typeCodes = conc.getCoding();

            for (Coding cd : typeCodes) {

              if (cd.getCode().contentEquals(V3ParticipationType.PPRF.toString())) {

                // Found the participant.
                // Look for the Practitioner.

                BundleEntryComponent ent =
                    getResourceEntryForId(
                        part.getIndividual().getReference(), "Practitioner", entries);

                if (ent != null) {

                  logger.info(" Found Practitioner for Id " + part.getIndividual().getReference());
                  return (Practitioner) ent.getResource();
                } else {
                  logger.info(
                      " Did not find the practitioner for : "
                          + part.getIndividual().getReference());
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

  public static BundleEntryComponent getResourceEntryForId(
      String id, String type, List<BundleEntryComponent> entries) {

    for (BundleEntryComponent ent : entries) {

      if (ent.getResource() != null
          &&
          //  ent.getResource() != null &&
          //   ent.getResource().fhirType().contentEquals(type) &&
          ent.getResource().getId() != null
          && ent.getResource().getId().contentEquals(id)) {

        logger.info(" Found entry for ID " + id + " Type : " + type);
        return ent;
      }
    }

    logger.info(" Did not find entry for ID " + id + " Type : " + type);
    return null;
  }

  public static String getCodeableConceptXml(
      List<CodeableConcept> cds, String cdName, Boolean valueTrue) {

    StringBuilder sb = new StringBuilder(500);
    List<Coding> codes = new ArrayList<Coding>();

    if (cds != null && cds.size() > 0) {

      CodeableConcept cd = cds.get(0);

      List<Coding> codings = cd.getCoding();

      if (codings != null && codings.size() > 0) {

        Boolean found = false;
        Boolean first = true;

        for (Coding code : codings) {

          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(code.getSystem());

          if (!StringUtils.isEmpty(csd.getValue0())) codes.add(code);
        }
      }
    }

    if (!valueTrue) sb.append(getCodingXml(codes, cdName));
    else sb.append(getCodingXmlForValue(codes, cdName));

    return sb.toString();
  }

  public static String getCodingXml(List<Coding> codes, String cdName) {

    StringBuilder sb = new StringBuilder(200);

    if (codes != null && codes.size() > 0) {

      Boolean first = true;
      for (Coding c : codes) {

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

  public static String getCodingXmlForValue(List<Coding> codes, String cdName) {

    StringBuilder sb = new StringBuilder(200);

    if (codes.size() > 0) {

      Boolean first = true;
      for (Coding c : codes) {

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

  public static String getPeriodXml(Period period, String elName) {

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

  public static String getQuantityXml(Quantity dt, String elName, Boolean valFlag) {

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

  public static String getGenderXml(AdministrativeGender gender) {

    String s = "";

    if (gender != null && (gender == AdministrativeGender.MALE)) {

      s +=
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.ADMIN_GENDER_CODE_EL_NAME,
              CdaGeneratorConstants.CDA_MALE_CODE,
              CdaGeneratorConstants.ADMIN_GEN_CODE_SYSTEM);
    } else if (gender != null && (gender == AdministrativeGender.FEMALE)) {

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

  public static String getNameXml(List<HumanName> names) {

    StringBuilder nameString = new StringBuilder(200);

    if (names != null && names.size() > 0) {

      HumanName name = names.stream().findFirst().get();
      List<StringType> ns = name.getGiven();

      for (StringType n : ns) {

        if (!StringUtils.isEmpty(n.getValue()))
          nameString.append(
              CdaGeneratorUtils.getXmlForText(
                  CdaGeneratorConstants.FIRST_NAME_EL_NAME, n.getValue()));
      }

      // If Empty create NF
      if (StringUtils.isEmpty(nameString)) {
        nameString.append(
            CdaGeneratorUtils.getXmlForNFText(
                CdaGeneratorConstants.FIRST_NAME_EL_NAME, CdaGeneratorConstants.NF_NI));
      }

      if (name.getFamily() != null && !StringUtils.isEmpty(name.getFamily())) {
        nameString.append(
            CdaGeneratorUtils.getXmlForText(
                CdaGeneratorConstants.LAST_NAME_EL_NAME, name.getFamily()));
      } else {
        nameString.append(
            CdaGeneratorUtils.getXmlForNFText(
                CdaGeneratorConstants.LAST_NAME_EL_NAME, CdaGeneratorConstants.NF_NI));
      }

      // Enough names for now.
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

  public static String getStringForType(Type dt) {

    if (dt != null) {

      String val = "";
      if (dt instanceof Coding) {
        Coding cd = (Coding) dt;

        if (cd.getCodeElement() != null && cd.getSystemElement() != null) {

          val +=
              cd.getSystemElement().getValue()
                  + CdaGeneratorConstants.PIPE
                  + cd.getCodeElement().getValue();
        }

      } else if (dt instanceof Quantity) {

        Quantity qt = (Quantity) dt;

        if (qt.getValueElement() != null && qt.getSystemElement() != null && qt.getUnit() != null) {

          val +=
              qt.getValueElement().getValueAsString()
                  + CdaGeneratorConstants.PIPE
                  + qt.getSystemElement().getValueAsString()
                  + CdaGeneratorConstants.PIPE
                  + qt.getUnit();
        }

      } else if (dt instanceof DateTimeType) {

        DateTimeType d = (DateTimeType) dt;

        val += d.getValueAsString();

      } else if (dt instanceof Period) {
        Period pt = (Period) dt;

        if (pt.getStart() != null && pt.getEnd() != null) {
          val += pt.getStart().toString() + CdaGeneratorConstants.PIPE + pt.getEnd().toString();
        } else if (pt.getStart() != null) {
          val += pt.getStart().toString();
        }
      } else if (dt instanceof CodeType) {

        CodeType cd = (CodeType) dt;

        val += cd.getValue();
      }

      logger.info(" Printing the class name " + dt.getClass());
      return val;
    }
    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  public static String getXmlForType(Type dt, String elName, Boolean valFlag) {

    if (dt != null) {

      String val = "";
      if (dt instanceof Coding) {
        Coding cd = (Coding) dt;

        List<Coding> cds = new ArrayList<Coding>();
        cds.add(cd);
        if (!valFlag) val += getCodingXml(cds, elName);
        else val += getCodingXmlForValue(cds, elName);

      } else if (dt instanceof CodeableConcept) {

        CodeableConcept cd = (CodeableConcept) dt;

        List<Coding> cds = cd.getCoding();

        if (!valFlag) val += getCodingXml(cds, elName);
        else val += getCodingXmlForValue(cds, elName);

      } else if (dt instanceof Quantity) {

        Quantity qt = (Quantity) dt;

        val += getQuantityXml(qt, elName, valFlag);

      } else if (dt instanceof DateTimeType) {

        DateTimeType d = (DateTimeType) dt;

        val += CdaGeneratorUtils.getXmlForEffectiveTime(elName, d.getValue());

      } else if (dt instanceof Period) {
        Period pt = (Period) dt;

        val += getPeriodXml(pt, elName);
      } else if (dt instanceof CodeType) {

        CodeType cd = (CodeType) dt;

        if (!valFlag)
          val += CdaGeneratorUtils.getNFXMLFoElement(elName, CdaGeneratorConstants.NF_NI);
        else val += CdaGeneratorUtils.getNFXMLForValue(CdaGeneratorConstants.NF_NI);
      }

      logger.info(" Printing the class name " + dt.getClass());
      return val;
    }

    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }
}
