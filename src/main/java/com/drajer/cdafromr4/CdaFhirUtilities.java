package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Address.AddressUse;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.DomainResource;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Encounter.EncounterLocationComponent;
import org.hl7.fhir.r4.model.Encounter.EncounterParticipantComponent;
import org.hl7.fhir.r4.model.Enumerations.AdministrativeGender;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.HumanName;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.InstantType;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Medication;
import org.hl7.fhir.r4.model.Medication.MedicationIngredientComponent;
import org.hl7.fhir.r4.model.MedicationAdministration;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Patient.ContactComponent;
import org.hl7.fhir.r4.model.Patient.PatientCommunicationComponent;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.Timing;
import org.hl7.fhir.r4.model.Type;
import org.hl7.fhir.r4.model.codesystems.V3ParticipationType;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaFhirUtilities {

  private CdaFhirUtilities() {
    throw new IllegalStateException("Utility class");
  }

  public static final Logger logger = LoggerFactory.getLogger(CdaFhirUtilities.class);

  public static List<Identifier> getIdentifierForType(List<Identifier> ids, String type) {

    List<Identifier> returnIds = new ArrayList<>();

    if (ids != null && !ids.isEmpty()) {

      for (Identifier id : ids) {

        if (id.getType() != null) {

          List<Coding> codings = id.getType().getCoding();

          if (codings != null && !codings.isEmpty()) {

            for (Coding coding : codings) {

              if (coding.getSystem() != null
                  && (coding
                          .getSystem()
                          .contentEquals(CdaGeneratorConstants.FHIR_IDENTIFIER_TYPE_SYSTEM)
                      || coding.getSystem().contentEquals(CdaGeneratorConstants.FHIR_IDTYPE_SYSTEM))
                  && coding.getCode() != null
                  && coding.getCode().contentEquals(type)) {

                logger.info(" Found the Identifier for Patient for type {}", type);
                returnIds.add(id);
              }
            }
          }
        }
      }
    }

    logger.info(" Did not find the Identifier for the patient for type {}", type);
    return returnIds;
  }

  public static Patient.ContactComponent getGuardianContact(List<ContactComponent> ccs) {

    if (ccs != null && !ccs.isEmpty()) {

      for (ContactComponent cc : ccs) {

        if (cc.getRelationship() != null && !cc.getRelationship().isEmpty()) {

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

    if (ids != null && !ids.isEmpty()) {

      for (Identifier id : ids) {

        if (id.getSystem() != null && id.getSystem().contentEquals(system)) {

          logger.info(" Found the Identifier for System: {}", system);
          return id;
        }
      }
    }

    logger.info(" Did not find the Identifier for  System : {}", system);
    return null;
  }

  public static Coding getCodingExtension(List<Extension> exts, String extUrl, String subextUrl) {

    if (exts != null && !exts.isEmpty()) {

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

    logger.info(" Did not find the Extension or sub extensions for the Url {}", extUrl);
    return null;
  }

  public static CodeType getCodeExtension(List<Extension> exts, String extUrl) {

    if (exts != null && !exts.isEmpty()) {

      for (Extension ext : exts) {

        // if the top level extension has CodingDt then we will use it.
        if (ext.getUrl() != null
            && ext.getUrl().contentEquals(extUrl)
            && ext.getValue() != null
            && (ext.getValue() instanceof CodeType)) {

          logger.info(" Found Extension at top level ");
          return (CodeType) ext.getValue();
        }
      }
    }

    logger.info(" Did not find the Extension or sub extensions for the Url {}", extUrl);
    return null;
  }

  public static Coding getLanguage(List<PatientCommunicationComponent> comms) {

    if (comms != null && !comms.isEmpty()) {

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

  public static Coding getCodingForCodeSystem(CodeableConcept cd, String codeSystemUrl) {

    if (cd != null) {

      List<Coding> cds = cd.getCoding();

      if (cds != null && !cds.isEmpty()) {

        for (Coding c : cds) {

          if (c.getSystem().contentEquals(codeSystemUrl)) {

            return c;
          }
        }
      }
    }

    return null;
  }

  public static Coding getLanguageForCodeSystem(
      List<PatientCommunicationComponent> comms, String codeSystemUrl) {

    if (comms != null && !comms.isEmpty()) {

      for (PatientCommunicationComponent comm : comms) {

        Coding c = getCodingForCodeSystem(comm.getLanguage(), codeSystemUrl);

        if (c != null) return c;
      }
    }

    logger.info(" Did not find the communication language ");
    return null;
  }

  public static String getAddressXml(List<Address> addrs) {

    StringBuilder addrString = new StringBuilder(200);

    if (addrs != null && !addrs.isEmpty()) {

      for (Address addr : addrs) {

        if (addr.getUseElement().getValue() == AddressUse.HOME
            || addr.getUseElement().getValue() == AddressUse.WORK) {

          logger.debug(" Found Home or Work Address ");
          addrString.append(
              CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ADDR_EL_NAME));

          // Address Line
          List<StringType> lines = addr.getLine();

          if (lines != null && !lines.isEmpty()) {

            for (StringType s : lines) {
              addrString.append(
                  CdaGeneratorUtils.getXmlForText(
                      CdaGeneratorConstants.ST_ADDR_LINE_EL_NAME, s.getValue()));
            }

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

    if (tels != null && !tels.isEmpty()) {

      for (ContactPoint tel : tels) {

        if (tel.getSystem() != null
            && tel.getSystem() == ContactPoint.ContactPointSystem.PHONE
            && !StringUtils.isEmpty(tel.getValue())) {

          logger.debug(" Found Telcom Number for " + tel.getSystem().getDisplay());

          String use = "";
          if (tel.getUse() != null) {
            use = CdaGeneratorConstants.getCodeForTelecomUse(tel.getUse().toCode());
          }

          telString.append(
              CdaGeneratorUtils.getXmlForTelecom(
                  CdaGeneratorConstants.TEL_EL_NAME, tel.getValue(), use));
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

    if (tels != null && !tels.isEmpty()) {

      for (ContactPoint tel : tels) {

        if (tel.getSystem() != null
            && tel.getSystem() == ContactPoint.ContactPointSystem.EMAIL
            && !StringUtils.isEmpty(tel.getValue())) {

          logger.info(" Found Email ");
          String use = "";
          if (tel.getUse() != null) {
            use = CdaGeneratorConstants.getCodeForTelecomUse(tel.getUse().toCode());
          }

          telString.append(
              CdaGeneratorUtils.getXmlForEmail(
                  CdaGeneratorConstants.TEL_EL_NAME, tel.getValue(), use));
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

  public static List<Practitioner> getPractitionersForType(
      R4FhirData data, V3ParticipationType type) {

    List<Practitioner> practs = new ArrayList<>();

    if (data != null && data.getEncounter() != null) {

      List<EncounterParticipantComponent> participants = data.getEncounter().getParticipant();

      if (participants != null && !participants.isEmpty()) {

        for (EncounterParticipantComponent part : participants) {

          if (part.getIndividual() != null && part.getIndividual().getReference() != null) {

            logger.debug(" Individual is present ");

            List<CodeableConcept> types = part.getType();

            if (types != null && !types.isEmpty()) {

              logger.debug(" Codeable Concepts present for individuals ");

              for (CodeableConcept conc : types) {

                logger.debug(" Get Coding information for codeable concept ");
                List<Coding> typeCodes = conc.getCoding();

                if (typeCodes != null && !typeCodes.isEmpty()) {

                  for (Coding cd : typeCodes) {

                    if (cd.getSystem() != null
                        && (cd.getSystem()
                                .contentEquals(CdaGeneratorConstants.FHIR_PARTICIPANT_TYPE)
                            || cd.getSystem()
                                .contentEquals(CdaGeneratorConstants.FHIR_PARTICIPANT_TYPE_V3))) {

                      logger.debug(" Found Practitioner for Participation code system ");

                      if (cd.getCode() != null && cd.getCode().contentEquals(type.toString())) {

                        logger.debug(" Found Practitioner for Code and CodeSystem ");

                        logger.debug(" part.getIndividual = {}", part.getIndividual().getDisplay());
                        if (part.getIndividual().getReferenceElement() != null)
                          logger.debug(
                              " part.getIndividual = {}",
                              part.getIndividual().getReferenceElement());

                        if (part.getIndividual().getReferenceElement() != null
                            && part.getIndividual().getReferenceElement().getIdPart() != null) {

                          Practitioner pr =
                              data.getPractitionerById(
                                  part.getIndividual().getReferenceElement().getIdPart());

                          if (pr != null) {

                            logger.info(" Found Practitioner for Type {}", type);
                            practs.add(pr);
                          } // Found Practitioenr
                        } // Valid Reference
                        else {
                          logger.info(" Individual Ref Id is null ");
                        }
                      } // Found Type that we need
                      else {
                        logger.debug(" Did not find the code for type {}", type);
                      }
                    } // Found participants that use standard code systems
                    else {
                      logger.debug(" Did not find participants using standard code system ");
                    }
                  } // For all Codings
                } // Codings present
              } // For all Codeable Concepts
            } // Codeable Concept present
          } // PArticipant is an individual
        } // For all EncounteR ParticipantComponents
      } // Participants not empty
    } // Encounter not null

    return practs;
  } // Method end

  public static BundleEntryComponent getResourceEntryForId(
      String id, String type, List<BundleEntryComponent> entries) {

    for (BundleEntryComponent ent : entries) {

      if (ent.getResource() != null
          &&
          //  ent.getResource() != null &&
          //   ent.getResource().fhirType().contentEquals(type) &&
          ent.getResource().getId() != null
          && ent.getResource().getId().contentEquals(id)) {

        logger.info(" Found entry for ID {} Type : {}", id, type);
        return ent;
      }
    }

    logger.info(" Did not find entry for ID {} Type : {}", id, type);
    return null;
  }

  public static Boolean isCodingPresentForCodeSystem(List<Coding> codings, String codeSystemUrl) {

    Boolean foundCodeSystem = false;

    for (Coding c : codings) {

      if (c.getSystem().contentEquals(codeSystemUrl)) {

        foundCodeSystem = true;
        break;
      }
    }

    return foundCodeSystem;
  }

  public static Pair<String, Boolean> getCodingDisplayForCodeSystem(
      List<Coding> codings, String codeSystemUrl, Boolean csOptional) {

    String display = "";
    String anyDisplay = "";
    Boolean foundCodeSystem = false;

    for (Coding c : codings) {

      if (c.getSystem().contentEquals(codeSystemUrl) && !StringUtils.isEmpty(c.getDisplay())) {

        display = c.getDisplay();
        foundCodeSystem = true;
        break;
      } else if (c.getSystem().contentEquals(codeSystemUrl)) {
        foundCodeSystem = true;
      }

      if (csOptional && !StringUtils.isEmpty(c.getDisplay())) {
        anyDisplay = c.getDisplay();
      }
    }

    if (!StringUtils.isEmpty(display)) return new Pair<>(display, foundCodeSystem);
    else if (!StringUtils.isEmpty(anyDisplay)) return new Pair<>(anyDisplay, foundCodeSystem);
    else return new Pair<>(display, foundCodeSystem);
  }

  public static Pair<String, Boolean> getCodeableConceptDisplayForCodeSystem(
      CodeableConcept cd, String codeSystemUrl, Boolean csOptional) {

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

  public static Date getActualDate(Type dt) {

    if (dt instanceof DateTimeType) {

      DateTimeType d1 = (DateTimeType) dt;
      return d1.getValue();
    } else if (dt instanceof Period) {

      logger.debug(" Found an instance of period ");
      Period d1 = (Period) dt;
      return d1.getStart();
    } else if (dt instanceof InstantType) {

      InstantType d1 = (InstantType) dt;
      return d1.getValue();
    } else if (dt instanceof Timing) {

      logger.debug(" Found an instance of timing ");
      Timing t = (Timing) (dt);
      if (t.getRepeat() != null && t.getRepeat().getBounds() != null) {

        logger.debug(" Found the bounds element ");
        return getActualDate(t.getRepeat().getBounds());
      }
    }

    return null;
  }

  public static String getCodeableConceptDisplayForCodeSystem(
      List<CodeableConcept> cds, String codeSystemUrl, Boolean csOptional) {

    String anyCdDisplay = "";
    Pair<String, Boolean> disp = null;

    if (cds != null && !cds.isEmpty()) {

      for (CodeableConcept cd : cds) {

        disp = getCodeableConceptDisplayForCodeSystem(cd, codeSystemUrl, csOptional);

        if (!StringUtils.isEmpty(disp.getValue0())) {

          // Found a display
          break;
        }

        // If display is at the Codeable Concept level, use it in case we don't find anything else
        if (cd != null && !StringUtils.isEmpty(cd.getText())) {
          anyCdDisplay = cd.getText();
        }
      }
    }

    if (disp != null && !StringUtils.isEmpty(disp.getValue0())) return disp.getValue0();
    else if (!StringUtils.isEmpty(anyCdDisplay)) return anyCdDisplay;
    else return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  public static String getCodeableConceptXmlForCodeSystem(
      List<CodeableConcept> cds,
      String cdName,
      Boolean valueTrue,
      String codeSystemUrl,
      Boolean csOptional) {

    StringBuilder sb = new StringBuilder(500);
    List<Coding> codes = getCodingForValidCodeSystems(cds);

    if (!valueTrue) sb.append(getCodingXmlForCodeSystem(codes, cdName, codeSystemUrl, csOptional));
    else sb.append(getCodingXmlForValueForCodeSystem(codes, cdName, codeSystemUrl, csOptional));

    return sb.toString();
  }

  public static List<Coding> getCodingForValidCodeSystems(List<CodeableConcept> cds) {
    List<Coding> codes = new ArrayList<>();
    if (cds != null && !cds.isEmpty()) {

      for (CodeableConcept cd : cds) {

        List<Coding> codings = cd.getCoding();

        if (codings != null && !codings.isEmpty()) {

          logger.debug(" Coding .size = {} ", codings.size());

          for (Coding code : codings) {

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

  public static String getCodeableConceptXml(
      List<CodeableConcept> cds, String cdName, Boolean valueTrue) {

    StringBuilder sb = new StringBuilder(500);
    List<Coding> codes = getCodingForValidCodeSystems(cds);

    if (!valueTrue) sb.append(getCodingXml(codes, cdName));
    else sb.append(getCodingXmlForValue(codes, cdName));

    return sb.toString();
  }

  public static String getCodingXmlForCodeSystem(
      List<Coding> codes, String cdName, String codeSystemUrl, Boolean csOptional) {

    StringBuilder sb = new StringBuilder(200);
    StringBuilder translations = new StringBuilder(200);

    Boolean foundCodeForCodeSystem = false;

    if (codes != null && !codes.isEmpty()) {

      for (Coding c : codes) {

        Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

        if (!csd.getValue0().isEmpty()
            && c.getSystem().contentEquals(codeSystemUrl)
            && !foundCodeForCodeSystem) {

          logger.info("Found the Coding for Codesystem {}", codeSystemUrl);
          sb.append(
              CdaGeneratorUtils.getXmlForCDWithoutEndTag(
                  cdName, c.getCode(), csd.getValue0(), csd.getValue1(), c.getDisplay()));

          foundCodeForCodeSystem = true;
        } else if (!csd.getValue0().isEmpty() && !csd.getValue1().isEmpty()) {

          logger.info(
              "Found the Coding for a different Codesystem {} for Translation ", csd.getValue0());
          translations.append(
              CdaGeneratorUtils.getXmlForCD(
                  CdaGeneratorConstants.TRANSLATION_EL_NAME,
                  c.getCode(),
                  csd.getValue0(),
                  csd.getValue1(),
                  c.getDisplay()));
        } else {
          logger.info(
              " Did not find the code system mapping from FHIR to CDA for {}", c.getSystem());
        }
      }

      // At least one code is there so...close the tag
      if (!foundCodeForCodeSystem) {

        // If we dont find the preferred code system, then add NF of OTH along with translations.
        sb.append(
            CdaGeneratorUtils.getXmlForNullCDWithoutEndTag(cdName, CdaGeneratorConstants.NF_OTH));
      }

      logger.info(" Sb = {}", sb);
      sb.append(translations);
      sb.append(CdaGeneratorUtils.getXmlForEndElement(cdName));

    } else {
      sb.append(CdaGeneratorUtils.getXmlForNullCD(cdName, CdaGeneratorConstants.NF_NI));
    }

    if (foundCodeForCodeSystem || (!csOptional)) {
      return sb.toString();
    } else {
      return new StringBuilder("").toString();
    }
  }

  public static String getCodingXml(List<Coding> codes, String cdName) {

    StringBuilder sb = new StringBuilder(200);

    if (codes != null && !codes.isEmpty()) {

      Boolean first = true;
      for (Coding c : codes) {

        if (first) {

          first = false;
          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

          if (!csd.getValue0().isEmpty() && !csd.getValue1().isEmpty())
            sb.append(
                CdaGeneratorUtils.getXmlForCDWithoutEndTag(
                    cdName, c.getCode(), csd.getValue0(), csd.getValue1(), c.getDisplay()));
        } else {

          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

          if (!csd.getValue1().isEmpty() && !csd.getValue0().isEmpty())
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

  public static String getCodingXmlForValueForCodeSystem(
      List<Coding> codes, String cdName, String codeSystemUrl, Boolean csOptional) {

    StringBuilder sb = new StringBuilder(200);
    StringBuilder translations = new StringBuilder(200);

    Boolean foundCodeForCodeSystem = false;

    if (codes != null && !codes.isEmpty()) {

      for (Coding c : codes) {

        Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

        if (!csd.getValue0().isEmpty()
            && !csd.getValue1().isEmpty()
            && c.getSystem().contentEquals(codeSystemUrl)
            && !foundCodeForCodeSystem) {

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
      if (!foundCodeForCodeSystem) {

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

    if (foundCodeForCodeSystem || (!csOptional)) {
      return sb.toString();
    } else {
      return new StringBuilder("").toString();
    }
  }

  public static String getCodingXmlForValue(List<Coding> codes, String cdName) {

    StringBuilder sb = new StringBuilder(200);

    if (!codes.isEmpty()) {

      Boolean first = true;
      for (Coding c : codes) {

        if (first) {

          first = false;
          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

          if (!csd.getValue0().isEmpty() && !csd.getValue1().isEmpty())
            sb.append(
                CdaGeneratorUtils.getXmlForValueCDWithoutEndTag(
                    c.getCode(), csd.getValue0(), csd.getValue1(), c.getDisplay()));
        } else {

          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

          if (!csd.getValue0().isEmpty() && !csd.getValue1().isEmpty())
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

    if (dt != null && dt.getValue() != null) {

      sb.append(
          CdaGeneratorUtils.getXmlForQuantityWithUnits(
              elName, dt.getValue().toString(), dt.getUnit(), valFlag));

    } else {
      sb.append(CdaGeneratorUtils.getXmlForNfQuantity(elName, CdaGeneratorConstants.NF_NI));
    }

    return sb.toString();
  }

  public static String getBirthSexXml(String birthSex) {

    String s = "";

    if (birthSex != null && (birthSex.contentEquals("M"))) {

      s +=
          CdaGeneratorUtils.getXmlForValueCD(
              birthSex,
              CdaGeneratorConstants.BIRTH_SEX_CODESYSTEM_OID,
              CdaGeneratorConstants.BIRTH_SEX_CODESYSTEM_NAME,
              "Male");
    } else if (birthSex != null && (birthSex.contentEquals("F"))) {
      s +=
          CdaGeneratorUtils.getXmlForValueCD(
              birthSex,
              CdaGeneratorConstants.BIRTH_SEX_CODESYSTEM_OID,
              CdaGeneratorConstants.BIRTH_SEX_CODESYSTEM_NAME,
              "Female");
    } else if (birthSex != null && (birthSex.contentEquals("UNK"))) {

      s +=
          CdaGeneratorUtils.getXmlForNullValueCD(
              CdaGeneratorConstants.VAL_EL_NAME, CdaGeneratorConstants.NF_UNK);
    } else {

      logger.info(" Did not find the birth sex for the patient ");
      s +=
          CdaGeneratorUtils.getXmlForNullValueCD(
              CdaGeneratorConstants.VAL_EL_NAME, CdaGeneratorConstants.NF_NI);
    }

    return s;
  }

  public static String getGenderXml(AdministrativeGender gender) {

    String s = "";

    if (gender == AdministrativeGender.MALE) {

      s +=
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.ADMIN_GENDER_CODE_EL_NAME,
              CdaGeneratorConstants.CDA_MALE_CODE,
              CdaGeneratorConstants.ADMIN_GEN_CODE_SYSTEM);
    } else if (gender == AdministrativeGender.FEMALE) {

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

    if (names != null && !names.isEmpty()) {

      Optional<HumanName> hName = names.stream().findFirst();
      if (hName.isPresent()) {

        HumanName name = hName.get();
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

  public static String getStringForCoding(Coding cd) {

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

  public static String getCombinationStringForCodeSystem(
      CodeableConcept code, Type value, String codeSystemUrl, Boolean csOptional) {

    Pair<String, Boolean> codeString = new Pair<>("", false);
    if (code != null) {

      codeString = getCodeableConceptDisplayForCodeSystem(code, codeSystemUrl, csOptional);
    }

    Pair<String, Boolean> valueString = new Pair<>("", false);
    if (value instanceof CodeableConcept) {

      CodeableConcept vCode = (CodeableConcept) value;
      valueString = getCodeableConceptDisplayForCodeSystem(vCode, codeSystemUrl, csOptional);
    } else if (value instanceof Coding) {
      Coding vCd = (Coding) value;
      List<Coding> cds = new ArrayList<>();
      cds.add(vCd);
      valueString = getCodingDisplayForCodeSystem(cds, codeSystemUrl, csOptional);
    } else if (value instanceof StringType) {
      StringType st = (StringType) value;
      valueString.setAt0(st.getValue());
    }

    return codeString.getValue0() + CdaGeneratorConstants.HYPHEN + valueString.getValue0();
  }

  public static String getStringForQuantity(Quantity qt) {

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

  public static String getStringForMedicationType(Resource r) {

    String retVal = CdaGeneratorConstants.UNKNOWN_VALUE;

    if (r instanceof MedicationRequest) {
      logger.info(" Found Med Request ");
      MedicationRequest mr = (MedicationRequest) r;

      if (mr.getMedication() instanceof Reference) {

        logger.info(" Found Med Request.Medication Reference ");

        Reference med = (Reference) mr.getMedication();

        if (med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {

          logger.info(" Found Med Request.Medication which is a contained reference");
          // Check contained.
          String refId = med.getReference().substring(1);

          logger.info("Ref Id {} ", refId);

          if (mr.getContained() != null) {

            retVal = getStringForMedicationFromContainedResources(mr.getContained(), refId);

            logger.info(" Return Val = {}", retVal);
          } // contained present
        } // Contained reference

        return retVal;

      } else if (mr.getMedication() instanceof CodeableConcept) {

        CodeableConcept cc = (CodeableConcept) mr.getMedication();

        return getStringForType(cc);
      }
    } else if (r instanceof MedicationAdministration) {

      MedicationAdministration medAdminRef = (MedicationAdministration) r;

      if (medAdminRef.getMedication() instanceof Reference) {

        Reference med = (Reference) medAdminRef.getMedication();

        if (med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {
          // Check contained.
          String refId = med.getReference().substring(1);

          if (medAdminRef.getContained() != null) {

            retVal =
                getStringForMedicationFromContainedResources(medAdminRef.getContained(), refId);
          } // contained present
        } // Contained reference

        return retVal;

      } else if (medAdminRef.getMedication() instanceof CodeableConcept) {

        CodeableConcept cc = (CodeableConcept) medAdminRef.getMedication();

        return getStringForType(cc);
      }

    } else if (r instanceof MedicationStatement) {

      MedicationStatement medStmtRef = (MedicationStatement) r;

      if (medStmtRef.getMedication() instanceof Reference) {

        Reference med = (Reference) medStmtRef.getMedication();

        if (med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {
          // Check contained.
          String refId = med.getReference().substring(1);

          if (medStmtRef.getContained() != null) {

            retVal = getStringForMedicationFromContainedResources(medStmtRef.getContained(), refId);
          } // contained present
        } // Contained reference

        return retVal;

      } else if (medStmtRef.getMedication() instanceof CodeableConcept) {

        CodeableConcept cc = (CodeableConcept) medStmtRef.getMedication();

        return getStringForType(cc);
      }
    }

    return retVal;
  }

  public static String getStringForMedicationFromContainedResources(
      List<Resource> resources, String refId) {

    Pair<String, Boolean> retVal = null;
    for (Resource res : resources) {

      logger.info(" res.getId {} ", res.getId());

      if (res.getId().contains(refId) && res instanceof Medication) {

        logger.info(" Found a Contained Resource with Id {} ", refId);
        Medication cmed = (Medication) res;
        // Found the reference, check the code and ingredients.

        if (cmed.getCode() != null) {
          logger.info(" Found Contained Med  Code ");
          retVal =
              getCodeableConceptDisplayForCodeSystem(
                  cmed.getCode(), CdaGeneratorConstants.FHIR_RXNORM_URL, false);
        } // if code present

        if (retVal.getValue0().isEmpty()) {

          logger.info(" Return Val is empty");

          if (cmed.getIngredient() != null) {

            logger.info(" Found ingredient ");
            List<MedicationIngredientComponent> ings = cmed.getIngredient();

            for (MedicationIngredientComponent ing : ings) {

              if (ing.getItem() instanceof CodeableConcept) {

                logger.info(" Found a CC for Ingredient ");
                CodeableConcept cc = (CodeableConcept) ing.getItem();
                retVal =
                    getCodeableConceptDisplayForCodeSystem(
                        cc, CdaGeneratorConstants.FHIR_RXNORM_URL, false);
                break;
              }
            }
          }
        }

        if (!retVal.getValue0().isEmpty()) return retVal.getValue0();
      } // Found id
    } // For all resources

    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  public static String getStringForType(Type dt) {

    if (dt != null) {

      StringBuilder val = new StringBuilder();
      if (dt instanceof Coding) {
        Coding cd = (Coding) dt;

        val.append(getStringForCoding(cd));

      } else if (dt instanceof CodeableConcept) {

        CodeableConcept cd = (CodeableConcept) dt;

        if (!StringUtils.isEmpty(cd.getText())) {
          val.append(cd.getText());
        } else {
          List<Coding> cds = cd.getCoding();
          Boolean first = true;

          for (Coding c : cds) {

            if (!first) {

              val.append(CdaGeneratorConstants.SPACE)
                  .append(CdaGeneratorConstants.PIPE)
                  .append(CdaGeneratorConstants.SPACE);
            }
            first = false;
            val.append(getStringForCoding(c));
          }
        }

      } else if (dt instanceof Quantity) {

        Quantity qt = (Quantity) dt;

        val.append(getStringForQuantity(qt));

      } else if (dt instanceof DateTimeType) {

        DateTimeType d = (DateTimeType) dt;

        val.append(d.getValueAsString());

      } else if (dt instanceof Timing) {

        logger.debug(" Found an instance of timing for creating string ");
        Timing t = (Timing) (dt);
        if (t.getRepeat() != null && t.getRepeat().getBounds() != null) {

          logger.debug(" Found the bounds element for creating string ");

          String v = getStringForType(t.getRepeat().getBounds());
          val.append(v);
        }

      } else if (dt instanceof Period) {
        Period pt = (Period) dt;

        logger.debug(" Found the Period element for creating string");
        if (pt.getStart() != null && pt.getEnd() != null) {
          val.append(pt.getStart().toString())
              .append(CdaGeneratorConstants.PIPE)
              .append(pt.getEnd().toString());
        } else if (pt.getStart() != null) {
          val.append(pt.getStart().toString());
        } else {
          val.append(CdaGeneratorConstants.UNKNOWN_VALUE);
        }
      } else if (dt instanceof CodeType) {

        CodeType cd = (CodeType) dt;

        val.append(cd.getValue());
      } else if (dt instanceof StringType) {

        StringType st = (StringType) dt;

        val.append(st.getValue());
      }

      logger.info(" Printing the class name {} and value {}", dt.getClass(), val);
      return val.toString();
    }
    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  public static String getXmlForType(Type dt, String elName, Boolean valFlag) {

    String val = "";
    if (dt != null) {

      if (dt instanceof Coding) {
        Coding cd = (Coding) dt;

        List<Coding> cds = new ArrayList<>();
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
      } else if (dt instanceof Timing) {

        Timing t = (Timing) (dt);
        if (t.getRepeat() != null && t.getRepeat().getBounds() != null) {

          logger.debug(" Found the bounds element for creating xml ");

          String v = getXmlForType(t.getRepeat().getBounds(), elName, valFlag);
          val += v;
        }
      } else if (dt instanceof CodeType) {

        CodeType cd = (CodeType) dt;
        if (!valFlag) val += CdaGeneratorUtils.getXmlForCD(elName, cd.getCode());
        else val += CdaGeneratorUtils.getXmlForValueString(cd.getCode());
      } else if (dt instanceof StringType) {

        StringType st = (StringType) dt;
        if (!valFlag) val += CdaGeneratorUtils.getXmlForText(elName, st.getValue());
        else val += CdaGeneratorUtils.getXmlForValueString(st.getValue());
      }

      logger.info(" Printing the class name " + dt.getClass());
      return val;
    }

    if (!valFlag) val += CdaGeneratorUtils.getNFXMLForElement(elName, CdaGeneratorConstants.NF_NI);
    else val += CdaGeneratorUtils.getNFXmlForValueString(CdaGeneratorConstants.NF_NI);

    return val;
  }

  public static String getXmlForTypeForValueIvlTsEffectiveTime(String elName, Type dt) {

    String val = "";
    if (dt != null) {

      if (dt instanceof DateTimeType) {

        DateTimeType d = (DateTimeType) dt;

        val += CdaGeneratorUtils.getXmlForEffectiveTime(elName, d.getValue());

      } else if (dt instanceof Period) {
        Period pt = (Period) dt;

        val += getPeriodXml(pt, elName);
      } else if (dt instanceof Timing) {

        Timing t = (Timing) (dt);
        if (t.getRepeat() != null && t.getRepeat().getBounds() != null) {

          logger.debug(" Found the bounds element for creating xml ");
        }
      }

      logger.debug(" Printing the class name " + dt.getClass());
      return val;
    }

    // val += CdaGeneratorUtils.getXmlForV(CdaGeneratorConstants.NF_NI);

    return val;
  }

  public static String getXmlForMedicationTypeForCodeSystem(
      Type dt,
      String elName,
      Boolean valFlag,
      String codeSystemUrl,
      Boolean csOptional,
      DomainResource res) {

    if (dt instanceof Reference) {

      logger.info(" Found Medication of Type Reference within Domain Resource ");
      Reference med = (Reference) dt;
      String codeXml = "";
      if (med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {
        // Check contained.
        String refId = med.getReference().substring(1);

        logger.info(" Found Medication of Type Reference with Id {} ", refId);

        if (res.getContained() != null) {

          logger.info(" Contained Elements Not null");
          List<Resource> meds = res.getContained();

          for (Resource r : meds) {

            if (r.getId().contains(refId) && r instanceof Medication) {

              logger.info(" Found Medication in contained resource ");

              Medication cmed = (Medication) r;

              // Found the reference, check the code and ingredients.

              if (cmed.getCode() != null
                  && cmed.getCode().getCoding() != null
                  && !cmed.getCode().getCoding().isEmpty()
                  && CdaFhirUtilities.isCodingPresentForCodeSystem(
                      cmed.getCode().getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL)) {

                logger.info(" Found Medication for code system in code element ");
                // Found the Medication that matters.
                codeXml =
                    getXmlForTypeForCodeSystem(
                        cmed.getCode(), elName, valFlag, codeSystemUrl, csOptional);

              } // if code present
              else {
                // Check the ingredients

                if (cmed.getIngredient() != null) {

                  logger.info(" Found Ingredients ");
                  List<MedicationIngredientComponent> ings = cmed.getIngredient();

                  for (MedicationIngredientComponent ing : ings) {

                    if (ing.getItem() instanceof CodeableConcept) {

                      logger.info(" Found Ingredient which is coded ");
                      CodeableConcept cc = (CodeableConcept) ing.getItem();

                      if (cc.getCoding() != null
                          && !cc.getCoding().isEmpty()
                          && CdaFhirUtilities.isCodingPresentForCodeSystem(
                              cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL)) {
                        codeXml =
                            getXmlForTypeForCodeSystem(
                                cc, elName, valFlag, codeSystemUrl, csOptional);
                        break;
                      }
                    }
                  }
                }
              }
            } // contained med
          } // for all contained resources
        } // contained present

      } // Contained reference
      else {

        // Check the actual medication

      }

      return codeXml;

    } else return getXmlForTypeForCodeSystem(dt, elName, valFlag, codeSystemUrl, csOptional);
  }

  public static String getXmlForTypeForCodeSystem(
      Type dt, String elName, Boolean valFlag, String codeSystemUrl, Boolean csOptional) {

    String val = "";
    if (dt != null) {

      if (dt instanceof Coding) {
        Coding cd = (Coding) dt;

        List<Coding> cds = new ArrayList<>();
        cds.add(cd);
        if (!valFlag) val += getCodingXmlForCodeSystem(cds, elName, codeSystemUrl, csOptional);
        else val += getCodingXmlForValueForCodeSystem(cds, elName, codeSystemUrl, csOptional);

      } else if (dt instanceof CodeableConcept) {

        CodeableConcept cd = (CodeableConcept) dt;

        List<Coding> cds = cd.getCoding();

        if (!valFlag) val += getCodingXmlForCodeSystem(cds, elName, codeSystemUrl, csOptional);
        else val += getCodingXmlForValueForCodeSystem(cds, elName, codeSystemUrl, csOptional);

      } else {

        if (!valFlag) val += CdaGeneratorUtils.getXmlForNullCD(elName, CdaGeneratorConstants.NF_NI);
        else val += CdaGeneratorUtils.getNFXMLForValue(CdaGeneratorConstants.NF_NI);
      }

      logger.info(" Printing the class name " + dt.getClass());
      return val;
    }

    if (!valFlag) val += CdaGeneratorUtils.getXmlForNullCD(elName, CdaGeneratorConstants.NF_NI);
    else val += CdaGeneratorUtils.getNFXMLForValue(CdaGeneratorConstants.NF_NI);

    return val;
  }

  public static List<String> getMatchedCodesForResourceAndUrl(
      LaunchDetails details, String matchResourceType, String csUrl) {

    PatientExecutionState state = ApplicationUtils.getDetailStatus(details);

    List<MatchedTriggerCodes> mtcs = state.getMatchTriggerStatus().getMatchedCodes();
    List<String> matchedCodesForUrl = new ArrayList<>();

    for (MatchedTriggerCodes mtc : mtcs) {

      // Add each code as an entry relationship observation
      if (mtc.hasMatchedTriggerCodes(matchResourceType)) {

        logger.info(" Found Matched Codes for Resource Type {}", matchResourceType);

        Set<String> matchedCodes = mtc.getMatchedCodes();

        if (matchedCodes != null && !matchedCodes.isEmpty()) {

          // Split the system and code.
          for (String s : matchedCodes) {
            String[] parts = s.split("\\|");
            if (parts[0].contentEquals(csUrl)) {
              matchedCodesForUrl.add(parts[1]);
            }
          }
        }
      }
    }

    return matchedCodesForUrl;
  }

  public static List<String> getMatchedValuesForResourceAndUrl(
      LaunchDetails details, String matchResourceType, String csUrl) {

    PatientExecutionState state = ApplicationUtils.getDetailStatus(details);

    List<MatchedTriggerCodes> mtcs = state.getMatchTriggerStatus().getMatchedCodes();
    List<String> matchedValuesForUrl = new ArrayList<>();

    for (MatchedTriggerCodes mtc : mtcs) {

      // Add each code as an entry relationship observation
      if (mtc.hasMatchedTriggerValue(matchResourceType)) {

        logger.info(" Found Matched Codes for Resource Type {}", matchResourceType);

        Set<String> matchedValues = mtc.getMatchedValues();

        if (matchedValues != null && !matchedValues.isEmpty()) {

          // Split the system and code.
          for (String s : matchedValues) {
            String[] parts = s.split("\\|");
            if (parts[0].contentEquals(csUrl)) {
              matchedValuesForUrl.add(parts[1]);
            }
          }
        }
      }
    }

    return matchedValuesForUrl;
  }

  public static Boolean isCodePresent(List<String> codes, String code) {

    for (String c : codes) {
      if (c.contentEquals(code)) return true;
    }

    return false;
  }

  public static String getMatchingCodeFromCodingForCodeSystem(
      List<String> matchedCodes, List<Coding> cds, String csUrl) {

    if (matchedCodes != null && cds != null && !cds.isEmpty()) {

      for (Coding c : cds) {

        Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

        if (!csd.getValue0().isEmpty()
            && !c.getCode().isEmpty()
            && !c.getSystem().isEmpty()
            && c.getSystem().contentEquals(csUrl)
            && isCodePresent(matchedCodes, c.getCode())) {

          logger.info("Found the Coding for Codesystem {} and Code =  {}", csUrl, c.getCode());

          return c.getCode();
        }
      }
    }

    return "";
  }

  public static String getMatchingCodeFromTypeForCodeSystem(
      List<String> matchedCodes, Type dt, String csUrl) {

    if (dt instanceof CodeableConcept) {
      CodeableConcept cc = (CodeableConcept) dt;
      return getMatchingCodeFromCodeableConceptForCodeSystem(matchedCodes, cc, csUrl);
    } else if (dt instanceof Coding) {
      List<Coding> cds = new ArrayList<>();
      cds.add((Coding) dt);
      return getMatchingCodeFromCodingForCodeSystem(matchedCodes, cds, csUrl);
    } else return "";
  }

  public static String getMatchingCodeFromCodeableConceptForCodeSystem(
      List<String> matchedCodes, CodeableConcept cd, String csUrl) {

    if (cd != null && cd.getCoding() != null && !cd.getCoding().isEmpty()) {
      return getMatchingCodeFromCodingForCodeSystem(matchedCodes, cd.getCoding(), csUrl);
    }

    return "";
  }

  public static String getXmlForCodeableConceptWithCDAndValueSetAndVersion(
      String elementName,
      String code,
      String codeSystem,
      String codeSystemName,
      String valueSet,
      String valuesetVersion,
      CodeableConcept cc,
      String csUrl) {

    StringBuilder retval = new StringBuilder();
    StringBuilder translations = new StringBuilder();

    Boolean foundCodings = false;
    if (cc != null) {

      String dispName = cc.getText();

      List<Coding> cds = cc.getCoding();

      if (cds != null && !cds.isEmpty()) {

        for (Coding cd : cds) {

          if (cd.getCode() != null
              && !cd.getCode().isEmpty()
              && code.contentEquals(cd.getCode())
              && csUrl.contentEquals(cd.getSystem())
              && !foundCodings) {

            logger.info(
                " Found a Coding that matches the CodeSystem and Code {} : {} ", codeSystem, code);
            if (cd.getDisplay() != null && !cd.getDisplay().isEmpty()) dispName = cd.getDisplay();

            if (elementName.contentEquals(CdaGeneratorConstants.CODE_EL_NAME)) {
              retval.append(
                  CdaGeneratorUtils.getXmlForCDWithValueSetAndVersionWihoutEndTag(
                      elementName,
                      code,
                      codeSystem,
                      codeSystemName,
                      valueSet,
                      valuesetVersion,
                      dispName));

            } else if (elementName.contentEquals(CdaGeneratorConstants.VAL_EL_NAME)) {

              retval.append(
                  CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersionWihoutEndTag(
                      elementName,
                      code,
                      codeSystem,
                      codeSystemName,
                      valueSet,
                      valuesetVersion,
                      dispName));
            }
            foundCodings = true;
          } else {

            Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(cd.getSystem());

            if (!csd.getValue0().isEmpty() && !csd.getValue1().isEmpty()) {

              if (cd.getDisplay() != null && !cd.getDisplay().isEmpty()) dispName = cd.getDisplay();

              // Create Translations.
              translations.append(
                  CdaGeneratorUtils.getXmlForCD(
                      CdaGeneratorConstants.TRANSLATION_EL_NAME,
                      cd.getCode(),
                      csd.getValue0(),
                      csd.getValue1(),
                      dispName));
            }
          }
        }
      }
    }

    if (foundCodings) {
      retval.append(translations.toString());
      retval.append(CdaGeneratorUtils.getXmlForEndElement(elementName));
    } else {

      String dispName = "";
      if (cc != null && cc.getText() != null && !cc.getText().isEmpty()) dispName = cc.getText();

      if (elementName.contentEquals(CdaGeneratorConstants.CODE_EL_NAME)) {
        retval.append(
            CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
                CdaGeneratorConstants.CODE_EL_NAME,
                code,
                CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
                CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
                CdaGeneratorConstants.RCTC_OID,
                ActionRepo.getInstance().getRctcVersion(),
                dispName));
      } else if (elementName.contentEquals(CdaGeneratorConstants.VAL_EL_NAME)) {
        retval.append(
            CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersion(
                code,
                CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
                CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
                CdaGeneratorConstants.RCTC_OID,
                ActionRepo.getInstance().getRctcVersion(),
                dispName));
      }
    }

    return retval.toString();
  }

  public static String getStatusCodeForFhirMedStatusCodes(String val) {

    if (val.equalsIgnoreCase("active")
        || val.equalsIgnoreCase("in-progress")
        || val.equalsIgnoreCase("intended")
        || val.equalsIgnoreCase("not-taken")) {
      return "active";
    } else if (val.equalsIgnoreCase("completed")) {
      return "completed";
    } else if (val.equalsIgnoreCase("entered-in-error")) {
      return "nullified";
    } else if (val.equalsIgnoreCase("stopped")) {
      return "aborted";
    } else if (val.equalsIgnoreCase("on-hold")) {
      return "suspended";
    } else if (val.equalsIgnoreCase("unknown") || val.equalsIgnoreCase("draft")) {
      return "held";
    } else if (val.equalsIgnoreCase("cancelled")) {
      return "held";
    } else return "completed";
  }
}
