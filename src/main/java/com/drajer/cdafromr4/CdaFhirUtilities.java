package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
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
import java.util.TimeZone;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.BooleanType;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.DateType;
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
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
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

  public static final String PRINTING_THE_CLASS_NAME = "Printing the class name {}";
  public static final String COMPLETED = "completed";

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

                logger.debug("Found the Identifier for Patient for type {}", type);
                returnIds.add(id);
              }
            }
          }
        }
      }
    }
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
            } else {
              List<Coding> cs = cd.getCoding();

              for (Coding c : cs) {

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
                  return cc;
                }
              }
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

          logger.debug("Found the Identifier for System: {}", system);
          return id;
        }
      }
    }

    logger.debug("Did not find the Identifier for  System : {}", system);
    return null;
  }

  public static Coding getCodingExtension(List<Extension> exts, String extUrl, String subextUrl) {

    if (exts != null && !exts.isEmpty()) {

      for (Extension ext : exts) {

        if (ext.getUrl() != null && ext.getUrl().contentEquals(extUrl)) {

          // if the top level extension has Coding then we will use it.
          if (ext.getValue() instanceof Coding) {

            logger.debug("Found Extension at top level ");
            return (Coding) ext.getValue();

          } else if (ext.getValue() == null) {

            // get child extensions.
            List<Extension> subExts = ext.getExtensionsByUrl(subextUrl);

            for (Extension subext : subExts) {

              if (subext.getValue() instanceof Coding) {

                logger.debug("Found Extension nested as children ");
                return (Coding) subext.getValue();
              }
            }
          }
        }
      }
    }

    logger.debug("Did not find the Extension or sub extensions for the Url {}", extUrl);
    return null;
  }

  public static List<Coding> getAllCodingsFromExtension(
      List<Extension> exts, String extUrl, String subextUrl) {
    List<Coding> codings = new ArrayList<>();

    if (exts == null || exts.isEmpty()) {
      logger.debug("No extensions provided");
      return codings;
    }

    for (Extension ext : exts) {
      if (ext.hasUrl() && ext.getUrl().contentEquals(extUrl)) {

        if (ext.hasValue() && ext.getValue() instanceof Coding) {
          logger.debug("Found Extension at top level ");
          codings.add((Coding) ext.getValue());

        } else if (!ext.hasValue()) {

          List<Extension> subExts = ext.getExtensionsByUrl(subextUrl);

          for (Extension subext : subExts) {
            if (subext.hasValue()) {

              if (subext.getValue() instanceof Coding) {
                logger.debug("Found Extension nested as children ");
                codings.add((Coding) subext.getValue());

              } else if (subext.getValue() instanceof CodeableConcept) {

                CodeableConcept cd = (CodeableConcept) subext.getValue();

                if (cd.hasCoding()) {
                  logger.debug("Found Extension nested as childrens ");
                  codings.addAll(cd.getCoding());
                }
              }
            }
          }
        }
      }
    }

    if (codings.isEmpty()) {
      logger.debug("Did not find the Extension or sub extensions for the Url {}", extUrl);
    }

    return codings;
  }

  public static Coding getCodingExtension(List<Extension> exts, String extUrl) {

    if (exts != null && !exts.isEmpty()) {

      for (Extension ext : exts) {

        if (ext.getUrl() != null && ext.getUrl().contentEquals(extUrl)) {

          // if the top level extension has Coding then we will use it.
          if (ext.getValue() instanceof Coding) {

            logger.debug("Found Extension at top level ");
            return (Coding) ext.getValue();
          }
        }
      }
    }

    logger.debug("Did not find the Extension or sub extensions for the Url {}", extUrl);
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

          logger.debug("Found Extension at top level ");
          return (CodeType) ext.getValue();
        }
      }
    }

    logger.debug("Did not find the Extension or sub extensions for the Url {}", extUrl);
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

    logger.debug("Did not find the communication language ");
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

  public static Pair<Coding, Boolean> getLanguageForCodeSystem(
      List<PatientCommunicationComponent> comms, String codeSystemUrl) {

    Coding prefCoding = null;
    Coding altCoding = null;
    if (comms != null && !comms.isEmpty()) {

      for (PatientCommunicationComponent comm : comms) {

        if (comm.hasPreferred() && comm.getPreferred()) {

          prefCoding = getCodingForCodeSystem(comm.getLanguage(), codeSystemUrl);
          break;
        } else if (comm.hasLanguage()
            && comm.getLanguage().hasCoding()
            && comm.getLanguage().getCodingFirstRep().hasCode()) {

          // Assign the alternate coding.
          altCoding = getCodingForCodeSystem(comm.getLanguage(), codeSystemUrl);
        }
      }

      // Found preferred language.
      if (prefCoding != null) {
        return new Pair<>(prefCoding, true);
      } else if (altCoding != null) {
        return new Pair<>(altCoding, false);
      }
    }

    logger.debug("Did not find the communication language ");
    return null;
  }

  public static String getAddressXml(List<Address> addrs, Boolean includeMultiples) {

    StringBuilder addrString = new StringBuilder(200);

    if (addrs != null && !addrs.isEmpty()) {

      if (includeMultiples) {
        for (Address addr : addrs) {
          addrString.append(getAddressXml(addr));
        }
      } else {

        Address addres = null;
        for (Address addr : addrs) {
          if (addr.hasUse() && addr.getUseElement().getValue() == Address.AddressUse.WORK) {
            addres = addr;
            break;
          }
        }

        if (addres == null) addres = addrs.get(0);
        addrString.append(getAddressXml(addres));
      }
    } else {
      Address addr = null;
      addrString.append(getAddressXml(addr));
    }

    return addrString.toString();
  }

  public static String getAddressXml(Address addr) {

    StringBuilder addrString = new StringBuilder(200);

    if (addr != null) {

      logger.debug(" Found a valid address. ");
      String addrUse = null;
      if (addr.getUse() != null) {
        addrUse = CdaGeneratorConstants.getCodeForAddressUse(addr.getUse().toCode());
      }

      addrString.append(
          CdaGeneratorUtils.getXmlForStartElementWithAttribute(
              CdaGeneratorConstants.ADDR_EL_NAME, CdaGeneratorConstants.USE_ATTR_NAME, addrUse));

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
      if (addr.hasCity() && !StringUtils.isEmpty(addr.getCity())) {
        addrString.append(
            CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.CITY_EL_NAME, addr.getCity()));
      } else {
        addrString.append(
            CdaGeneratorUtils.getXmlForNFText(
                CdaGeneratorConstants.CITY_EL_NAME, CdaGeneratorConstants.NF_NI));
      }

      // County
      if (addr.hasDistrict() && !StringUtils.isEmpty(addr.getDistrict())) {
        addrString.append(
            CdaGeneratorUtils.getXmlForText(
                CdaGeneratorConstants.COUNTY_EL_NAME, addr.getDistrict()));
      }

      // State
      if (addr.hasState() && !StringUtils.isEmpty(addr.getState())) {
        addrString.append(
            CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.STATE_EL_NAME, addr.getState()));
      } else {
        addrString.append(
            CdaGeneratorUtils.getXmlForNFText(
                CdaGeneratorConstants.STATE_EL_NAME, CdaGeneratorConstants.NF_NI));
      }

      // Postal Code
      if (addr.hasPostalCode() && !StringUtils.isEmpty(addr.getPostalCode())) {
        addrString.append(
            CdaGeneratorUtils.getXmlForText(
                CdaGeneratorConstants.POSTAL_CODE_EL_NAME, addr.getPostalCode()));
      } else {
        addrString.append(
            CdaGeneratorUtils.getXmlForNFText(
                CdaGeneratorConstants.POSTAL_CODE_EL_NAME, CdaGeneratorConstants.NF_NI));
      }

      // Country
      if (addr.hasCountry() && !StringUtils.isEmpty(addr.getCountry())) {
        addrString.append(
            CdaGeneratorUtils.getXmlForText(
                CdaGeneratorConstants.COUNTRY_EL_NAME, addr.getCountry()));
      } else {
        addrString.append(
            CdaGeneratorUtils.getXmlForNFText(
                CdaGeneratorConstants.COUNTRY_EL_NAME, CdaGeneratorConstants.NF_NI));
      }

      addrString.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ADDR_EL_NAME));

    } else {

      logger.debug("Did not find the Address ");
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

  public static String getTelecomXml(List<ContactPoint> tels, boolean onlyOne) {

    StringBuilder telString = new StringBuilder(200);

    if (tels != null && !tels.isEmpty()) {

      for (ContactPoint tel : tels) {

        if (tel.getSystem() != null
            && tel.getSystem() == ContactPoint.ContactPointSystem.PHONE
            && !StringUtils.isEmpty(tel.getValue())) {

          logger.debug("Found Telecom Number for {}", tel.getSystem().getDisplay());

          String use =
              (tel.getUse() == null)
                  ? ""
                  : CdaGeneratorConstants.getCodeForTelecomUse(tel.getUse().toCode());

          telString.append(
              CdaGeneratorUtils.getXmlForTelecom(
                  CdaGeneratorConstants.TEL_EL_NAME, tel.getValue(), use, false));

          if (onlyOne) break;
        } else if (tel.getSystem() != null
            && tel.getSystem() == ContactPoint.ContactPointSystem.EMAIL
            && !StringUtils.isEmpty(tel.getValue())) {

          logger.debug("Found Email address ");
          String use = "";

          telString.append(
              CdaGeneratorUtils.getXmlForEmail(
                  CdaGeneratorConstants.TEL_EL_NAME, tel.getValue(), use));

          if (onlyOne) break;
        } else if (tel.getSystem() != null
            && tel.getSystem() == ContactPoint.ContactPointSystem.FAX
            && !StringUtils.isEmpty(tel.getValue())) {

          logger.debug("Found Fax address ");
          String use = "";

          telString.append(
              CdaGeneratorUtils.getXmlForTelecom(
                  CdaGeneratorConstants.TEL_EL_NAME, tel.getValue(), use, true));

          if (onlyOne) break;
        }
      }
    } else {

      logger.debug("Did not find the Telecom ");
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

          logger.debug("Found Email address ");
          String use = "";

          telString.append(
              CdaGeneratorUtils.getXmlForEmail(
                  CdaGeneratorConstants.TEL_EL_NAME, tel.getValue(), use));
        }
      }
    } else {

      logger.debug("Did not find the Email ");
      telString.append(
          CdaGeneratorUtils.getXmlForNFText(
              CdaGeneratorConstants.TEL_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    return telString.toString();
  }

  public static Organization getOrganization(List<BundleEntryComponent> entries, Encounter en) {

    if (en.getServiceProvider().getReference() != null) {

      BundleEntryComponent ent =
          getResourceEntryForId(en.getServiceProvider().getReference(), "Organization", entries);

      if (ent != null) {

        logger.debug("Found organization for Id {}", en.getServiceProvider().getReference());
        return (Organization) ent.getResource();
      }
    }

    logger.debug("Did not find the organization resource for encounter");
    return null;
  }

  public static Location getLocation(List<BundleEntryComponent> entries, Encounter en) {

    EncounterLocationComponent loc = en.getLocationFirstRep();

    if (loc != null && loc.getLocation() != null) {

      BundleEntryComponent ent =
          getResourceEntryForId(loc.getLocation().getReference(), "Location", entries);

      if (ent != null) {

        logger.debug("Found Location for Id {}", loc.getLocation().getReference());
        return (Location) ent.getResource();
      }
    }

    logger.debug("Did not find the location resource for encounter");
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

            logger.debug("Individual is present");

            List<CodeableConcept> types = part.getType();

            if (types != null && !types.isEmpty()) {

              logger.debug("Codeable Concepts present for individuals");

              for (CodeableConcept conc : types) {

                logger.debug("Get Coding information for codeable concept");
                List<Coding> typeCodes = conc.getCoding();

                if (typeCodes != null && !typeCodes.isEmpty()) {

                  for (Coding cd : typeCodes) {

                    if (cd.getSystem() != null
                        && (cd.getSystem()
                                .contentEquals(CdaGeneratorConstants.FHIR_PARTICIPANT_TYPE)
                            || cd.getSystem()
                                .contentEquals(CdaGeneratorConstants.FHIR_PARTICIPANT_TYPE_V3))) {

                      logger.debug("Found Practitioner for Participation code system");

                      if (cd.getCode() != null && cd.getCode().contentEquals(type.toString())) {

                        logger.debug("Found Practitioner for Code and CodeSystem");

                        logger.debug("part.getIndividual = {}", part.getIndividual().getDisplay());
                        if (part.getIndividual().getReferenceElement() != null)
                          logger.debug(
                              "part.getIndividual = {}",
                              part.getIndividual().getReferenceElement());

                        if (part.getIndividual().getReferenceElement() != null
                            && part.getIndividual().getReferenceElement().getIdPart() != null) {

                          Practitioner pr =
                              data.getPractitionerById(
                                  part.getIndividual().getReferenceElement().getIdPart());

                          if (pr != null) {

                            logger.info("Found Practitioner for Type {}", type);
                            practs.add(pr);
                          } // Found Practitioenr
                        } // Valid Reference
                        else {
                          logger.debug("Individual Ref Id is null");
                        }
                      } // Found Type that we need
                      else {
                        logger.debug("Did not find the code for type {}", type);
                      }
                    } // Found participants that use standard code systems
                    else {
                      logger.debug("Did not find participants using standard code system ");
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
          // ent.getResource() != null &&
          // ent.getResource().fhirType().contentEquals(type) &&
          ent.getResource().getId() != null
          && ent.getResource().getId().contentEquals(id)) {

        logger.debug("Found entry for ID {} Type : {}", id, type);
        return ent;
      }
    }

    logger.debug("Did not find entry for ID {} Type : {}", id, type);
    return null;
  }

  public static Boolean isCodingPresentForCodeSystem(List<Coding> codings, String codeSystemUrl) {

    Boolean foundCodeSystem = false;

    for (Coding c : codings) {

      if (c.hasSystem() && c.getSystem().contentEquals(codeSystemUrl)) {

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

      if (Boolean.TRUE.equals(csOptional) && !StringUtils.isEmpty(c.getDisplay())) {
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

  public static Pair<Date, TimeZone> getActualDate(Type dt) {

    Date d = null;
    TimeZone t = null;
    if (dt instanceof DateTimeType) {

      DateTimeType d1 = (DateTimeType) dt;
      d = d1.getValue();
      t = d1.getTimeZone();

    } else if (dt instanceof Period) {

      logger.debug("Found an instance of period");
      Period d1 = (Period) dt;

      if (d1.getStartElement() != null) {

        d = d1.getStart();
        t = d1.getStartElement().getTimeZone();
      } else if (d1.getEndElement() != null) {
        d = d1.getEnd();
        t = d1.getEndElement().getTimeZone();
      }

    } else if (dt instanceof InstantType) {

      InstantType d1 = (InstantType) dt;
      d = d1.getValue();
      t = d1.getTimeZone();

    } else if (dt instanceof Timing) {

      logger.debug(" Found an instance of timing ");
      Timing tmg = (Timing) (dt);
      if (tmg.getRepeat() != null && tmg.getRepeat().getBounds() != null) {

        logger.debug(" Found the bounds element ");
        return getActualDate(tmg.getRepeat().getBounds());
      }
    }

    return new Pair<>(d, t);
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

        // If display is at the Codeable Concept level, use it in case we don't find
        // anything else
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

    if (Boolean.FALSE.equals(valueTrue))
      sb.append(getCodingXmlForCodeSystem(codes, cdName, codeSystemUrl, csOptional, ""));
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

    if (Boolean.FALSE.equals(valueTrue)) sb.append(getCodingXml(codes, cdName, ""));
    else sb.append(getCodingXmlForValue(codes, cdName, null));

    return sb.toString();
  }

  public static String getCodeableConceptXmlForMappedConceptDomain(
      String conceptDomain,
      List<CodeableConcept> cds,
      String cdName,
      Boolean valueTrue,
      Boolean includeNullFlavor) {

    if (cds == null) {
      return "";
    }

    StringBuilder sb = new StringBuilder();

    List<Coding> codes = getCodingForValidCodeSystems(cds);

    if (!codes.isEmpty()) {
      if (Boolean.FALSE.equals(valueTrue)) {
        sb.append(
            getCodingXmlForMappedConceptDomain(conceptDomain, codes, cdName, includeNullFlavor));
      } else {
        sb.append(
            getCodingXmlForValueForMappedConceptDomain(
                conceptDomain, codes, cdName, includeNullFlavor));
      }
    } else {
      for (CodeableConcept cc : cds) {
        if (cc.hasText()) {
          sb.append(
              CdaGeneratorUtils.getXmlForNullCDWithText(
                  cdName, CdaGeneratorConstants.NF_OTH, cc.getText()));
        }
      }
    }

    return sb.toString();
  }

  public static String getCodingXmlForCodeSystem(
      List<Coding> codes,
      String cdName,
      String codeSystemUrl,
      Boolean csOptional,
      String contentRef) {

    StringBuilder sb = new StringBuilder(200);
    StringBuilder translations = new StringBuilder(200);

    Boolean foundCodeForCodeSystem = false;

    if (codes != null && !codes.isEmpty()) {

      for (Coding c : codes) {

        Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

        if (!csd.getValue0().isEmpty()
            && (c.getSystem().contentEquals(codeSystemUrl)
                || c.getSystem().contains(csd.getValue0()))
            && Boolean.FALSE.equals(foundCodeForCodeSystem)) {

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

        // If we dont find the preferred code system, then add NF of OTH along with
        // translations.
        sb.append(
            CdaGeneratorUtils.getXmlForNullCDWithoutEndTag(cdName, CdaGeneratorConstants.NF_OTH));
      }

      logger.debug(" Sb = {}", sb);
      sb.append(translations);
      sb.append(CdaGeneratorUtils.getXmlForEndElement(cdName));

    } else {
      sb.append(CdaGeneratorUtils.getXmlForNullCD(cdName, CdaGeneratorConstants.NF_NI));
    }

    if (Boolean.TRUE.equals(foundCodeForCodeSystem) || Boolean.FALSE.equals(csOptional)) {
      return sb.toString();
    } else {
      return new StringBuilder("").toString();
    }
  }

  public static String getCodingXml(List<Coding> codes, String cdName, String contentRef) {

    StringBuilder sb = new StringBuilder(200);

    if (codes != null && !codes.isEmpty()) {

      Boolean first = true;
      Boolean found = false;
      for (Coding c : codes) {

        if (Boolean.TRUE.equals(first)) {

          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

          if (!csd.getValue0().isEmpty() && !csd.getValue1().isEmpty()) {
            first = false;
            found = true;
            sb.append(
                CdaGeneratorUtils.getXmlForCDWithoutEndTag(
                    cdName, c.getCode(), csd.getValue0(), csd.getValue1(), c.getDisplay()));

            if (!contentRef.isEmpty())
              sb.append(CdaGeneratorUtils.getXmlForOriginalTextWithReference(contentRef));
          }
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
      if (!sb.toString().isEmpty() && found)
        sb.append(CdaGeneratorUtils.getXmlForEndElement(cdName));
      else sb.append(CdaGeneratorUtils.getXmlForNullCD(cdName, CdaGeneratorConstants.NF_NI));
    } else {
      sb.append(CdaGeneratorUtils.getXmlForNullCD(cdName, CdaGeneratorConstants.NF_NI));
    }

    return sb.toString();
  }

  public static String getCodingXmlForMappedConceptDomain(
      String conceptDomain, List<Coding> codes, String cdName, Boolean includeNullFlavor) {

    StringBuilder sb = new StringBuilder();

    if (codes != null && !codes.isEmpty()) {

      Boolean first = true;
      Boolean cdStarted = false;
      for (Coding c : codes) {

        if (Boolean.TRUE.equals(first) || Boolean.FALSE.equals(cdStarted)) {

          first = false;
          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());
          String mappedCd =
              CdaGeneratorConstants.getMappedCodeFromFhirToCda(conceptDomain, c.getCode());

          if (!csd.getValue0().isEmpty()
              && !csd.getValue1().isEmpty()
              && mappedCd != null
              && !mappedCd.isEmpty()) {
            cdStarted = true;
            sb.append(
                CdaGeneratorUtils.getXmlForCDWithoutEndTag(
                    cdName, mappedCd, csd.getValue0(), csd.getValue1(), c.getDisplay()));
          }
        } else if (Boolean.TRUE.equals(cdStarted)) {

          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());
          String mappedCode =
              CdaGeneratorConstants.getMappedCodeFromFhirToCda(conceptDomain, c.getCode());

          if (!csd.getValue1().isEmpty()
              && !csd.getValue0().isEmpty()
              && mappedCode != null
              && !mappedCode.isEmpty())
            sb.append(
                CdaGeneratorUtils.getXmlForCD(
                    CdaGeneratorConstants.TRANSLATION_EL_NAME,
                    mappedCode,
                    csd.getValue0(),
                    csd.getValue1(),
                    c.getDisplay()));
        }
      }

      // At cd started...close the tag
      if (Boolean.TRUE.equals(cdStarted)) sb.append(CdaGeneratorUtils.getXmlForEndElement(cdName));

    } else if (Boolean.TRUE.equals(includeNullFlavor)) {
      sb.append(CdaGeneratorUtils.getXmlForNullCD(cdName, CdaGeneratorConstants.NF_NI));
    }

    // else it will be an empty string

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
            && Boolean.FALSE.equals(foundCodeForCodeSystem)) {

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

        // If we dont find the preferred code system, then add NF of OTH along with
        // translations.
        sb.append(
            CdaGeneratorUtils.getXmlForNullValueCDWithoutEndTag(
                cdName, CdaGeneratorConstants.NF_OTH));
      }

      sb.append(translations);
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.VAL_EL_NAME));

    } else {
      sb.append(CdaGeneratorUtils.getXmlForNullValueCD(cdName, CdaGeneratorConstants.NF_NI));
    }

    if (Boolean.TRUE.equals(foundCodeForCodeSystem) || Boolean.FALSE.equals(csOptional)) {
      return sb.toString();
    } else {
      return new StringBuilder("").toString();
    }
  }

  public static String getCodingXmlForValue(List<Coding> codes, String cdName, String contentRef) {

    StringBuilder sb = new StringBuilder(200);

    if (!codes.isEmpty()) {

      Boolean first = true;
      Boolean found = false;
      for (Coding c : codes) {

        if (Boolean.TRUE.equals(first)) {

          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

          if (!csd.getValue0().isEmpty() && !csd.getValue1().isEmpty()) {
            first = false;
            found = true;
            sb.append(
                CdaGeneratorUtils.getXmlForValueCDWithoutEndTag(
                    c.getCode(), csd.getValue0(), csd.getValue1(), c.getDisplay()));

            if (contentRef != null && !contentRef.isEmpty())
              sb.append(CdaGeneratorUtils.getXmlForOriginalTextWithReference(contentRef));
          }

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
      if (found && !sb.toString().isEmpty())
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.VAL_EL_NAME));
      else sb.append(CdaGeneratorUtils.getXmlForNullValueCD(cdName, CdaGeneratorConstants.NF_NI));
    } else {
      sb.append(CdaGeneratorUtils.getXmlForNullValueCD(cdName, CdaGeneratorConstants.NF_NI));
    }

    return sb.toString();
  }

  public static String getCodingXmlForValueForMappedConceptDomain(
      String conceptDomain, List<Coding> codes, String cdName, Boolean includeNullFlavor) {

    StringBuilder sb = new StringBuilder();

    if (codes != null && !codes.isEmpty()) {

      Boolean first = true;
      Boolean cdStarted = false;
      for (Coding c : codes) {

        if (Boolean.TRUE.equals(first) || Boolean.FALSE.equals(cdStarted)) {

          first = false;
          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());
          String mappedCode =
              CdaGeneratorConstants.getMappedCodeFromFhirToCda(conceptDomain, c.getCode());

          if (!csd.getValue0().isEmpty()
              && !csd.getValue1().isEmpty()
              && mappedCode != null
              && !mappedCode.isEmpty()) {
            cdStarted = true;
            sb.append(
                CdaGeneratorUtils.getXmlForValueCDWithoutEndTag(
                    mappedCode, csd.getValue0(), csd.getValue1(), c.getDisplay()));
          }
        } else if (Boolean.TRUE.equals(cdStarted)) {

          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());
          String mappedCode =
              CdaGeneratorConstants.getMappedCodeFromFhirToCda(conceptDomain, c.getCode());

          if (!csd.getValue0().isEmpty()
              && !csd.getValue1().isEmpty()
              && mappedCode != null
              && !mappedCode.isEmpty())
            sb.append(
                CdaGeneratorUtils.getXmlForCD(
                    CdaGeneratorConstants.TRANSLATION_EL_NAME,
                    mappedCode,
                    csd.getValue0(),
                    csd.getValue1(),
                    c.getDisplay()));
        }
      }

      // At least one code is there so...close the tag
      if (Boolean.TRUE.equals(cdStarted))
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.VAL_EL_NAME));
    } else if (Boolean.TRUE.equals(includeNullFlavor)) {
      sb.append(CdaGeneratorUtils.getXmlForNullValueCD(cdName, CdaGeneratorConstants.NF_NI));
    }

    return sb.toString();
  }

  public static String getDateTimeTypeXml(DateTimeType dt, String elName) {

    if (dt != null) {

      return CdaGeneratorUtils.getXmlForEffectiveTime(elName, dt.getValue(), dt.getTimeZone());
    } else {
      return CdaGeneratorUtils.getXmlForEffectiveTime(elName, null, null);
    }
  }

  public static String getDateTypeXml(DateType dt, String elName) {

    if (dt != null) {

      return CdaGeneratorUtils.getXmlForEffectiveTime(elName, dt.getValue(), null);
    } else {
      return CdaGeneratorUtils.getXmlForEffectiveTime(elName, null, null);
    }
  }

  public static String getDisplayStringForDateTimeType(DateTimeType dt) {

    if (dt != null) {

      return CdaGeneratorUtils.getStringForDateTime(dt.getValue(), dt.getTimeZone());
    } else {
      return CdaGeneratorConstants.UNKNOWN_VALUE;
    }
  }

  public static String getPeriodXml(Period period, String elName, Boolean valFlag) {

    StringBuilder sb = new StringBuilder(200);

    if (period != null) {

      if (valFlag) {

        sb.append(
            CdaGeneratorConstants.START_XMLTAG
                + elName
                + CdaGeneratorConstants.SPACE
                + CdaGeneratorConstants.XSI_TYPE
                + CdaGeneratorConstants.DOUBLE_QUOTE
                + CdaGeneratorConstants.IVL_TS_TYPE
                + CdaGeneratorConstants.DOUBLE_QUOTE
                + CdaGeneratorConstants.RIGHT_ANGLE_BRACKET);

        if (period.hasStart())
          sb.append(
              CdaGeneratorUtils.getXmlForEffectiveTime(
                  CdaGeneratorConstants.TIME_LOW_EL_NAME,
                  period.getStart(),
                  period.getStartElement().getTimeZone()));
        else
          sb.append(
              CdaGeneratorUtils.getXmlForEffectiveTime(
                  CdaGeneratorConstants.TIME_LOW_EL_NAME, null, null));

        if (period.hasEnd())
          sb.append(
              CdaGeneratorUtils.getXmlForEffectiveTime(
                  CdaGeneratorConstants.TIME_HIGH_EL_NAME,
                  period.getEnd(),
                  period.getEndElement().getTimeZone()));
        else
          sb.append(
              CdaGeneratorUtils.getXmlForEffectiveTime(
                  CdaGeneratorConstants.TIME_HIGH_EL_NAME, null, null));

        sb.append(CdaGeneratorUtils.getXmlForEndElement(elName));

      } else {
        sb.append(CdaGeneratorUtils.getXmlForStartElement(elName));

        sb.append(
            CdaFhirUtilities.getDateTimeTypeXml(
                period.getStartElement(), CdaGeneratorConstants.TIME_LOW_EL_NAME));

        sb.append(
            CdaFhirUtilities.getDateTimeTypeXml(
                period.getEndElement(), CdaGeneratorConstants.TIME_HIGH_EL_NAME));

        sb.append(CdaGeneratorUtils.getXmlForEndElement(elName));
      }

    } else if (!valFlag) {
      sb.append(CdaGeneratorUtils.getXmlForNullEffectiveTime(elName, CdaGeneratorConstants.NF_NI));
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForValueIVLWithTS(
              elName, CdaGeneratorConstants.UNKNOWN_VALUE, CdaGeneratorConstants.UNKNOWN_VALUE));
    }

    return sb.toString();
  }

  public static String getQuantityXml(Quantity dt, String elName, Boolean valFlag) {

    StringBuilder sb = new StringBuilder(200);

    if (dt != null && dt.hasValue() && dt.getValue() != null) {

      String units = "";

      if (dt.hasCode()) {
        units = dt.getCode();
      } else if (units.isEmpty() && dt.hasUnit()) {
        units = dt.getUnit();
      }

      sb.append(
          CdaGeneratorUtils.getXmlForQuantityWithUnits(
              elName, dt.getValue().toString(), units, valFlag));

    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForNfQuantity(elName, CdaGeneratorConstants.NF_NI, valFlag));
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

      logger.debug("Did not find the birth sex for the patient ");
      s +=
          CdaGeneratorUtils.getXmlForNullValueCD(
              CdaGeneratorConstants.VAL_EL_NAME, CdaGeneratorConstants.NF_NI);
    }

    return s;
  }

  public static String getMaritalStatusXml(CodeableConcept cd) {

    String s = "";

    s =
        getSingleCodingXmlForCodeSystem(
            cd,
            CdaGeneratorConstants.MARITAL_STATUS_CODE_EL_NAME,
            CdaGeneratorConstants.FHIR_MARITAL_STATUS_URL);

    return s;
  }

  public static String getReligiousAffiliationXml(Coding cd) {

    String s = "";

    s =
        getSingleCodingXml(
            cd,
            CdaGeneratorConstants.RELIGION_CODE_EL_NAME,
            CdaGeneratorConstants.FHIR_RELIGIOUS_AFFILIATION_URL);

    return s;
  }

  public static String getSingleCodingXmlForCodeSystem(
      CodeableConcept cd, String elName, String csUrl) {

    String s = "";

    if (cd != null) {

      Coding c = getCodingForCodeSystem(cd, csUrl);

      if (c != null && c.hasSystem()) {

        s = getSingleCodingXml(c, elName, csUrl);
      }
    }
    return s;
  }

  public static Coding getSingleCodingForCodeSystems(List<CodeableConcept> cds, String csUrl) {
    if (cds == null || cds.isEmpty()) {
      return null;
    }

    for (CodeableConcept cd : cds) {
      Coding c = getCodingForCodeSystem(cd, csUrl);
      if (c != null && c.hasSystem()) {
        return c;
      }
    }

    return null;
  }

  public static String getSingleCodingXml(Coding c, String elName, String csUrl) {

    String s = "";

    if (c != null && c.hasSystem() && c.hasCode()) {

      Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

      if (!csd.getValue0().isEmpty() && !csd.getValue1().isEmpty()) {
        s =
            CdaGeneratorUtils.getXmlForCD(
                elName, c.getCode(), csd.getValue0(), csd.getValue1(), c.getDisplay());
      }
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
              CdaGeneratorConstants.ADMIN_GEN_CODE_SYSTEM,
              CdaGeneratorConstants.ADMIN_GEN_CODE_SYSTEM_NAME,
              CdaGeneratorConstants.CDA_MALE_CODE_DISPLAY);
    } else if (gender == AdministrativeGender.FEMALE) {

      s +=
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.ADMIN_GENDER_CODE_EL_NAME,
              CdaGeneratorConstants.CDA_FEMALE_CODE,
              CdaGeneratorConstants.ADMIN_GEN_CODE_SYSTEM,
              CdaGeneratorConstants.ADMIN_GEN_CODE_SYSTEM_NAME,
              CdaGeneratorConstants.CDA_FEMALE_CODE_DISPLAY);
    } else if (gender == AdministrativeGender.UNKNOWN) {

      s +=
          CdaGeneratorUtils.getXmlForNullCD(
              CdaGeneratorConstants.ADMIN_GENDER_CODE_EL_NAME, CdaGeneratorConstants.NF_UNK);

    } else if (gender != null) {

      s +=
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.ADMIN_GENDER_CODE_EL_NAME,
              CdaGeneratorConstants.CDA_UNK_GENDER,
              CdaGeneratorConstants.ADMIN_GEN_CODE_SYSTEM,
              CdaGeneratorConstants.ADMIN_GEN_CODE_SYSTEM_NAME,
              CdaGeneratorConstants.CDA_UNK_GENDER_DISPLAY);
    } else {

      logger.debug("Did not find the gender for the patient ");
      s +=
          CdaGeneratorUtils.getXmlForNullCD(
              CdaGeneratorConstants.ADMIN_GENDER_CODE_EL_NAME, CdaGeneratorConstants.NF_NI);
    }

    return s;
  }

  public static String getNameXml(List<HumanName> allNames) {

    StringBuilder nameString = new StringBuilder(200);

    List<HumanName> names = new ArrayList<>();
    if (allNames != null && !allNames.isEmpty()) {

      for (HumanName n : allNames) {

        // Add name which is not expired
        if (!n.hasPeriod()) {
          names.add(n); // No period = active
        } else if (n.hasPeriod() && !n.getPeriod().hasEnd()) {
          names.add(n); // No end = active
        }
      }

      if (names == null || names.isEmpty()) {
        // All are expired so use whatever names were passed in
        names = allNames;
      }
    }

    if (names != null && !names.isEmpty()) {

      Optional<HumanName> hName = names.stream().findFirst();
      if (hName.isPresent()) {

        HumanName name = hName.get();
        List<StringType> ns = name.getGiven();

        for (StringType n : ns) {

          if (!StringUtils.isEmpty(n.getValue())) {

            String nameQualifier = null;
            if (name.getUse() != null) {
              nameQualifier = CdaGeneratorConstants.getCodeForNameQualifier(name.getUse().toCode());
            }

            nameString.append(
                CdaGeneratorUtils.getXmlForTextWithAttribute(
                    CdaGeneratorConstants.FIRST_NAME_EL_NAME,
                    CdaGeneratorConstants.QUALIFIER_ATTR_NAME,
                    nameQualifier,
                    n.getValue()));
          }
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

      logger.debug("Did not find the Name for the patient ");
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

  public static String getStringForCodings(List<Coding> cds) {

    String val = "";
    if (cds != null) {
      Boolean first = true;
      for (Coding c : cds) {

        if (first) {
          val += getStringForCoding(c);
        } else {
          val += " | " + getStringForCoding(c);
        }
        first = false;
      }
    }

    return val;
  }

  public static String getStringForCodeableConcept(CodeableConcept cd) {

    String val = "";
    if (cd != null) {

      if (!StringUtils.isEmpty(cd.getText())) {
        val += cd.getText();
      } else if (cd.hasCoding()) {
        val += getStringForCodings(cd.getCoding());
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
        && qt.hasValueElement()
        && qt.hasSystemElement()
        && (qt.hasUnit() || qt.hasCode())) {

      String units = (qt.hasCode() ? qt.getCode() : CdaGeneratorConstants.UNKNOWN_VALUE);

      if (units.contentEquals(CdaGeneratorConstants.UNKNOWN_VALUE) && qt.hasUnit()) {
        units = qt.getUnit();
      }

      val +=
          qt.getValueElement().getValueAsString()
              + CdaGeneratorConstants.PIPE
              + qt.getSystemElement().getValueAsString()
              + CdaGeneratorConstants.PIPE
              + units;
    } else if (qt != null && qt.hasValueElement()) {
      val += qt.getValueElement().getValueAsString();
    } else {
      val += CdaGeneratorConstants.UNKNOWN_VALUE;
    }

    return val;
  }

  public static String getStringForMedicationFromContainedResources(
      List<Resource> resources, String refId) {

    Pair<String, Boolean> retVal = null;
    for (Resource res : resources) {

      logger.debug("res.getId {}", res.getIdElement().getIdPart());

      if (res.getId().contains(refId) && res instanceof Medication) {

        logger.debug("Found a Contained Resource with Id {}", refId);
        Medication cmed = (Medication) res;
        // Found the reference, check the code and ingredients.

        if (cmed.getCode() != null) {
          logger.debug("Found Contained Med  Code");
          retVal =
              getCodeableConceptDisplayForCodeSystem(
                  cmed.getCode(), CdaGeneratorConstants.FHIR_RXNORM_URL, false);
        } // if code present

        if (retVal.getValue0().isEmpty()) {

          logger.debug("Return Val is empty");

          if (cmed.getIngredient() != null) {

            logger.debug("Found ingredient");
            List<MedicationIngredientComponent> ings = cmed.getIngredient();

            for (MedicationIngredientComponent ing : ings) {

              if (ing.getItem() instanceof CodeableConcept) {

                logger.debug("Found a CC for Ingredient");
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

  public static String getStringForMedicationType(Resource r, List<Medication> medList) {

    String retVal = CdaGeneratorConstants.UNKNOWN_VALUE;

    if (r instanceof MedicationRequest) {
      logger.debug("Found Med Request ");
      MedicationRequest mr = (MedicationRequest) r;

      if (mr.getMedication() instanceof Reference) {

        logger.debug("Found Med Request.Medication Reference ");

        Reference med = (Reference) mr.getMedication();

        if (med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {

          logger.debug("Found Med Request.Medication which is a contained reference");
          // Check contained.
          String refId = med.getReference().substring(1);

          logger.debug("Ref Id {} ", refId);

          if (mr.getContained() != null) {

            retVal = getStringForMedicationFromContainedResources(mr.getContained(), refId);

            logger.debug("Return Val = {}", retVal);
          } // contained present
        } // Contained reference
        else {
          // Handle actual reference
          logger.debug(" Checking medication references ");
          // check if the medications have been extracted for non contained references.
          if (medList != null && !medList.isEmpty()) {

            String id = med.getReferenceElement().getIdPart();
            Medication medRes = null;
            for (Medication m : medList) {
              if (m.getIdElement().getIdPart().contentEquals(id)) {

                logger.info(" Found the non-contained medication reference resource {}", id);
                medRes = m;
                break;
              }
            }

            // Found the reference, check the code and ingredients.
            if (medRes != null && medRes.hasCode()) {
              retVal = getStringForType(medRes.getCode());
            }
          }
        }

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
        else {
          // Handle actual reference
          logger.debug(" Checking medication references ");
          // check if the medications have been extracted for non contained references.
          if (medList != null && !medList.isEmpty()) {

            String id = med.getReferenceElement().getIdPart();
            Medication medRes = null;
            for (Medication m : medList) {
              if (m.getIdElement().getIdPart().contentEquals(id)) {

                logger.info(" Found the non-contained medication reference resource {}", id);
                medRes = m;
                break;
              }
            }

            // Found the reference, check the code and ingredients.
            if (medRes != null && medRes.hasCode()) {
              retVal = getStringForType(medRes.getCode());
            }
          }
        }

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
        else {
          // Handle actual reference
          logger.debug(" Checking medication references ");
          // check if the medications have been extracted for non contained references.
          if (medList != null && !medList.isEmpty()) {

            String id = med.getReferenceElement().getIdPart();
            Medication medRes = null;
            for (Medication m : medList) {
              if (m.getIdElement().getIdPart().contentEquals(id)) {

                logger.info(" Found the non-contained medication reference resource {}", id);
                medRes = m;
                break;
              }
            }

            // Found the reference, check the code and ingredients.
            if (medRes != null && medRes.hasCode()) {
              retVal = getStringForType(medRes.getCode());
            }
          }
        }

        return retVal;

      } else if (medStmtRef.getMedication() instanceof CodeableConcept) {

        CodeableConcept cc = (CodeableConcept) medStmtRef.getMedication();

        return getStringForType(cc);
      }
    }

    return retVal;
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

            if (Boolean.FALSE.equals(first)) {

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

        val.append(CdaGeneratorUtils.getStringForDateTime(d.getValue(), d.getTimeZone()));

      } else if (dt instanceof Timing) {

        logger.debug("Found an instance of timing for creating string ");
        Timing t = (Timing) (dt);
        if (t.getRepeat() != null && t.getRepeat().getBounds() != null) {

          logger.debug("Found the bounds element for creating string ");

          String v = getStringForType(t.getRepeat().getBounds());
          val.append(v);
        }

      } else if (dt instanceof Period) {
        Period pt = (Period) dt;

        logger.debug("Found the Period element for creating string");
        if (pt.hasStart() && pt.hasEnd()) {

          val.append(
                  CdaGeneratorUtils.getStringForDateTime(
                      pt.getStart(), pt.getStartElement().getTimeZone()))
              .append(CdaGeneratorConstants.PIPE)
              .append(
                  CdaGeneratorUtils.getStringForDateTime(
                      pt.getEnd(), pt.getEndElement().getTimeZone()));
        } else if (pt.hasStart()) {
          val.append(
              CdaGeneratorUtils.getStringForDateTime(
                  pt.getStart(), pt.getStartElement().getTimeZone()));
        } else if (pt.hasEnd()) {
          val.append(
              CdaGeneratorUtils.getStringForDateTime(
                  pt.getEnd(), pt.getEndElement().getTimeZone()));
        } else {
          val.append(CdaGeneratorConstants.UNKNOWN_VALUE);
        }
      } else if (dt instanceof CodeType) {

        CodeType cd = (CodeType) dt;

        val.append(cd.getValue());
      } else if (dt instanceof StringType) {

        StringType st = (StringType) dt;

        val.append(st.getValue());
      } else if (dt instanceof BooleanType) {

        BooleanType b = (BooleanType) dt;

        String ret = "false";
        if (b.getValueAsString().equalsIgnoreCase("true")) {

          ret = "true";
        }

        val.append(ret);
      }

      logger.debug("Printing the class name {} and value {}", dt.getClass(), val);
      return val.toString();
    }
    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  public static String getStringForDates(
      Pair<Date, TimeZone> onset, Pair<Date, TimeZone> abatement, Pair<Date, TimeZone> recorded) {

    StringBuilder val = new StringBuilder();

    if (recorded != null && recorded.getValue0() != null) {
      val.append(recorded.getValue0().toString());
    } else {
      val.append(CdaGeneratorConstants.UNKNOWN_VALUE);
    }

    if (onset != null && onset.getValue0() != null) {
      val.append('|').append(onset.getValue0().toString());
    } else {
      val.append('|').append(CdaGeneratorConstants.UNKNOWN_VALUE);
    }

    if (abatement != null && abatement.getValue0() != null) {
      val.append('|').append(abatement.getValue0().toString());
    } else {
      val.append('|').append(CdaGeneratorConstants.UNKNOWN_VALUE);
    }

    return val.toString();
  }

  public static String getXmlForType(Type dt, String elName, Boolean valFlag) {

    String val = "";
    if (dt != null && !dt.hasExtension(CdaGeneratorConstants.FHIR_DATA_ABSENT_REASON_EXT_URL)) {

      if (dt instanceof Coding) {
        Coding cd = (Coding) dt;

        List<Coding> cds = new ArrayList<>();
        cds.add(cd);

        if (Boolean.FALSE.equals(valFlag)) val += getCodingXml(cds, elName, "");
        else val += getCodingXmlForValue(cds, elName, null);

      } else if (dt instanceof CodeableConcept) {

        CodeableConcept cd = (CodeableConcept) dt;
        Boolean textFound = false;
        List<Coding> cds = new ArrayList<>();

        if (cd.hasCoding()) {
          cds.addAll(cd.getCoding());
        } else if (cd.hasText() && valFlag) {
          // Add Value Result as String if there is soemthing in the CodeableConcept
          val += CdaGeneratorUtils.getXmlForValueString(cd.getText());
          textFound = true;
        }

        if (!textFound) {
          if (Boolean.FALSE.equals(valFlag)) {
            val += getCodingXml(cds, elName, "");
          } else val += getCodingXmlForValue(cds, elName, null);
        }

      } else if (dt instanceof Quantity) {

        Quantity qt = (Quantity) dt;

        val += getQuantityXml(qt, elName, valFlag);

      } else if (dt instanceof DateTimeType) {

        DateTimeType d = (DateTimeType) dt;

        if (Boolean.FALSE.equals(valFlag))
          val += CdaGeneratorUtils.getXmlForEffectiveTime(elName, d.getValue(), d.getTimeZone());
        else
          val +=
              CdaGeneratorUtils.getXmlForValueEffectiveTime(elName, d.getValue(), d.getTimeZone());

      } else if (dt instanceof Period) {
        Period pt = (Period) dt;

        val += getPeriodXml(pt, elName, valFlag);
      } else if (dt instanceof Timing) {

        Timing t = (Timing) (dt);
        if (t.getRepeat() != null && t.getRepeat().getBounds() != null) {

          logger.debug("Found the bounds element for creating xml");

          String v = getXmlForType(t.getRepeat().getBounds(), elName, valFlag);
          val += v;
        }
      } else if (dt instanceof CodeType) {

        CodeType cd = (CodeType) dt;
        if (Boolean.FALSE.equals(valFlag))
          val += CdaGeneratorUtils.getXmlForCD(elName, cd.getCode());
        else val += CdaGeneratorUtils.getXmlForValueString(cd.getCode());
      } else if (dt instanceof StringType) {

        StringType st = (StringType) dt;
        if (Boolean.FALSE.equals(valFlag))
          val += CdaGeneratorUtils.getXmlForText(elName, st.getValue());
        else val += CdaGeneratorUtils.getXmlForValueString(st.getValue());
      } else if (dt instanceof BooleanType) {

        BooleanType b = (BooleanType) dt;

        String ret = "false";
        if (b.getValueAsString().equalsIgnoreCase("true")) {

          val += CdaGeneratorUtils.getXmlForValueString("true");
        } else {
          val += CdaGeneratorUtils.getXmlForValueString("false");
        }
      }

      logger.debug(PRINTING_THE_CLASS_NAME, dt.getClass());
      return val;
    }

    if (Boolean.FALSE.equals(valFlag))
      val += CdaGeneratorUtils.getNFXMLForElement(elName, CdaGeneratorConstants.NF_NI);
    else val += CdaGeneratorUtils.getXmlForValueString(CdaGeneratorConstants.NO_VALUE);

    return val;
  }

  public static String getXmlForTypeForValueIvlTsEffectiveTime(String elName, Type dt) {

    String val = "";
    if (dt != null) {

      if (dt instanceof DateTimeType) {

        DateTimeType d = (DateTimeType) dt;

        val += CdaGeneratorUtils.getXmlForEffectiveTime(elName, d.getValue(), d.getTimeZone());

      } else if (dt instanceof Period) {
        Period pt = (Period) dt;

        val += getPeriodXml(pt, elName, false);
      } else if (dt instanceof Timing) {

        Timing t = (Timing) (dt);
        if (t.getRepeat() != null && t.getRepeat().getBounds() != null) {

          logger.debug("Found the bounds element for creating xml");
        }
      }

      logger.debug(PRINTING_THE_CLASS_NAME, dt.getClass());
      return val;
    }

    return val;
  }

  public static String getXmlForMedicationTypeForCodeSystem(
      Type dt,
      String elName,
      Boolean valFlag,
      String codeSystemUrl,
      Boolean csOptional,
      DomainResource res,
      List<Medication> medList) {

    if (dt instanceof Reference) {

      logger.info("Found Medication of Type Reference within Domain Resource");
      Reference med = (Reference) dt;
      String codeXml = "";
      if (med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {
        // Check contained.
        String refId = med.getReference().substring(1);

        logger.info("Found Medication of Type Reference with Id {}", refId);

        if (res.getContained() != null) {

          logger.info("Contained Elements Not null");
          List<Resource> meds = res.getContained();

          for (Resource r : meds) {

            if (r.getId().contains(refId) && r instanceof Medication) {

              logger.info("Found Medication in contained resource");

              Medication cmed = (Medication) r;

              // Found the reference, check the code and ingredients.
              codeXml = getXmlForMedication(cmed, elName, valFlag, codeSystemUrl, csOptional);
            } // contained med
          } // for all contained resources
        } // contained present

      } // Contained reference
      else {

        logger.info(" Checking medication references ");
        // check if the medications have been extracted for non contained references.
        if (medList != null && !medList.isEmpty()) {

          String id = med.getReferenceElement().getIdPart();
          Medication medRes = null;
          for (Medication m : medList) {
            if (m.getIdElement().getIdPart().contentEquals(id)) {

              logger.info(" Found the non-contained medication reference resource {}", id);
              medRes = m;
              break;
            }
          }

          // Found the reference, check the code and ingredients.
          if (medRes != null) {
            codeXml = getXmlForMedication(medRes, elName, valFlag, codeSystemUrl, csOptional);
          }
        }
      }

      return codeXml;

    } else return getXmlForTypeForCodeSystem(dt, elName, valFlag, codeSystemUrl, csOptional);
  }

  public static String getXmlForMedication(
      Medication cmed, String elName, Boolean valFlag, String codeSystemUrl, Boolean csOptional) {

    String codeXml = "";

    if (cmed.getCode() != null
        && cmed.getCode().getCoding() != null
        && !cmed.getCode().getCoding().isEmpty()
        && Boolean.TRUE.equals(
            CdaFhirUtilities.isCodingPresentForCodeSystem(
                cmed.getCode().getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

      logger.debug("Found Medication for code system in code element");
      // Found the Medication that matters.
      codeXml =
          getXmlForTypeForCodeSystem(cmed.getCode(), elName, valFlag, codeSystemUrl, csOptional);

    } // if code present
    else {
      // Check the ingredients

      if (cmed.getIngredient() != null) {

        logger.debug("Found Ingredients");
        List<MedicationIngredientComponent> ings = cmed.getIngredient();

        for (MedicationIngredientComponent ing : ings) {

          if (ing.getItem() instanceof CodeableConcept) {

            logger.debug("Found Ingredient which is coded");
            CodeableConcept cc = (CodeableConcept) ing.getItem();

            if (cc.getCoding() != null
                && !cc.getCoding().isEmpty()
                && Boolean.TRUE.equals(
                    CdaFhirUtilities.isCodingPresentForCodeSystem(
                        cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {
              codeXml = getXmlForTypeForCodeSystem(cc, elName, valFlag, codeSystemUrl, csOptional);
              break;
            }
          }
        }
      }
    }

    return codeXml;
  }

  public static String getXmlForTypeForCodeSystem(
      Type dt, String elName, Boolean valFlag, String codeSystemUrl, Boolean csOptional) {

    String val = "";
    if (dt != null) {

      if (dt instanceof Coding) {
        Coding cd = (Coding) dt;

        List<Coding> cds = new ArrayList<>();
        cds.add(cd);
        if (Boolean.FALSE.equals(valFlag))
          val += getCodingXmlForCodeSystem(cds, elName, codeSystemUrl, csOptional, "");
        else val += getCodingXmlForValueForCodeSystem(cds, elName, codeSystemUrl, csOptional);

      } else if (dt instanceof CodeableConcept) {

        CodeableConcept cd = (CodeableConcept) dt;

        List<Coding> cds = cd.getCoding();

        if (Boolean.FALSE.equals(valFlag))
          val += getCodingXmlForCodeSystem(cds, elName, codeSystemUrl, csOptional, "");
        else val += getCodingXmlForValueForCodeSystem(cds, elName, codeSystemUrl, csOptional);

      } else {

        if (Boolean.FALSE.equals(valFlag))
          val += CdaGeneratorUtils.getXmlForNullCD(elName, CdaGeneratorConstants.NF_NI);
        else val += CdaGeneratorUtils.getNFXMLForValue(CdaGeneratorConstants.NF_NI);
      }

      logger.debug(PRINTING_THE_CLASS_NAME, dt.getClass());
      return val;
    }

    if (Boolean.FALSE.equals(valFlag))
      val += CdaGeneratorUtils.getXmlForNullCD(elName, CdaGeneratorConstants.NF_NI);
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
      if (Boolean.TRUE.equals(mtc.hasMatchedTriggerCodes(matchResourceType))) {

        logger.info("Found Matched Codes for Resource Type {}", matchResourceType);

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
      if (Boolean.TRUE.equals(mtc.hasMatchedTriggerValue(matchResourceType))) {

        logger.debug("Found Matched Codes for Resource Type {}", matchResourceType);

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
            && Boolean.TRUE.equals(isCodePresent(matchedCodes, c.getCode()))) {

          logger.debug("Found the Coding for Codesystem {} and Code =  {}", csUrl, c.getCode());

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
      String csUrl,
      String contentRef,
      Boolean valueElem) {

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
              && Boolean.FALSE.equals(foundCodings)) {

            logger.debug(
                " Found a Coding that matches the CodeSystem and Code {} : {} ", codeSystem, code);
            if (cd.getDisplay() != null && !cd.getDisplay().isEmpty()) dispName = cd.getDisplay();

            if (Boolean.FALSE.equals(valueElem)) {
              retval.append(
                  CdaGeneratorUtils.getXmlForCDWithValueSetAndVersionWihoutEndTag(
                      elementName,
                      code,
                      codeSystem,
                      codeSystemName,
                      valueSet,
                      valuesetVersion,
                      dispName));

              if (!contentRef.isEmpty())
                retval.append(CdaGeneratorUtils.getXmlForOriginalTextWithReference(contentRef));

            } else if (Boolean.TRUE.equals(valueElem)) {

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

    if (Boolean.TRUE.equals(foundCodings)) {
      retval.append(translations.toString());
      retval.append(CdaGeneratorUtils.getXmlForEndElement(elementName));
    } else {

      String dispName = "";
      if (cc != null && cc.getText() != null && !cc.getText().isEmpty()) dispName = cc.getText();

      if (Boolean.FALSE.equals(valueElem)) {
        retval.append(
            CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
                CdaGeneratorConstants.CODE_EL_NAME,
                code,
                codeSystem,
                codeSystemName,
                valueSet,
                valuesetVersion,
                dispName,
                contentRef));
      } else if (Boolean.TRUE.equals(valueElem)) {
        retval.append(
            CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersion(
                code, codeSystem, codeSystemName, valueSet, valuesetVersion, dispName));
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
    } else if (val.equalsIgnoreCase("stopped") || val.equalsIgnoreCase("not-done")) {
      return "aborted";
    } else if (val.equalsIgnoreCase("on-hold")) {
      return "suspended";
    } else if (val.equalsIgnoreCase("unknown") || val.equalsIgnoreCase("draft")) {
      return "held";
    } else if (val.equalsIgnoreCase("cancelled")) {
      return "cancelled";
    } else return "completed";
  }

  public static String getCodeForNameUse(List<HumanName> names) {

    String nameUse = null;

    if (names != null && !names.isEmpty()) {

      Optional<HumanName> hName = names.stream().findFirst();
      if (hName.isPresent()) {

        HumanName name = hName.get();

        if (name.getUse() != null) {
          nameUse = CdaGeneratorConstants.getCodeForNameUse(name.getUse().toCode());
        }
      }
    }

    return nameUse;
  }

  public static Address getAddressExtensionValue(List<Extension> extensions, String extensionUrl) {
    if (extensions == null || extensions.isEmpty()) {
      return null;
    }

    for (Extension extension : extensions) {
      if (extension.hasUrl()
          && extension.getUrl().equals(extensionUrl)
          && extension.hasValue()
          && extension.getValue() instanceof Address) {
        logger.debug("Found Address Extension at top level.");
        return (Address) extension.getValue();
      }
    }
    logger.debug("Did not find the Extension or sub extensions for the Url {}", extensionUrl);
    return null;
  }

  public static String getTravelHistoryAddressXml(Address addr) {
    StringBuilder addrString = new StringBuilder(200);

    if (addr != null) {

      addrString.append(
          CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ADDR_EL_NAME));

      if (addr.hasCountry()) {
        addrString.append(
            CdaGeneratorUtils.getXmlForText(
                CdaGeneratorConstants.COUNTRY_EL_NAME, addr.getCountry()));
      } else {
        addrString.append(
            CdaGeneratorUtils.getXmlForNFText(
                CdaGeneratorConstants.COUNTRY_EL_NAME, CdaGeneratorConstants.NF_NI));
      }

      if (addr.hasCity()) {
        addrString.append(
            CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.CITY_EL_NAME, addr.getCity()));
      }

      if (addr.hasState()) {
        addrString.append(
            CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.STATE_EL_NAME, addr.getState()));
      }

      addrString.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ADDR_EL_NAME));
    }
    return addrString.toString();
  }

  public static String getSingleCodingXmlFromCodings(List<Coding> coding, String elName) {

    StringBuilder addrString = new StringBuilder(200);

    StringBuilder altXml = new StringBuilder(200);

    if (coding != null && !coding.isEmpty()) {

      for (Coding c : coding) {

        String xml = getSingleCodingXml(c, elName, "");

        if (!xml.isEmpty()) {
          addrString.append(xml);
          break;
        }
      }
    }

    return addrString.toString();
  }

  public static Object getPerformerXml(Practitioner pract, String functionCode) {

    StringBuilder s = new StringBuilder(200);

    if (pract != null) {

      s.append(
          CdaGeneratorUtils.getXmlForStartElementWithTypeCode(
              CdaGeneratorConstants.PERF_EL_NAME, CdaGeneratorConstants.DEFAULT_PERF_EL_TYPE_CODE));

      s.append(
          CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSIGNED_ENTITY_EL_NAME));

      s.append(getPractitionerXml(pract));
      s.append(
          CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_ENTITY_EL_NAME));
      s.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PERF_EL_NAME));
    }

    return s;
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

      sb.append(CdaFhirUtilities.getAddressXml(pr.getAddress(), false));
      sb.append(CdaFhirUtilities.getTelecomXml(pr.getTelecom(), false));

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
      sb.append(CdaFhirUtilities.getAddressXml(addrs, false));

      List<ContactPoint> cps = null;
      sb.append(CdaFhirUtilities.getTelecomXml(cps, false));

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

  public static String getStringForObservationsWithComponents(Observation obs) {

    String result = "";

    if (obs != null) {
      // Get the text from the code
      result += getStringForCodeableConcept(obs.getCode());

      if (obs.hasValue()) {
        result += " | " + " Value : " + getStringForType(obs.getValue());
      }

      if (obs.hasComponent()) {

        List<ObservationComponentComponent> comps = obs.getComponent();

        int i = 1;
        for (ObservationComponentComponent comp : comps) {

          result +=
              " Component "
                  + Integer.toString(i)
                  + " : "
                  + getStringForCodeableConcept(comp.getCode());

          if (comp.hasValue()) {

            result += " | " + " Value : " + getStringForType(comp.getValue());
          }

          i++;
        }
      }
    }

    return result;
  }
}
