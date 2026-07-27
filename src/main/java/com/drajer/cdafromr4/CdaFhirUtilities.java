package com.drajer.cdafromr4;

import static com.drajer.cda.utils.CdaGeneratorConstants.FHIR_NPI_URL;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.TimeZone;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
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
import org.hl7.fhir.r4.model.Patient.ContactComponent;
import org.hl7.fhir.r4.model.Patient.PatientCommunicationComponent;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.Specimen;
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

  private static final String FOUND_EXTENSION_TOP_LEVEL = "Found Extension at top level ";
  private static final String FOUND_EXTENSION_NESTED = "Found Extension nested as children ";
  private static final String EXT_NOT_FOUND_MSG =
      "Did not find the Extension or sub extensions for the Url {}";
  private static final String FOUND_ADDRESS_EXTENSION_MSG = "Found Address Extension at top level.";
  private static final String CHECKING_MED_REFS_MSG = " Checking medication references ";
  private static final String FOUND_NON_CONTAINED_MED_MSG =
      " Found the non-contained medication reference resource {}";
  private static final String FALSE_VALUE = "false";
  private static final String TRUE_VALUE = "true";

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

  /**
   * Checks if text matches guardian type.
   *
   * @param text the text to check
   * @return true if text is a guardian type
   */
  private static boolean isGuardianText(String text) {
    return text != null
        && (text.equalsIgnoreCase(CdaGeneratorConstants.GUARDIAN_EL_NAME)
            || text.equalsIgnoreCase(CdaGeneratorConstants.GUARDIAN_PERSON_EL_NAME));
  }

  /**
   * Checks if coding system is valid for guardian.
   *
   * @param system the system to check
   * @return true if system is valid for guardian
   */
  private static boolean isValidGuardianSystem(String system) {
    return system != null
        && (system.equals(CdaGeneratorConstants.FHIR_CONTACT_RELATIONSHIP_CODESYSTEM)
            || system.equals(CdaGeneratorConstants.DSTU2_FHIR_CONTACT_RELATIONSHIP_CODESYSTEM)
            || system.equals(CdaGeneratorConstants.FHIR_LOC_ROLE_CODE_TYPE_V3));
  }

  /**
   * Checks if coding code is a guardian code.
   *
   * @param code the code to check
   * @return true if code is a guardian code
   */
  private static boolean isGuardianCode(String code) {
    return code != null
        && (code.equals(CdaGeneratorConstants.GUARDIAN_VALUE)
            || code.equals(CdaGeneratorConstants.GUARDIAN_EL_NAME)
            || code.equals(CdaGeneratorConstants.GUARDIAN_PERSON_EL_NAME)
            || code.equals(CdaGeneratorConstants.FHIR_GUARDIAN_VALUE)
            || code.equals(CdaGeneratorConstants.EMERGENCY_VALUE)
            || code.equals(CdaGeneratorConstants.FHIR_EMERGENCY_CONTACT_VALUE));
  }

  /**
   * Processes coding and adds contact to list if guardian.
   *
   * @param coding the coding to process
   * @param cc the contact to add if guardian
   * @param guardianContacts the list to add to
   */
  private static void processCodingForGuardian(
      Coding coding, ContactComponent cc, List<ContactComponent> guardianContacts) {
    if (coding.hasSystem()
        && coding.hasCode()
        && isValidGuardianSystem(coding.getSystem())
        && isGuardianCode(coding.getCode())) {
      guardianContacts.add(cc);
    }
  }

  public static List<ContactComponent> getGuardianContacts(List<ContactComponent> ccs) {
    List<ContactComponent> guardianContacts = new ArrayList<>();

    if (ccs != null && !ccs.isEmpty()) {
      for (ContactComponent cc : ccs) {
        if (cc.hasRelationship()) {
          for (CodeableConcept cd : cc.getRelationship()) {
            // Check Text field first
            if (isGuardianText(cd.getText())) {
              guardianContacts.add(cc);
            }

            // Check Codings
            for (Coding coding : cd.getCoding()) {
              processCodingForGuardian(coding, cc, guardianContacts);
            }
          }
        }
      }
    }
    return guardianContacts;
  }

  public static Identifier getIdentifierForSystem(List<Identifier> ids, String system) {

    if (StringUtils.isBlank(system)) {
      logger.debug("System is null : {}", system);
      return null;
    }
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

            logger.debug(FOUND_EXTENSION_TOP_LEVEL);
            return (Coding) ext.getValue();

          } else if (ext.getValue() == null) {

            // get child extensions.
            List<Extension> subExts = ext.getExtensionsByUrl(subextUrl);

            for (Extension subext : subExts) {

              if (subext.getValue() instanceof Coding) {

                logger.debug(FOUND_EXTENSION_NESTED);
                return (Coding) subext.getValue();
              }
            }
          }
        }
      }
    }

    logger.debug(EXT_NOT_FOUND_MSG, extUrl);
    return null;
  }

  public static String getStringExtension(List<Extension> exts, String extUrl, String subextUrl) {

    if (exts != null && !exts.isEmpty()) {

      for (Extension ext : exts) {

        if (ext.getUrl() != null && ext.getUrl().contentEquals(extUrl)) {

          // if the top level extension has Coding then we will use it.
          if (ext.getValue() instanceof StringType) {

            logger.debug(FOUND_EXTENSION_TOP_LEVEL);
            return ((StringType) ext.getValue()).getValue();

          } else if (ext.getValue() == null) {

            // get child extensions.
            List<Extension> subExts = ext.getExtensionsByUrl(subextUrl);

            for (Extension subext : subExts) {

              if (subext.getValue() instanceof StringType) {

                logger.debug(FOUND_EXTENSION_NESTED);
                return ((StringType) subext.getValue()).getValue();
              }
            }
          }
        }
      }
    }

    logger.debug(EXT_NOT_FOUND_MSG, extUrl);
    return null;
  }

  /**
   * Processes sub-extensions and adds codings to list.
   *
   * @param subExts the sub-extensions to process
   * @param codings the list to add codings to
   */
  private static void processSubExtensionsForCodings(
      List<Extension> subExts, List<Coding> codings) {
    for (Extension subext : subExts) {
      if (subext.hasValue()) {
        if (subext.getValue() instanceof Coding) {
          logger.debug(FOUND_EXTENSION_NESTED);
          codings.add((Coding) subext.getValue());
        } else if (subext.getValue() instanceof CodeableConcept) {
          processCodeableConceptForCodings((CodeableConcept) subext.getValue(), codings);
        }
      }
    }
  }

  /**
   * Processes codeable concept and adds codings to list.
   *
   * @param cd the codeable concept to process
   * @param codings the list to add codings to
   */
  private static void processCodeableConceptForCodings(CodeableConcept cd, List<Coding> codings) {
    if (cd.hasCoding()) {
      logger.debug("Found Extension nested as childrens ");
      codings.addAll(cd.getCoding());
    }
  }

  /**
   * Processes extension with given URL and adds codings to list.
   *
   * @param ext the extension to process
   * @param extUrl the URL to match
   * @param subextUrl the sub-extension URL
   * @param codings the list to add codings to
   */
  private static void processExtensionForCodings(
      Extension ext, String extUrl, String subextUrl, List<Coding> codings) {
    if (!ext.hasUrl() || !ext.getUrl().contentEquals(extUrl)) {
      return;
    }

    if (ext.hasValue() && ext.getValue() instanceof Coding) {
      logger.debug(FOUND_EXTENSION_TOP_LEVEL);
      codings.add((Coding) ext.getValue());
    } else if (!ext.hasValue()) {
      List<Extension> subExts = ext.getExtensionsByUrl(subextUrl);
      processSubExtensionsForCodings(subExts, codings);
    }
  }

  public static List<Coding> getAllCodingsFromExtension(
      List<Extension> exts, String extUrl, String subextUrl) {
    List<Coding> codings = new ArrayList<>();

    if (exts == null || exts.isEmpty()) {
      logger.debug("No extensions provided");
      return codings;
    }

    for (Extension ext : exts) {
      processExtensionForCodings(ext, extUrl, subextUrl, codings);
    }

    if (codings.isEmpty()) {
      logger.debug(EXT_NOT_FOUND_MSG, extUrl);
    }

    return codings;
  }

  public static Coding getCodingExtension(List<Extension> exts, String extUrl) {

    if (exts != null && !exts.isEmpty()) {

      for (Extension ext : exts) {

        if (ext.getUrl() != null && ext.getUrl().contentEquals(extUrl)) {

          // if the top level extension has Coding then we will use it.
          if (ext.getValue() instanceof Coding) {

            logger.debug(FOUND_EXTENSION_TOP_LEVEL);
            return (Coding) ext.getValue();
          }
        }
      }
    }

    logger.debug(EXT_NOT_FOUND_MSG, extUrl);
    return null;
  }

  public static CodeableConcept getCodeableConceptFromExtension(
      List<Extension> exts, String extUrl) {

    if (exts != null && !exts.isEmpty()) {

      for (Extension ext : exts) {

        if (ext.getUrl() != null && ext.getUrl().contentEquals(extUrl)) {

          // if the top level extension has Coding then we will use it.
          if (ext.getValue() instanceof CodeableConcept) {

            logger.debug("Found Extension ");
            return (CodeableConcept) ext.getValue();
          }
        }
      }
    }

    logger.debug("Did not find the Extension for the Url {}", extUrl);
    return null;
  }

  public static Extension getExtensionForUrl(List<Extension> exts, String extUrl) {

    if (exts != null && !exts.isEmpty()) {

      for (Extension ext : exts) {

        if (ext.getUrl() != null && ext.getUrl().contentEquals(extUrl)) {

          logger.debug("Found Extension ");
          return ext;
        }
      }
    }

    logger.debug("Did not find the Extension for the Url {}", extUrl);
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

          logger.debug(FOUND_EXTENSION_TOP_LEVEL);
          return (CodeType) ext.getValue();
        }
      }
    }

    logger.debug(EXT_NOT_FOUND_MSG, extUrl);
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
    StringBuilder altAddr = new StringBuilder(200);

    if (addrs != null && !addrs.isEmpty()) {

      if (includeMultiples) {
        Boolean found = false;
        for (Address addr : addrs) {

          if (addr.hasPeriod() && !addr.getPeriod().hasEnd()) {
            found = true;
            addrString.append(getAddressXml(addr));
          } else {
            altAddr.append(getAddressXml(addr));
          }
        }

        // Add an address if it was not found to have an end
        if (!found) {
          addrString.append(altAddr);
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

  /**
   * Processes phone contact point.
   *
   * @param tel the contact point
   * @param telString the string builder
   * @param onlyOne whether to process only one
   * @param isPhonePr whether phone is preferred
   * @return the phone entry if onlyOne and isPhonePr, null otherwise
   */
  private static String processPhoneContact(
      ContactPoint tel, StringBuilder telString, boolean onlyOne, boolean isPhonePr) {
    logger.debug("Found Telecom Number for {}", tel.getSystem().getDisplay());
    String use =
        (tel.getUse() == null)
            ? ""
            : CdaGeneratorConstants.getCodeForTelecomUse(tel.getUse().toCode());
    String telecomEntry =
        CdaGeneratorUtils.getXmlForTelecom(
            CdaGeneratorConstants.TEL_EL_NAME, tel.getValue(), use, false);
    telString.append(telecomEntry);
    if (onlyOne && isPhonePr) {
      return telecomEntry;
    }
    return null;
  }

  /**
   * Processes email contact point.
   *
   * @param tel the contact point
   * @param telString the string builder
   */
  private static void processEmailContact(ContactPoint tel, StringBuilder telString) {
    logger.debug("Found Email address ");
    telString.append(
        CdaGeneratorUtils.getXmlForEmail(CdaGeneratorConstants.TEL_EL_NAME, tel.getValue(), ""));
  }

  /**
   * Processes fax contact point.
   *
   * @param tel the contact point
   * @param telString the string builder
   */
  private static void processFaxContact(ContactPoint tel, StringBuilder telString) {
    logger.debug("Found Fax address ");
    telString.append(
        CdaGeneratorUtils.getXmlForTelecom(
            CdaGeneratorConstants.TEL_EL_NAME, tel.getValue(), "", true));
  }

  /**
   * Processes a single contact point based on its system type.
   *
   * @param tel the contact point to process
   * @param telString the string builder
   * @param onlyOne whether to process only one
   * @param isPhonePr whether phone is preferred
   * @return true if processing should stop, false otherwise
   */
  private static boolean processContactPoint(
      ContactPoint tel, StringBuilder telString, boolean onlyOne, boolean isPhonePr) {
    if (tel.getSystem() == null || StringUtils.isEmpty(tel.getValue())) {
      return false;
    }

    if (tel.getSystem() == ContactPoint.ContactPointSystem.PHONE) {
      String result = processPhoneContact(tel, telString, onlyOne, isPhonePr);
      if (result != null) {
        return true; // stop processing
      }
      return onlyOne && isPhonePr;
    } else if (tel.getSystem() == ContactPoint.ContactPointSystem.EMAIL) {
      processEmailContact(tel, telString);
      return onlyOne && !isPhonePr;
    } else if (tel.getSystem() == ContactPoint.ContactPointSystem.FAX) {
      processFaxContact(tel, telString);
      return onlyOne && !isPhonePr;
    }

    return false;
  }

  public static String getTelecomXml(List<ContactPoint> tels, boolean onlyOne, boolean isPhonePr) {
    StringBuilder telString = new StringBuilder(200);

    if (tels != null && !tels.isEmpty()) {
      for (ContactPoint tel : tels) {
        if (processContactPoint(tel, telString, onlyOne, isPhonePr)) {
          break;
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

  /**
   * Checks if coding system is valid for participant type.
   *
   * @param system the system to check
   * @return true if system is valid
   */
  private static boolean isValidParticipantCodeSystem(String system) {
    return system != null
        && (system.contentEquals(CdaGeneratorConstants.FHIR_PARTICIPANT_TYPE)
            || system.contentEquals(CdaGeneratorConstants.FHIR_PARTICIPANT_TYPE_V3));
  }

  /**
   * Checks if coding code matches the participant type.
   *
   * @param code the code to check
   * @param type the type to match
   * @return true if code matches type
   */
  private static boolean matchesParticipationType(String code, V3ParticipationType type) {
    return code != null && code.contentEquals(type.toString());
  }

  /**
   * Extracts practitioner ID from participant reference.
   *
   * @param part the participant component
   * @return the practitioner ID or null
   */
  private static String extractPractitionerId(EncounterParticipantComponent part) {
    if (part.getIndividual() == null || part.getIndividual().getReferenceElement() == null) {
      return null;
    }
    return part.getIndividual().getReferenceElement().getIdPart();
  }

  /**
   * Looks up and adds practitioner if found.
   *
   * @param data the FHIR data
   * @param practitionerId the practitioner ID
   * @param type the participation type
   * @param practs the list to add to
   */
  private static void addPractitionerIfFound(
      R4FhirData data, String practitionerId, V3ParticipationType type, List<Practitioner> practs) {
    if (practitionerId == null) {
      logger.debug("Individual Ref Id is null");
      return;
    }
    Practitioner pr = data.getPractitionerById(practitionerId);
    if (pr != null) {
      logger.info("Found Practitioner for Type {}", type);
      practs.add(pr);
    }
  }

  /**
   * Processes codings within a codeable concept for participant type.
   *
   * @param typeCodes the codings to process
   * @param part the participant
   * @param type the participation type
   * @param data the FHIR data
   * @param practs the list to add practitioners to
   */
  private static void processCodingsForParticipant(
      List<Coding> typeCodes,
      EncounterParticipantComponent part,
      V3ParticipationType type,
      R4FhirData data,
      List<Practitioner> practs) {
    for (Coding cd : typeCodes) {
      if (cd.getSystem() == null || !isValidParticipantCodeSystem(cd.getSystem())) {
        logger.debug("Did not find participants using standard code system ");
        continue;
      }

      logger.debug("Found Practitioner for Participation code system");

      if (!matchesParticipationType(cd.getCode(), type)) {
        logger.debug("Did not find the code for type {}", type);
        continue;
      }

      logger.debug("Found Practitioner for Code and CodeSystem");
      logger.debug("part.getIndividual = {}", part.getIndividual().getDisplay());
      if (part.getIndividual().getReferenceElement() != null) {
        logger.debug("part.getIndividual = {}", part.getIndividual().getReferenceElement());
      }

      String practitionerId = extractPractitionerId(part);
      addPractitionerIfFound(data, practitionerId, type, practs);
    }
  }

  /**
   * Processes all types for a participant.
   *
   * @param types the types to process
   * @param part the participant
   * @param type the participation type
   * @param data the FHIR data
   * @param practs the list to add practitioners to
   */
  private static void processParticipantTypes(
      List<CodeableConcept> types,
      EncounterParticipantComponent part,
      V3ParticipationType type,
      R4FhirData data,
      List<Practitioner> practs) {
    for (CodeableConcept conc : types) {
      logger.debug("Get Coding information for codeable concept");
      List<Coding> typeCodes = conc.getCoding();

      if (typeCodes != null && !typeCodes.isEmpty()) {
        processCodingsForParticipant(typeCodes, part, type, data, practs);
      }
    }
  }

  /**
   * Processes a single encounter participant.
   *
   * @param part the participant to process
   * @param type the participation type
   * @param data the FHIR data
   * @param practs the list to add practitioners to
   */
  private static void processEncounterParticipant(
      EncounterParticipantComponent part,
      V3ParticipationType type,
      R4FhirData data,
      List<Practitioner> practs) {
    if (part.getIndividual() == null || part.getIndividual().getReference() == null) {
      return;
    }

    logger.debug("Individual is present");
    List<CodeableConcept> types = part.getType();

    if (types != null && !types.isEmpty()) {
      logger.debug("Codeable Concepts present for individuals");
      processParticipantTypes(types, part, type, data, practs);
    }
  }

  public static List<Practitioner> getPractitionersForType(
      R4FhirData data, V3ParticipationType type) {
    List<Practitioner> practs = new ArrayList<>();

    if (data == null || data.getEncounter() == null) {
      return practs;
    }

    List<EncounterParticipantComponent> participants = data.getEncounter().getParticipant();

    if (participants == null || participants.isEmpty()) {
      return practs;
    }

    for (EncounterParticipantComponent part : participants) {
      processEncounterParticipant(part, type, data, practs);
    }

    return practs;
  }

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

  public static Boolean isCodeableConceptPresentForCodeSystem(
      CodeableConcept cc, String codeSystemUrl) {

    Boolean foundCodeSystem = false;

    if (cc != null && cc.hasCoding()) {

      for (Coding c : cc.getCoding()) {

        if (c.hasSystem() && c.getSystem().contentEquals(codeSystemUrl)) {

          foundCodeSystem = true;
          break;
        }
      }
    }
    return foundCodeSystem;
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

      if (c.hasSystem() && c.getSystem().contentEquals(codeSystemUrl) && c.hasDisplay()) {

        display = c.getDisplay();
        foundCodeSystem = true;
        break;
      } else if (c.hasSystem() && c.getSystem().contentEquals(codeSystemUrl)) {
        foundCodeSystem = true;
      }

      if (Boolean.TRUE.equals(csOptional) && c.hasDisplay()) {
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
      Boolean csOptional,
      String contentRef) {

    StringBuilder sb = new StringBuilder(500);
    List<Coding> codes = getCodingForValidCodeSystems(cds);

    if (Boolean.FALSE.equals(valueTrue))
      sb.append(getCodingXmlForCodeSystem(codes, cdName, codeSystemUrl, csOptional, contentRef));
    else
      sb.append(
          getCodingXmlForValueForCodeSystem(codes, cdName, codeSystemUrl, csOptional, contentRef));

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

    if (Boolean.TRUE.equals(foundCodeForCodeSystem) || Boolean.TRUE.equals(csOptional)) {
      return sb.toString();
    } else {
      return new StringBuilder("").toString();
    }
  }

  public static String getCodeableConceptXml(CodeableConcept cd, String cdName, String contentRef) {

    String sb = "";
    if (cd != null && cd.hasCoding()) {
      sb += getCodingXml(cd.getCoding(), cdName, contentRef);
    } else if (cd != null && cd.hasText()) {
      sb +=
          CdaGeneratorUtils.getXmlForNullCDWithText(
              cdName, CdaGeneratorConstants.NF_OTH, cd.getText());
    } else {
      sb += CdaGeneratorUtils.getXmlForNullCD(cdName, CdaGeneratorConstants.NF_NI);
    }

    return sb;
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
            && !csd.getValue1().isEmpty()
            && c.getSystem().contentEquals(codeSystemUrl)
            && Boolean.FALSE.equals(foundCodeForCodeSystem)) {

          logger.debug("Found the Coding for Codesystem {}", codeSystemUrl);
          sb.append(
              CdaGeneratorUtils.getXmlForValueCDWithoutEndTag(
                  c.getCode(), csd.getValue0(), csd.getValue1(), c.getDisplay()));

          if (!contentRef.isEmpty())
            sb.append(CdaGeneratorUtils.getXmlForOriginalTextWithReference(contentRef));

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

    if (Boolean.TRUE.equals(foundCodeForCodeSystem) || Boolean.TRUE.equals(csOptional)) {
      return sb.toString();
    } else {
      return new StringBuilder("").toString();
    }
  }

  public static String getCodeableConceptXmlForValue(
      CodeableConcept cd, String cdName, String contentRef) {
    String sb = "";
    if (cd != null && cd.hasCoding()) {
      sb += getCodingXmlForValue(cd.getCoding(), cdName, contentRef);
    } else if (cd != null && cd.hasText()) {
      sb += CdaGeneratorUtils.getXmlForValueString(cd.getText());
    } else {
      sb += CdaGeneratorUtils.getXmlForNullValueCD(cdName, CdaGeneratorConstants.NF_NI);
    }

    return sb;
  }

  public static String getCodeableConceptXmlForValueWithValueSetAndVersion(
      CodeableConcept cd,
      String cdName,
      String contentRef,
      String valueset,
      String valuesetversion) {
    String sb = "";
    if (cd != null && cd.hasCoding()) {
      sb += getCodingXmlForValue(cd.getCoding(), cdName, contentRef);
    } else if (cd != null && cd.hasText()) {
      sb += CdaGeneratorUtils.getXmlForValueString(cd.getText());
    } else {
      sb += CdaGeneratorUtils.getXmlForNullValueCD(cdName, CdaGeneratorConstants.NF_NI);
    }

    return sb;
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

  /**
   * Filters out expired names, returns active ones.
   *
   * @param allNames the names to filter
   * @return list of active names, or all names if all are expired
   */
  private static List<HumanName> filterActiveNames(List<HumanName> allNames) {
    List<HumanName> names = new ArrayList<>();
    if (allNames != null && !allNames.isEmpty()) {
      for (HumanName n : allNames) {
        // Add name which is not expired
        if (!n.hasPeriod() || (n.hasPeriod() && !n.getPeriod().hasEnd())) {
          names.add(n);
        }
      }
      // All are expired so use whatever names were passed in
      if (names.isEmpty()) {
        names = allNames;
      }
    }
    return names;
  }

  /**
   * Gets name qualifier from human name if applicable.
   *
   * @param name the human name
   * @param isQualifierReq whether qualifier is required
   * @return the qualifier or null
   */
  private static String getNameQualifier(HumanName name, boolean isQualifierReq) {
    if (name.getUse() != null && isQualifierReq) {
      return CdaGeneratorConstants.getCodeForNameQualifier(name.getUse().toCode());
    }
    return null;
  }

  /**
   * Builds given names XML elements.
   *
   * @param ns the given names
   * @param nameQualifier the name qualifier
   * @param nameString the string builder
   */
  private static void buildGivenNames(
      List<StringType> ns, String nameQualifier, StringBuilder nameString) {
    for (StringType n : ns) {
      if (!StringUtils.isEmpty(n.getValue())) {
        nameString.append(
            CdaGeneratorUtils.getXmlForTextWithAttribute(
                CdaGeneratorConstants.FIRST_NAME_EL_NAME,
                CdaGeneratorConstants.QUALIFIER_ATTR_NAME,
                nameQualifier,
                n.getValue()));
      }
    }
  }

  /**
   * Adds last name XML element to string builder.
   *
   * @param name the human name
   * @param nameString the string builder
   */
  private static void addLastNameXml(HumanName name, StringBuilder nameString) {
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

  /**
   * Handles empty names case.
   *
   * @param nameString the string builder
   */
  private static void handleEmptyNames(StringBuilder nameString) {
    logger.debug("Did not find the Name for the patient ");
    nameString.append(
        CdaGeneratorUtils.getXmlForNFText(
            CdaGeneratorConstants.FIRST_NAME_EL_NAME, CdaGeneratorConstants.NF_NI));
    nameString.append(
        CdaGeneratorUtils.getXmlForNFText(
            CdaGeneratorConstants.LAST_NAME_EL_NAME, CdaGeneratorConstants.NF_NI));
  }

  public static String getNameXml(List<HumanName> allNames, boolean isQualifierReq) {
    StringBuilder nameString = new StringBuilder(200);

    List<HumanName> names = filterActiveNames(allNames);

    if (names != null && !names.isEmpty()) {
      Optional<HumanName> hName = names.stream().findFirst();
      if (hName.isPresent()) {
        HumanName name = hName.get();
        String nameQualifier = getNameQualifier(name, isQualifierReq);
        List<StringType> ns = name.getGiven();

        buildGivenNames(ns, nameQualifier, nameString);

        // If Empty create NF
        if (StringUtils.isEmpty(nameString)) {
          nameString.append(
              CdaGeneratorUtils.getXmlForNFText(
                  CdaGeneratorConstants.FIRST_NAME_EL_NAME, CdaGeneratorConstants.NF_NI));
        }

        addLastNameXml(name, nameString);
      }
    } else {
      handleEmptyNames(nameString);
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

    val = (val.isEmpty()) ? CdaGeneratorConstants.UNKNOWN_VALUE : val;

    return StringEscapeUtils.escapeXml11(val);
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

  /**
   * Finds medication in list by ID.
   *
   * @param medList the medication list
   * @param id the medication ID to find
   * @param strictMatch whether to do strict null checks
   * @return the medication or null
   */
  private static Medication findMedicationById(
      List<Medication> medList, String id, boolean strictMatch) {
    if (medList == null || medList.isEmpty() || id == null) {
      return null;
    }

    for (Medication m : medList) {
      if (strictMatch) {
        if (m.hasIdElement()
            && m.getIdElement().hasIdPart()
            && m.getIdElement().getIdPart().contentEquals(id)) {
          logger.info(FOUND_NON_CONTAINED_MED_MSG, id);
          return m;
        }
      } else {
        if (m.getIdElement().getIdPart().contentEquals(id)) {
          logger.info(FOUND_NON_CONTAINED_MED_MSG, id);
          return m;
        }
      }
    }
    return null;
  }

  /**
   * Processes medication reference.
   *
   * @param med the medication reference
   * @param contained the contained resources
   * @param medList the medication list
   * @param strictMatch whether to do strict null checks
   * @return the medication type string
   */
  private static String processMedicationReference(
      Reference med, List<Resource> contained, List<Medication> medList, boolean strictMatch) {
    if (med == null || !med.hasReference()) {
      return CdaGeneratorConstants.UNKNOWN_VALUE;
    }

    if (med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {
      logger.debug("Found Med which is a contained reference");
      String refId = med.getReference().substring(1);
      logger.debug("Ref Id {} ", refId);

      if (contained != null) {
        String result = getStringForMedicationFromContainedResources(contained, refId);
        logger.debug("Return Val = {}", result);
        return result;
      }
    } else {
      // Handle actual reference
      logger.debug(CHECKING_MED_REFS_MSG);
      String id = med.getReferenceElement().getIdPart();
      Medication medRes = findMedicationById(medList, id, strictMatch);

      if (medRes != null && medRes.hasCode()) {
        return getStringForType(medRes.getCode());
      }
    }

    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  /**
   * Handles medication for MedicationRequest.
   *
   * @param mr the medication request
   * @param medList the medication list
   * @return the medication type string
   */
  private static String handleMedicationRequest(MedicationRequest mr, List<Medication> medList) {
    logger.debug("Found Med Request ");

    if (mr.getMedication() instanceof Reference) {
      logger.debug("Found Med Request.Medication Reference ");
      return processMedicationReference(
          (Reference) mr.getMedication(), mr.getContained(), medList, false);
    } else if (mr.getMedication() instanceof CodeableConcept) {
      return getStringForType((CodeableConcept) mr.getMedication());
    }

    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  /**
   * Handles medication for MedicationAdministration.
   *
   * @param medAdmin the medication administration
   * @param medList the medication list
   * @return the medication type string
   */
  private static String handleMedicationAdministration(
      MedicationAdministration medAdmin, List<Medication> medList) {
    if (medAdmin.getMedication() instanceof Reference) {
      return processMedicationReference(
          (Reference) medAdmin.getMedication(), medAdmin.getContained(), medList, false);
    } else if (medAdmin.getMedication() instanceof CodeableConcept) {
      return getStringForType((CodeableConcept) medAdmin.getMedication());
    }

    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  /**
   * Handles medication for MedicationStatement.
   *
   * @param medStmt the medication statement
   * @param medList the medication list
   * @return the medication type string
   */
  private static String handleMedicationStatement(
      MedicationStatement medStmt, List<Medication> medList) {
    if (medStmt.getMedication() instanceof Reference) {
      return processMedicationReference(
          (Reference) medStmt.getMedication(), medStmt.getContained(), medList, true);
    } else if (medStmt.getMedication() instanceof CodeableConcept) {
      return getStringForType((CodeableConcept) medStmt.getMedication());
    }

    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  public static String getStringForMedicationType(Resource r, List<Medication> medList) {
    if (r instanceof MedicationRequest) {
      return handleMedicationRequest((MedicationRequest) r, medList);
    } else if (r instanceof MedicationAdministration) {
      return handleMedicationAdministration((MedicationAdministration) r, medList);
    } else if (r instanceof MedicationStatement) {
      return handleMedicationStatement((MedicationStatement) r, medList);
    }

    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  /**
   * Handles Extension type.
   *
   * @param ext the extension
   * @return the extension as string or UNKNOWN_VALUE
   */
  private static String handleExtensionType(Extension ext) {
    if (ext.hasValue()) {
      return getStringForType(ext.getValue());
    } else if (ext.hasExtension()) {
      List<Extension> exts = ext.getExtension();
      StringBuilder retV = new StringBuilder();
      Boolean first = true;

      for (Extension ex : exts) {
        if (ex.hasValue()) {
          if (first) {
            retV.append(ex.getUrl()).append("-").append(getStringForType(ex.getValue()));
            first = false;
          } else {
            retV.append("|")
                .append(ex.getUrl())
                .append("-")
                .append(getStringForType(ex.getValue()));
          }
        }
      }
      return retV.toString();
    }
    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  /**
   * Handles CodeableConcept type.
   *
   * @param cd the codeable concept
   * @return the concept as string
   */
  private static String handleCodeableConceptType(CodeableConcept cd) {
    if (!StringUtils.isEmpty(cd.getText())) {
      return cd.getText();
    }

    List<Coding> cds = cd.getCoding();
    if (cds == null || cds.isEmpty()) {
      return "";
    }

    StringBuilder val = new StringBuilder();
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
    return val.toString();
  }

  /**
   * Handles Period type.
   *
   * @param pt the period
   * @return the period as string
   */
  private static String handlePeriodType(Period pt) {
    logger.debug("Found the Period element for creating string");

    if (pt.hasStart() && pt.hasEnd()) {
      return CdaGeneratorUtils.getStringForDateTime(
              pt.getStart(), pt.getStartElement().getTimeZone())
          + CdaGeneratorConstants.PIPE
          + CdaGeneratorUtils.getStringForDateTime(pt.getEnd(), pt.getEndElement().getTimeZone());
    } else if (pt.hasStart()) {
      return CdaGeneratorUtils.getStringForDateTime(
          pt.getStart(), pt.getStartElement().getTimeZone());
    } else if (pt.hasEnd()) {
      return CdaGeneratorUtils.getStringForDateTime(pt.getEnd(), pt.getEndElement().getTimeZone());
    }
    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  /**
   * Handles Timing type.
   *
   * @param t the timing
   * @return the timing as string
   */
  private static String handleTimingType(Timing t) {
    logger.debug("Found an instance of timing for creating string ");
    if (t.getRepeat() != null && t.getRepeat().getBounds() != null) {
      logger.debug("Found the bounds element for creating string ");
      return getStringForType(t.getRepeat().getBounds());
    }
    return "";
  }

  /**
   * Handles BooleanType.
   *
   * @param b the boolean type
   * @return "true" or "false"
   */
  private static String handleBooleanType(BooleanType b) {
    return b.getValueAsString().equalsIgnoreCase("true") ? "true" : FALSE_VALUE;
  }

  public static String getStringForType(Type dt) {
    if (dt == null) {
      return CdaGeneratorConstants.UNKNOWN_VALUE;
    }

    String val = "";

    if (dt instanceof Extension) {
      val = handleExtensionType((Extension) dt);
    } else if (dt instanceof Coding) {
      val = getStringForCoding((Coding) dt);
    } else if (dt instanceof CodeableConcept) {
      val = handleCodeableConceptType((CodeableConcept) dt);
    } else if (dt instanceof Quantity) {
      val = getStringForQuantity((Quantity) dt);
    } else if (dt instanceof DateTimeType) {
      DateTimeType d = (DateTimeType) dt;
      val = CdaGeneratorUtils.getStringForDateTime(d.getValue(), d.getTimeZone());
    } else if (dt instanceof Timing) {
      val = handleTimingType((Timing) dt);
    } else if (dt instanceof Period) {
      val = handlePeriodType((Period) dt);
    } else if (dt instanceof CodeType) {
      val = ((CodeType) dt).getValue();
    } else if (dt instanceof StringType) {
      val = ((StringType) dt).getValue();
    } else if (dt instanceof BooleanType) {
      val = handleBooleanType((BooleanType) dt);
    }

    logger.debug("Printing the class name {} and value {}", dt.getClass(), val);
    return StringEscapeUtils.escapeXml11(val);
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

  /**
   * Handles Coding for XML.
   *
   * @param cd the coding
   * @param elName the element name
   * @param valFlag the flag
   * @return the XML
   */
  private static String handleCodingForXml(Coding cd, String elName, Boolean valFlag) {
    List<Coding> cds = new ArrayList<>();
    cds.add(cd);
    return Boolean.FALSE.equals(valFlag)
        ? getCodingXml(cds, elName, "")
        : getCodingXmlForValue(cds, elName, null);
  }

  /**
   * Handles CodeableConcept for XML.
   *
   * @param cd the codeable concept
   * @param elName the element name
   * @param valFlag the flag
   * @return the XML
   */
  private static String handleCodeableConceptForXml(
      CodeableConcept cd, String elName, Boolean valFlag) {
    String val = "";
    if (cd.hasCoding()) {
      List<Coding> cds = cd.getCoding();
      val =
          Boolean.FALSE.equals(valFlag)
              ? getCodingXml(cds, elName, "")
              : getCodingXmlForValue(cds, elName, null);
    } else if (cd.hasText() && valFlag) {
      val = CdaGeneratorUtils.getXmlForValueString(cd.getText());
    }
    return val;
  }

  /**
   * Handles DateTimeType for XML.
   *
   * @param d the date time
   * @param elName the element name
   * @param valFlag the flag
   * @return the XML
   */
  private static String handleDateTimeForXml(DateTimeType d, String elName, Boolean valFlag) {
    return Boolean.FALSE.equals(valFlag)
        ? CdaGeneratorUtils.getXmlForEffectiveTime(elName, d.getValue(), d.getTimeZone())
        : CdaGeneratorUtils.getXmlForValueEffectiveTime(elName, d.getValue(), d.getTimeZone());
  }

  /**
   * Handles CodeType for XML.
   *
   * @param cd the code type
   * @param elName the element name
   * @param valFlag the flag
   * @return the XML
   */
  private static String handleCodeTypeForXml(CodeType cd, String elName, Boolean valFlag) {
    return Boolean.FALSE.equals(valFlag)
        ? CdaGeneratorUtils.getXmlForCD(elName, cd.getCode())
        : CdaGeneratorUtils.getXmlForValueString(cd.getCode());
  }

  /**
   * Handles StringType for XML.
   *
   * @param st the string type
   * @param elName the element name
   * @param valFlag the flag
   * @return the XML
   */
  private static String handleStringTypeForXml(StringType st, String elName, Boolean valFlag) {
    return Boolean.FALSE.equals(valFlag)
        ? CdaGeneratorUtils.getXmlForText(elName, st.getValue())
        : CdaGeneratorUtils.getXmlForValueString(st.getValue());
  }

  /**
   * Handles BooleanType for XML.
   *
   * @param b the boolean type
   * @return the XML
   */
  private static String handleBooleanTypeForXml(BooleanType b) {
    String boolVal = b.getValueAsString().equalsIgnoreCase(TRUE_VALUE) ? TRUE_VALUE : FALSE_VALUE;
    return CdaGeneratorUtils.getXmlForValueString(boolVal);
  }

  /**
   * Handles Timing for XML.
   *
   * @param t the timing
   * @param elName the element name
   * @param valFlag the flag
   * @return the XML
   */
  private static String handleTimingForXml(Timing t, String elName, Boolean valFlag) {
    if (t.getRepeat() != null && t.getRepeat().getBounds() != null) {
      logger.debug("Found the bounds element for creating xml");
      return getXmlForType(t.getRepeat().getBounds(), elName, valFlag);
    }
    return "";
  }

  /**
   * Handles null or absent reason extension case.
   *
   * @param elName the element name
   * @param valFlag the flag
   * @return the XML
   */
  private static String handleNullOrAbsentType(String elName, Boolean valFlag) {
    return Boolean.FALSE.equals(valFlag)
        ? CdaGeneratorUtils.getNFXMLForElement(elName, CdaGeneratorConstants.NF_NI)
        : CdaGeneratorUtils.getXmlForValueString(CdaGeneratorConstants.NO_VALUE);
  }

  public static String getXmlForType(Type dt, String elName, Boolean valFlag) {
    if (dt == null || dt.hasExtension(CdaGeneratorConstants.FHIR_DATA_ABSENT_REASON_EXT_URL)) {
      return handleNullOrAbsentType(elName, valFlag);
    }

    String val = "";

    if (dt instanceof Coding) {
      val = handleCodingForXml((Coding) dt, elName, valFlag);
    } else if (dt instanceof CodeableConcept) {
      val = handleCodeableConceptForXml((CodeableConcept) dt, elName, valFlag);
    } else if (dt instanceof Quantity) {
      val = getQuantityXml((Quantity) dt, elName, valFlag);
    } else if (dt instanceof DateTimeType) {
      val = handleDateTimeForXml((DateTimeType) dt, elName, valFlag);
    } else if (dt instanceof Period) {
      val = getPeriodXml((Period) dt, elName, valFlag);
    } else if (dt instanceof Timing) {
      val = handleTimingForXml((Timing) dt, elName, valFlag);
    } else if (dt instanceof CodeType) {
      val = handleCodeTypeForXml((CodeType) dt, elName, valFlag);
    } else if (dt instanceof StringType) {
      val = handleStringTypeForXml((StringType) dt, elName, valFlag);
    } else if (dt instanceof BooleanType) {
      val = handleBooleanTypeForXml((BooleanType) dt);
    }

    logger.debug(PRINTING_THE_CLASS_NAME, dt.getClass());
    return val;
  }

  public static String getXmlForTypeForValueIvlTsEffectiveTime(String elName, Type dt) {

    String val = "";
    if (dt != null) {

      if (dt instanceof DateTimeType) {

        DateTimeType d = (DateTimeType) dt;

        Pair<Date, TimeZone> low = new Pair<>(d.getValue(), d.getTimeZone());
        val += CdaGeneratorUtils.getXmlForValueIVLWithTS(elName, low, null);

      } else if (dt instanceof Period) {
        Period pt = (Period) dt;

        Pair<Date, TimeZone> low = null;
        Pair<Date, TimeZone> high = null;
        if (pt.hasStart()) low = new Pair<>(pt.getStart(), pt.getStartElement().getTimeZone());
        if (pt.hasEnd()) high = new Pair<>(pt.getEnd(), pt.getEndElement().getTimeZone());
        val += CdaGeneratorUtils.getXmlForValueIVLWithTS(elName, low, high);
      } else if (dt instanceof Timing) {

        Timing t = (Timing) (dt);
        if (t.getRepeat() != null && t.getRepeat().getBounds() != null) {

          logger.debug("Found the bounds element for creating xml");
        }
      }

      return val;
    } else {
      Pair<Date, TimeZone> low = null;
      Pair<Date, TimeZone> high = null;
      val +=
          CdaGeneratorUtils.getXmlForValueIVLWithTS(
              CdaGeneratorConstants.EFF_TIME_EL_NAME, low, high);
    }

    return val;
  }

  /**
   * Handles contained medication reference.
   *
   * @param med the medication reference
   * @param res the domain resource
   * @param elName the element name
   * @param valFlag the flag
   * @param codeSystemUrl the code system URL
   * @param csOptional whether code system is optional
   * @return the XML for medication
   */
  private static String handleContainedMedicationReference(
      Reference med,
      DomainResource res,
      String elName,
      Boolean valFlag,
      String codeSystemUrl,
      Boolean csOptional) {
    String refId = med.getReference().substring(1);
    logger.info("Found Medication of Type Reference with Id {}", refId);

    if (res.getContained() == null) {
      return "";
    }

    logger.info("Contained Elements Not null");
    List<Resource> meds = res.getContained();

    for (Resource r : meds) {
      if (r.hasId() && r.getId().contains(refId) && r instanceof Medication) {
        logger.info("Found Medication in contained resource");
        Medication cmed = (Medication) r;
        return getXmlForMedication(cmed, elName, valFlag, codeSystemUrl, csOptional);
      }
    }
    return "";
  }

  /**
   * Handles non-contained medication reference.
   *
   * @param med the medication reference
   * @param medList the medication list
   * @param elName the element name
   * @param valFlag the flag
   * @param codeSystemUrl the code system URL
   * @param csOptional whether code system is optional
   * @return the XML for medication
   */
  private static String handleNonContainedMedicationReference(
      Reference med,
      List<Medication> medList,
      String elName,
      Boolean valFlag,
      String codeSystemUrl,
      Boolean csOptional) {
    logger.info(CHECKING_MED_REFS_MSG);

    if (medList == null || medList.isEmpty()) {
      return "";
    }

    String id = med.getReferenceElement().getIdPart();
    Medication medRes = findMedicationById(medList, id, true);

    if (medRes != null) {
      return getXmlForMedication(medRes, elName, valFlag, codeSystemUrl, csOptional);
    }
    return "";
  }

  public static String getXmlForMedicationTypeForCodeSystem(
      Type dt,
      String elName,
      Boolean valFlag,
      String codeSystemUrl,
      Boolean csOptional,
      DomainResource res,
      List<Medication> medList) {

    if (!(dt instanceof Reference)) {
      return getXmlForTypeForCodeSystem(dt, elName, valFlag, codeSystemUrl, csOptional);
    }

    logger.info("Found Medication of Type Reference within Domain Resource");
    Reference med = (Reference) dt;

    if (med.hasReference()
        && med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {
      return handleContainedMedicationReference(
          med, res, elName, valFlag, codeSystemUrl, csOptional);
    } else {
      return handleNonContainedMedicationReference(
          med, medList, elName, valFlag, codeSystemUrl, csOptional);
    }
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
        else val += getCodingXmlForValueForCodeSystem(cds, elName, codeSystemUrl, csOptional, "");

      } else if (dt instanceof CodeableConcept) {

        CodeableConcept cd = (CodeableConcept) dt;

        List<Coding> cds = cd.getCoding();

        if (Boolean.FALSE.equals(valFlag))
          val += getCodingXmlForCodeSystem(cds, elName, codeSystemUrl, csOptional, "");
        else val += getCodingXmlForValueForCodeSystem(cds, elName, codeSystemUrl, csOptional, "");

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

  /**
   * Processes matching coding and builds XML.
   *
   * @param code the code to match
   * @param csUrl the code system URL
   * @param elementName the element name
   * @param codeSystem the code system
   * @param codeSystemName the code system name
   * @param valueSet the value set
   * @param valuesetVersion the value set version
   * @param dispName the display name
   * @param contentRef the content reference
   * @param valueElem whether it's a value element
   * @param retval the result builder
   * @return the display name
   */
  private static String processMatchingCoding(
      String code,
      String csUrl,
      String elementName,
      String codeSystem,
      String codeSystemName,
      String valueSet,
      String valuesetVersion,
      String dispName,
      String contentRef,
      Boolean valueElem,
      StringBuilder retval) {
    logger.debug(" Found a Coding that matches the CodeSystem and Code {} : {} ", codeSystem, code);

    if (Boolean.FALSE.equals(valueElem)) {
      retval.append(
          CdaGeneratorUtils.getXmlForCDWithValueSetAndVersionWihoutEndTag(
              elementName, code, codeSystem, codeSystemName, valueSet, valuesetVersion, dispName));
      if (!contentRef.isEmpty())
        retval.append(CdaGeneratorUtils.getXmlForOriginalTextWithReference(contentRef));
    } else if (Boolean.TRUE.equals(valueElem)) {
      retval.append(
          CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersionWihoutEndTag(
              elementName, code, codeSystem, codeSystemName, valueSet, valuesetVersion, dispName));
    }
    return dispName;
  }

  /**
   * Processes translation coding.
   *
   * @param cd the coding
   * @param translations the translations builder
   */
  private static void processTranslationCoding(Coding cd, StringBuilder translations) {
    if (cd.getSystem() == null) {
      return;
    }
    Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(cd.getSystem());
    if (csd != null && !csd.getValue0().isEmpty() && !csd.getValue1().isEmpty()) {
      String dispName = (cd.hasDisplay()) ? cd.getDisplay() : "";
      translations.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.TRANSLATION_EL_NAME,
              cd.getCode(),
              csd.getValue0(),
              csd.getValue1(),
              dispName));
    }
  }

  /**
   * Processes codings when none match.
   *
   * @param cc the codeable concept
   * @param code the code
   * @param codeSystem the code system
   * @param codeSystemName the code system name
   * @param valueSet the value set
   * @param valuesetVersion the value set version
   * @param contentRef the content reference
   * @param valueElem whether it's a value element
   * @param retval the result builder
   */
  private static void processNotFoundCodings(
      CodeableConcept cc,
      String code,
      String codeSystem,
      String codeSystemName,
      String valueSet,
      String valuesetVersion,
      String contentRef,
      Boolean valueElem,
      StringBuilder retval) {
    String dispName = "";
    if (cc != null && cc.getText() != null && !cc.getText().isEmpty()) {
      dispName = cc.getText();
    }

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
    String dispName = (cc != null) ? cc.getText() : "";

    if (cc != null && cc.hasCoding()) {
      List<Coding> cds = cc.getCoding();

      for (Coding cd : cds) {
        if (cd.getCode() != null
            && !cd.getCode().isEmpty()
            && code.contentEquals(cd.getCode())
            && csUrl.contentEquals(cd.getSystem())
            && Boolean.FALSE.equals(foundCodings)) {
          if (cd.getDisplay() != null && !cd.getDisplay().isEmpty()) {
            dispName = cd.getDisplay();
          }
          dispName =
              processMatchingCoding(
                  code,
                  csUrl,
                  elementName,
                  codeSystem,
                  codeSystemName,
                  valueSet,
                  valuesetVersion,
                  dispName,
                  contentRef,
                  valueElem,
                  retval);
          foundCodings = true;
        } else {
          processTranslationCoding(cd, translations);
        }
      }
    }

    if (Boolean.TRUE.equals(foundCodings)) {
      retval.append(translations.toString());
      retval.append(CdaGeneratorUtils.getXmlForEndElement(elementName));
    } else {
      processNotFoundCodings(
          cc,
          code,
          codeSystem,
          codeSystemName,
          valueSet,
          valuesetVersion,
          contentRef,
          valueElem,
          retval);
    }

    return retval.toString();
  }

  /**
   * Checks if code system is valid for matched code.
   *
   * @param csd the code system pair
   * @return true if valid
   */
  private static boolean isValidCodeSystem(Pair<String, String> csd) {
    return csd != null && !csd.getValue0().isEmpty() && !csd.getValue1().isEmpty();
  }

  /**
   * Processes matched coding for value set.
   *
   * @param cd the coding
   * @param elementName the element name
   * @param valueSet the value set
   * @param valuesetVersion the value set version
   * @param contentRef the content reference
   * @param valueElem the value element flag
   * @param retval the result builder
   * @return true if matched and processed
   */
  private static boolean processMatchedCodingForValueSet(
      Coding cd,
      String elementName,
      String valueSet,
      String valuesetVersion,
      String contentRef,
      Boolean valueElem,
      StringBuilder retval) {
    logger.debug(" Found a Coding that is in the trigger code matches. ");
    Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(cd.getSystem());

    if (!isValidCodeSystem(csd)) {
      return false;
    }

    String dispName = cd.hasDisplay() ? cd.getDisplay() : "";

    if (Boolean.FALSE.equals(valueElem)) {
      retval.append(
          CdaGeneratorUtils.getXmlForCDWithValueSetAndVersionWihoutEndTag(
              elementName,
              cd.getCode(),
              csd.getValue0(),
              csd.getValue1(),
              valueSet,
              valuesetVersion,
              dispName));
      if (!contentRef.isEmpty())
        retval.append(CdaGeneratorUtils.getXmlForOriginalTextWithReference(contentRef));
    } else if (Boolean.TRUE.equals(valueElem)) {
      retval.append(
          CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersionWihoutEndTag(
              elementName,
              cd.getCode(),
              csd.getValue0(),
              csd.getValue1(),
              valueSet,
              valuesetVersion,
              dispName));
    }
    return true;
  }

  public static String getXmlForMatchedCodesWithValueSetAndVersion(
      String elementName,
      Set<String> matchedCodes,
      String valueSet,
      String valuesetVersion,
      CodeableConcept cc,
      String contentRef,
      Boolean valueElem) {

    StringBuilder retval = new StringBuilder();
    StringBuilder translations = new StringBuilder();
    Boolean foundCodings = false;

    if (cc != null && cc.hasCoding()) {
      String dispName = cc.getText();
      List<Coding> cds = cc.getCoding();

      for (Coding cd : cds) {
        if (cd.hasCode() && isCodeContained(matchedCodes, cd.getCode()) && !foundCodings) {
          if (cd.hasDisplay()) {
            dispName = cd.getDisplay();
          }
          if (processMatchedCodingForValueSet(
              cd, elementName, valueSet, valuesetVersion, contentRef, valueElem, retval)) {
            foundCodings = true;
          }
        } else {
          processTranslationCoding(cd, translations);
        }
      }
    }

    if (Boolean.TRUE.equals(foundCodings)) {
      retval.append(translations.toString());
      retval.append(CdaGeneratorUtils.getXmlForEndElement(elementName));
    } else {
      String dispName = "";
      if (cc != null && cc.getText() != null && !cc.getText().isEmpty()) {
        dispName = cc.getText();
      }

      if (Boolean.FALSE.equals(valueElem)) {
        retval.append(
            CdaFhirUtilities.getCodeableConceptXml(
                cc, CdaGeneratorConstants.CODE_EL_NAME, contentRef));
      } else if (Boolean.TRUE.equals(valueElem)) {
        retval.append(
            CdaFhirUtilities.getCodeableConceptXmlForValue(
                cc, CdaGeneratorConstants.CODE_EL_NAME, contentRef));
      }
    }

    return retval.toString();
  }

  public static boolean isCodeContained(Set<String> codes, String code) {

    if (codes != null && code != null) {

      for (String s : codes) {
        if (s.contains(code)) return true;
      }
    }

    return false;
  }

  public static String getStatusCodeForFhirMedStatusCodes(String val) {

    if (val.equalsIgnoreCase("active")
        || val.equalsIgnoreCase("in-progress")
        || val.equalsIgnoreCase("intended")
        || val.equalsIgnoreCase("not-taken")) {
      return "active";
    } else if (val.equalsIgnoreCase(COMPLETED)) {
      return COMPLETED;
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
    } else return COMPLETED;
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
        logger.debug(FOUND_ADDRESS_EXTENSION_MSG);
        return (Address) extension.getValue();
      }
    }
    logger.debug(EXT_NOT_FOUND_MSG, extensionUrl);
    return null;
  }

  public static Boolean getBooleanExtensionValue(List<Extension> extensions, String extensionUrl) {
    if (extensions == null || extensions.isEmpty()) {
      return false;
    }

    for (Extension extension : extensions) {
      if (extension.hasUrl()
          && extension.getUrl().equals(extensionUrl)
          && extension.hasValue()
          && extension.getValue() instanceof BooleanType) {
        logger.debug(FOUND_ADDRESS_EXTENSION_MSG);
        BooleanType retVal = (BooleanType) extension.getValue();
        return retVal.getValue();
      }
    }
    logger.debug(EXT_NOT_FOUND_MSG, extensionUrl);
    return false;
  }

  public static DateTimeType getDateTimeExtensionValue(
      List<Extension> extensions, String extensionUrl) {

    if (extensions == null || extensions.isEmpty()) {
      return null;
    }

    for (Extension extension : extensions) {
      if (extension.hasUrl()
          && extension.getUrl().equals(extensionUrl)
          && extension.hasValue()
          && extension.getValue() instanceof DateTimeType) {
        logger.debug(FOUND_ADDRESS_EXTENSION_MSG);
        return (DateTimeType) extension.getValue();
      }
    }
    logger.debug(EXT_NOT_FOUND_MSG, extensionUrl);
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

  public static Object getPerformerXml(Practitioner pract, String functionCode, Organization org) {

    StringBuilder s = new StringBuilder(200);

    if (pract != null || org != null) {

      s.append(
          CdaGeneratorUtils.getXmlForStartElementWithTypeCode(
              CdaGeneratorConstants.PERF_EL_NAME, CdaGeneratorConstants.DEFAULT_PERF_EL_TYPE_CODE));

      s.append(
          CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSIGNED_ENTITY_EL_NAME));

      s.append(getPractitionerXml(pract, org));

      if (org != null && org.hasName()) {

        s.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.REP_ORG_EL_NAME));

        Identifier id =
            org.hasIdentifier()
                ? CdaFhirUtilities.getIdentifierForSystem(org.getIdentifier(), FHIR_NPI_URL)
                : null;
        if (id != null && id.hasSystem() && id.hasValue()) {
          s.append(
              CdaGeneratorUtils.getXmlForII(
                  CdaGeneratorUtils.getRootOid(id.getSystem(), id.getValue()), id.getValue()));
        } else {
          s.append(CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA));
        }
        s.append(
            CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.NAME_EL_NAME, org.getName()));
        s.append(CdaFhirUtilities.getTelecomXml(org.getTelecom(), false, false));
        s.append(CdaFhirUtilities.getAddressXml(org.getAddress(), false));

        s.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.REP_ORG_EL_NAME));
      }
      s.append(
          CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_ENTITY_EL_NAME));
      s.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PERF_EL_NAME));
    }

    return s;
  }

  public static String getPractitionerXml(Practitioner pr, Organization org) {

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
      List<ContactPoint> telecom;

      List<ContactPoint> practitionerTelecom = pr.getTelecom();
      List<ContactPoint> orgTelecom = (org != null) ? org.getTelecom() : null;

      if (practitionerTelecom != null && !practitionerTelecom.isEmpty()) {
        telecom = practitionerTelecom;
      } else if (orgTelecom != null && !orgTelecom.isEmpty()) {
        telecom = orgTelecom;
      } else {
        telecom = Collections.emptyList();
      }

      sb.append(CdaFhirUtilities.getTelecomXml(telecom, false, false));
      sb.append(
          CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSIGNED_PERSON_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.NAME_EL_NAME));

      List<HumanName> hns = pr.getName();
      sb.append(CdaFhirUtilities.getNameXml(hns, true));

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));
      sb.append(
          CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_PERSON_EL_NAME));

    } else {

      sb.append(CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA));

      List<Address> addrs = null;
      sb.append(CdaFhirUtilities.getAddressXml(addrs, false));

      List<ContactPoint> cps = null;
      sb.append(CdaFhirUtilities.getTelecomXml(cps, false, false));

      sb.append(
          CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSIGNED_PERSON_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.NAME_EL_NAME));

      List<HumanName> hns = null;
      sb.append(CdaFhirUtilities.getNameXml(hns, true));

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

    return StringEscapeUtils.escapeXml11(result);
  }

  public static String getRaceOrEthnicityXml(List<Extension> exts, String elName, String extUrl) {

    StringBuffer str = new StringBuffer(200);
    Coding re =
        CdaFhirUtilities.getCodingExtension(
            exts, extUrl, CdaGeneratorConstants.OMB_RACE_CATEGORY_URL);

    if (re != null && re.hasCode()) {
      if (!isCodingNullFlavor(re)) {
        str.append(
            CdaGeneratorUtils.getXmlForCD(
                elName,
                re.getCode(),
                CdaGeneratorConstants.RACE_CODE_SYSTEM,
                CdaGeneratorConstants.RACE_CODE_SYSTEM_NAME,
                re.getDisplay()));
      } else if (doesCodingHaveOtherRace(re)) {

        str.append(
            CdaGeneratorUtils.getXmlForNFCDWithTranslation(
                elName,
                re.getCode(),
                CdaGeneratorConstants.RACE_CODE_SYSTEM,
                CdaGeneratorConstants.RACE_CODE_SYSTEM,
                re.getDisplay()));

      } else {
        str.append(CdaGeneratorUtils.getXmlForNullCD(elName, re.getCode()));
      }
    } else if (re != null && re.hasDisplay()) {

      // Add display if it is present.
      str.append(
          CdaGeneratorUtils.getXmlForNFCDWithText(
              elName, CdaGeneratorConstants.NF_NI, re.getDisplay()));
    } else {

      // Check for Text and use it if present.
      String val =
          CdaFhirUtilities.getStringExtension(exts, extUrl, CdaGeneratorConstants.OMB_TEXT_URL);

      str.append(CdaGeneratorUtils.getXmlForNFCDWithText(elName, CdaGeneratorConstants.NF_NI, val));
    }

    return str.toString();
  }

  public static boolean isCodingNullFlavor(Coding coding) {

    if (coding != null
        && coding.hasCode()
        && (coding.getCode().contentEquals("ASKU") || coding.getCode().contentEquals("UNK"))) {
      return true;
    } else return false;
  }

  public static boolean doesCodingHaveOtherRace(Coding coding) {

    if (coding != null && coding.hasCode() && (coding.getCode().contentEquals("2131-1"))) {
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

  public static String getXmlForAuthor(List<Reference> performerRefs, R4FhirData data) {
    StringBuilder sb = new StringBuilder();
    if (data == null || performerRefs == null || performerRefs.isEmpty()) {
      return sb.toString();
    }

    for (Reference reference : performerRefs) {
      if (reference.hasReferenceElement()
          && reference.getReferenceElement().hasResourceType()
          && ResourceType.fromCode(reference.getReferenceElement().getResourceType())
              == ResourceType.Practitioner) {

        Practitioner pract = data.getPractitionerById(reference.getReferenceElement().getIdPart());
        if (pract != null) {
          HashMap<V3ParticipationType, List<Practitioner>> practMap = new HashMap<>();
          practMap.put(V3ParticipationType.AUT, Collections.singletonList(pract));
          sb.append(CdaHeaderGenerator.getAuthorXml(data, data.getEncounter(), practMap));
        }
      }
    }
    return sb.toString();
  }

  public static String getXmlForAuthorTime(DateTimeType dt) {

    if (dt == null) {
      return "";
    }

    return getXmlForAuthorTimeValues(dt.getValue(), dt.getTimeZone());
  }

  public static String getXmlForAuthorTimeValues(Date d, TimeZone t) {

    StringBuilder sb = new StringBuilder();
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.AUTHOR_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.TIME_EL_NAME, d, t));
    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSIGNED_AUTHOR_EL_NAME));
    sb.append(CdaGeneratorUtils.getNFXMLForII(CdaGeneratorConstants.NF_NA));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_AUTHOR_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.AUTHOR_EL_NAME));

    return sb.toString();
  }

  public static String getXmlForAuthorTime(InstantType dt) {
    if (dt == null) {
      return "";
    }

    return getXmlForAuthorTimeValues(dt.getValue(), dt.getTimeZone());
  }

  public static String getDisplayStringForPeriod(Period pd) {
    if (pd != null) {
      if (pd.hasStart())
        return CdaGeneratorUtils.getStringForDateTime(
            pd.getStart(), pd.getStartElement().getTimeZone());
      else if (pd.hasEnd())
        return CdaGeneratorUtils.getStringForDateTime(
            pd.getEnd(), pd.getEndElement().getTimeZone());
      else return CdaGeneratorConstants.UNKNOWN_VALUE;
    } else {
      return CdaGeneratorConstants.UNKNOWN_VALUE;
    }
  }

  public static String getDisplayStringForCodeableConcept(List<CodeableConcept> codes) {

    if (codes != null) {

      for (CodeableConcept cc : codes) {

        String s = getDisplayStringForCodeableConcept(cc);

        if (!s.contentEquals(CdaGeneratorConstants.UNKNOWN_VALUE)) return s;
      }
    }

    // Nothing worked, so use unknown value.
    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  public static String getDisplayStringForCodeableConcept(CodeableConcept code) {

    if (code != null) {
      if (code.hasText() && !code.getText().isEmpty()) return code.getText();

      if (code.hasCoding() && !code.getCoding().isEmpty()) {

        for (Coding c : code.getCoding()) {

          if (c.hasDisplay() && !c.getDisplay().isEmpty()) {
            return c.getDisplay();
          }
        }

        // No display names, so use the system + code or just code
        if (code.getCodingFirstRep().hasSystem() && code.getCodingFirstRep().hasCode()) {
          return code.getCodingFirstRep().getSystem() + "|" + code.getCodingFirstRep().getCode();
        }
      }
    }

    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  public static String getDisplayStringForCoding(Coding code) {

    if (code != null && code.hasDisplay() && !code.getDisplay().isEmpty()) {
      return code.getDisplay();
    } else if (code != null && code.hasSystem() && code.hasCode()) {
      return code.getSystem() + "|" + code.getCode();
    } else if (code != null && code.hasCode()) {
      return code.getCode();
    }
    // Nothing worked, so use unknown value.
    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  public static Boolean isCodePresent(List<CodeableConcept> cds, String code, String codeSystem) {

    Boolean retVal = false;
    if (cds != null && !cds.isEmpty()) {

      for (CodeableConcept cc : cds) {
        if (cc.hasCoding() && !cc.getCoding().isEmpty()) {

          List<Coding> codings = cc.getCoding();
          for (Coding c : codings) {
            if (c.getCode().contentEquals(code) && c.getSystem().contentEquals(codeSystem)) {
              logger.info(" Found code {} and codesystem {}", code, codeSystem);
              return true;
            }
          }
        }
      }
    }

    return retVal;
  }

  public static Boolean isCodeableConceptPresentInValueSet(String valueset, CodeableConcept code) {

    if (valueset == null || code == null) return false;

    if (code.hasCoding()) {

      List<Coding> cds = code.getCoding();

      for (Coding cd : cds) {

        if (cd.hasCode() && CdaGeneratorConstants.isCodePresentInValueSet(valueset, cd.getCode())) {
          return true;
        }
      }
    }

    return false;
  }

  public static String getXmlForSpecimen(Specimen spec) {

    StringBuilder sb = new StringBuilder();

    if (spec != null && spec.hasType()) {
      sb.append(
          CdaGeneratorUtils.getXmlForStartElementWithTypeCode(
              CdaGeneratorConstants.SPECIMEN_EL_NAME, CdaGeneratorConstants.TYPE_CODE_SPECIMEN));

      sb.append(
          CdaGeneratorUtils.getXmlForStartElementWithClassCode(
              CdaGeneratorConstants.SPECIMEN_ROLE_EL_NAME,
              CdaGeneratorConstants.SPECIMEN_ROLE_CLASS_CODE));
      sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());
      sb.append(
          CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SPECIMEN_PLAYING_ENTITY));

      List<CodeableConcept> cds = new ArrayList<>();
      cds.add(spec.getType());
      sb.append(
          CdaFhirUtilities.getCodeableConceptXml(cds, CdaGeneratorConstants.CODE_EL_NAME, false));

      sb.append(
          CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SPECIMEN_PLAYING_ENTITY));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SPECIMEN_ROLE_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SPECIMEN_EL_NAME));
    }

    return sb.toString();
  }

  public static String getStringForSpecimenCollectionDate(
      List<Reference> specimenRefs, R4FhirData data) {
    if (data == null || specimenRefs == null || specimenRefs.isEmpty()) {
      return CdaGeneratorConstants.UNKNOWN_VALUE;
    }

    for (Reference reference : specimenRefs) {
      if (reference.hasReferenceElement()
          && reference.getReferenceElement().hasResourceType()
          && ResourceType.fromCode(reference.getReferenceElement().getResourceType())
              == ResourceType.Specimen) {

        Specimen specimen = data.getSpecimenById(reference.getReferenceElement().getIdPart());
        if (specimen != null && specimen.hasCollection()) {

          if (specimen.getCollection().hasCollectedDateTimeType()) {
            return CdaFhirUtilities.getStringForType(
                specimen.getCollection().getCollectedDateTimeType());

          } else if (specimen.getCollection().hasCollectedPeriod()) {
            return CdaFhirUtilities.getStringForType(specimen.getCollection().getCollectedPeriod());
          }
        }
      }
    }
    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  public static String getNarrative(String frequency, String period, String periodUnit) {
    StringBuilder narrative = new StringBuilder();
    appendValue(narrative, "frequency", frequency);
    appendValue(narrative, "period", period);
    appendValue(narrative, "periodUnit", periodUnit);

    return narrative.toString();
  }

  private static void appendValue(StringBuilder narrative, String label, String value) {
    if (StringUtils.isNotBlank(value)) {
      if (narrative.length() > 0) {
        narrative.append("|"); // Add separator if narrative already has content
      }
      narrative.append(label).append(": ").append(value);
    }
  }

  public static boolean isResourceOfType(Reference actor, ResourceType type) {
    if (actor.hasReferenceElement()
        && actor.getReferenceElement().hasResourceType()
        && ResourceType.fromCode(actor.getReferenceElement().getResourceType()) == type) {
      return true;
    }

    return false;
  }

  public static ResourceType getResourceType(Reference reference) {
    try {
      return reference != null && reference.hasReferenceElement()
          ? ResourceType.valueOf(reference.getReferenceElement().getResourceType())
          : null;
    } catch (Exception e) {
      return null;
    }
  }

  public static Pair<Boolean, String> getMedicationCodeXml(
      LaunchDetails details,
      CodeableConcept code,
      Boolean valElement,
      String contentRef,
      List<String> paths,
      String version) {

    String elementType =
        valElement ? CdaGeneratorConstants.VAL_EL_NAME : CdaGeneratorConstants.CODE_EL_NAME;
    PatientExecutionState state = ApplicationUtils.getDetailStatus(details);
    List<MatchedTriggerCodes> mtcs = state.getMatchTriggerStatus().getMatchedCodes();

    for (MatchedTriggerCodes mtc : mtcs) {
      Pair<String, String> matchedCode = findMatchingCode(mtc, code, paths);
      if (matchedCode != null) {
        String systemUrl =
            valElement
                ? CdaGeneratorConstants.FHIR_SNOMED_URL
                : CdaGeneratorConstants.FHIR_RXNORM_URL;

        logger.info("Found a matched {} for the Medication ", elementType);

        Pair<String, String> systemName =
            CdaGeneratorConstants.getCodeSystemFromUrl(matchedCode.getValue1());
        String xml =
            CdaFhirUtilities.getXmlForCodeableConceptWithCDAndValueSetAndVersion(
                elementType,
                matchedCode.getValue0(),
                systemName.getValue0(),
                systemName.getValue1(),
                details.getRctcOid(),
                details.getRctcVersion(),
                code,
                systemUrl,
                contentRef,
                valElement);
        return new Pair<>(true, xml);
      }
    }

    logger.info("Did not find a matched Code or value for the Medication");

    String xml =
        valElement
            ? CdaFhirUtilities.getCodeableConceptXmlForValue(code, elementType, contentRef)
            : CdaFhirUtilities.getCodeableConceptXml(code, elementType, contentRef);
    return new Pair<>(false, xml);
  }

  public static Pair<String, String> findMatchingCode(
      MatchedTriggerCodes mtc, CodeableConcept code, List<String> paths) {

    for (String s : paths) {
      Pair<String, String> matchedCode = mtc.getMatchingCode(code, s);
      if (matchedCode != null) {
        return matchedCode;
      }
    }
    return null; // Indicate no matching code found
  }

  public static CodeableConcept getMedicationCodeableConcept(Type mr, List<Medication> medList) {

    CodeableConcept cc = null;

    if (mr instanceof CodeableConcept) {
      cc = (CodeableConcept) mr;
    } else if (mr instanceof Reference) {
      Reference medRef = (Reference) mr;
      String medId = medRef.getReference();
      if (medId != null && !medId.isEmpty()) {
        if (medId.startsWith("Medication/")) {
          medId = medId.substring(11);
        }
        for (Medication m : medList) {
          if (m.getIdElement().getIdPart().equals(medId)) {
            if (m.hasCode()) {
              cc = m.getCode();
              break;
            }
          }
        }
      }
    }

    return cc;
  }

  /**
   * Gets name use attribute for XML.
   *
   * @param name the human name
   * @param isNameUseReq whether name use is required
   * @return the name use code or null
   */
  private static String getNameUseAttribute(HumanName name, boolean isNameUseReq) {
    if (isNameUseReq && name.hasUse()) {
      return CdaGeneratorConstants.getCodeForNameUse(name.getUse().toCode());
    }
    return null;
  }

  /**
   * Builds given names XML elements.
   *
   * @param ns the given names
   * @param name the human name
   * @param isQualifierReq whether qualifier is required
   * @param nameString the string builder
   * @return true if any given name was added
   */
  private static boolean buildGivenNamesForXml(
      List<StringType> ns, HumanName name, boolean isQualifierReq, StringBuilder nameString) {
    boolean hasGiven = false;

    for (StringType n : ns) {
      if (!StringUtils.isEmpty(n.getValue())) {
        hasGiven = true;
        String nameQualifier = null;
        if (name.getUse() != null && isQualifierReq) {
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
    return hasGiven;
  }

  /**
   * Adds last name XML element or NF placeholder.
   *
   * @param name the human name
   * @param nameString the string builder
   */
  private static void addLastNameXmlForHumanName(HumanName name, StringBuilder nameString) {
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

  /**
   * Handles empty names case for XML.
   *
   * @param nameString the string builder
   */
  private static void handleEmptyNamesForXml(StringBuilder nameString) {
    logger.debug("Did not find the Name for the patient ");
    nameString.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.NAME_EL_NAME));
    nameString.append(
        CdaGeneratorUtils.getXmlForNFText(
            CdaGeneratorConstants.FIRST_NAME_EL_NAME, CdaGeneratorConstants.NF_NI));
    nameString.append(
        CdaGeneratorUtils.getXmlForNFText(
            CdaGeneratorConstants.LAST_NAME_EL_NAME, CdaGeneratorConstants.NF_NI));
    nameString.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));
  }

  public static String getHumanNameXml(
      List<HumanName> allNames, boolean isQualifierReq, boolean isNameUseReq) {
    StringBuilder nameString = new StringBuilder(200);
    List<HumanName> names = filterActiveNames(allNames);

    if (names.isEmpty()) {
      handleEmptyNamesForXml(nameString);
      return nameString.toString();
    }

    Optional<HumanName> hName = names.stream().findFirst();
    if (!hName.isPresent()) {
      handleEmptyNamesForXml(nameString);
      return nameString.toString();
    }

    HumanName name = hName.get();
    String nameUse = getNameUseAttribute(name, isNameUseReq);

    nameString.append(
        CdaGeneratorUtils.getXmlForStartElementWithAttribute(
            CdaGeneratorConstants.NAME_EL_NAME, CdaGeneratorConstants.USE_ATTR_NAME, nameUse));

    List<StringType> ns = name.getGiven();
    boolean hasGiven = buildGivenNamesForXml(ns, name, isQualifierReq, nameString);

    // If Empty create NF
    if (!hasGiven) {
      nameString.append(
          CdaGeneratorUtils.getXmlForNFText(
              CdaGeneratorConstants.FIRST_NAME_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    addLastNameXmlForHumanName(name, nameString);
    nameString.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));

    return nameString.toString();
  }
}
