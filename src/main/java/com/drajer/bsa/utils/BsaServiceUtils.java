package com.drajer.bsa.utils;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.fhirpath.IFhirPath;
import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.kar.action.BsaActionStatus;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatus;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatusList;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.FhirQueryFilter;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.BsaTypes.MessageType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.fhirecr.FhirGeneratorConstants;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;
import org.hl7.fhir.instance.model.api.IBase;
import org.hl7.fhir.r4.hapi.fluentpath.FhirPathR4;
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.DocumentReference.DocumentReferenceContentComponent;
import org.hl7.fhir.r4.model.Parameters.ParametersParameterComponent;
import org.hl7.fhir.r4.model.ValueSet.ConceptReferenceComponent;
import org.hl7.fhir.r4.model.ValueSet.ConceptSetComponent;
import org.hl7.fhir.r4.model.ValueSet.ValueSetComposeComponent;
import org.hl7.fhir.r4.model.ValueSet.ValueSetExpansionComponent;
import org.hl7.fhir.r4.model.ValueSet.ValueSetExpansionContainsComponent;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

/**
 *
 *
 * <h1>BsaServiceUtils</h1>
 *
 * This class implements a few utilities to be used by many of the different services.
 *
 * @author nbashyam
 */
@Service
public class BsaServiceUtils {

  private static final Logger logger = LoggerFactory.getLogger(BsaServiceUtils.class);

  @Autowired
  @Qualifier("jsonParser")
  IParser jsonParser;

  @Value("${bsa.output.directory}")
  String debugDirectory;

  @Value("${save.debug.files:true}")
  boolean saveDebugToFiles;

  @Autowired(required = false)
  Map<String, BsaActionStatus> actions;

  private static String DEBUG_DIRECTORY;
  private static IParser FHIR_JSON_PARSER;
  private static boolean SAVE_DEBUG_TO_FILES;

  private static final String FHIR_PATH_VARIABLE_PREFIX = "%";
  private static IFhirPath FHIR_PATH = new FhirPathR4(FhirContext.forR4());

  @PostConstruct
  public void initialize() {
    DEBUG_DIRECTORY = debugDirectory;
    FHIR_JSON_PARSER = jsonParser;
    SAVE_DEBUG_TO_FILES = saveDebugToFiles;
  }

  public static String getFhirPathVariableString(String id) {

    if (id.length() > 2) {

      String part1 = id.substring(0, 1).toLowerCase();
      String part2 = id.substring(1);

      return FHIR_PATH_VARIABLE_PREFIX + part1 + part2;
    }

    return id.toLowerCase();
  }

  public Bundle readKarFromFile(String filePath) {

    logger.info("About to read KAR File {}", filePath);
    Bundle bundle = null;
    try (InputStream in = new FileInputStream(new File(filePath))) {
      logger.info("Start Reading KAR File ");

      bundle = jsonParser.parseResource(Bundle.class, in);
      logger.info("Completed Reading KAR File");
    } catch (Exception e) {
      logger.error("Exception Reading KAR File", e);
    }
    return bundle;
  }

  public static Set<Resource> filterResources(
      Set<Resource> resources, DataRequirement dataRequirement, KarProcessingData kd) {

    List<DataRequirement.DataRequirementCodeFilterComponent> codeFilters =
        dataRequirement.getCodeFilter();
    List<DataRequirement.DataRequirementDateFilterComponent> dateFilters =
        dataRequirement.getDateFilter();
    List<CanonicalType> profileFilters = dataRequirement.getProfile();

    Set<Resource> filtered = filterByCodeFilters(resources, codeFilters, kd);
    filtered = filterByDateFilters(filtered, dateFilters, kd);
    filtered = filterByProfileFilters(filtered, profileFilters, kd);
    // gather all codes that
    return filtered;
  }

  private static Set<Resource> filterByProfileFilters(
      Set<Resource> resources, List<CanonicalType> profileFilters, KarProcessingData kd) {
    Set<Resource> filtered = new HashSet<>();
    for (Resource res : resources) {
      boolean matches = true;
      for (CanonicalType profile : profileFilters) {
        if (!matchesProfile(res, profile, kd)) {
          matches = false;
          break;
        }
      }
      if (matches) {
        logger.info("Resource matches filter {}", res.getId());
        filtered.add(res);
      }
    }
    return filtered;
  }

  private static boolean matchesProfile(
      Resource res, CanonicalType drProfile, KarProcessingData kd) {
    logger.info("KarProcessingData in matchesProfile:{}", kd);
    if (drProfile == null) {
      return true;
    } else if (!res.hasMeta() || !res.getMeta().hasProfile()) {
      return false;
    } else {
      return !res.getMeta()
          .getProfile()
          .stream()
          .filter(resProfile -> resProfile.getValueAsString().equals(drProfile.getValueAsString()))
          .collect(Collectors.toList())
          .isEmpty();
    }
  }

  public static Set<Resource> filterByCodeFilters(
      Set<Resource> resources,
      List<DataRequirement.DataRequirementCodeFilterComponent> codeFilters,
      KarProcessingData kd) {
    Set<Resource> filtered = new HashSet<>();
    for (Resource res : resources) {
      boolean matches = true;
      for (DataRequirement.DataRequirementCodeFilterComponent drcfc : codeFilters) {
        if (!matchesCodeFilter(res, drcfc, kd)) {
          matches = false;
          break;
        }
      }
      if (matches) {
        logger.info("Resource matches filter {}", res.getId());
        filtered.add(res);
      }
    }
    return filtered;
  }

  public static Set<Resource> filterByDateFilters(
      Set<Resource> resources,
      List<DataRequirement.DataRequirementDateFilterComponent> dateFilters,
      KarProcessingData kd) {
    Set<Resource> filtered = new HashSet<>();
    for (Resource res : resources) {
      boolean matches = true;
      for (DataRequirement.DataRequirementDateFilterComponent drdfc : dateFilters) {
        if (!matchesDateFilter(res, drdfc, kd)) {
          matches = false;
          break;
        }
      }
      if (matches) {
        filtered.add(res);
      }
    }
    return filtered;
  }

  public static boolean matchesCodeFilter(
      Resource resource,
      DataRequirement.DataRequirementCodeFilterComponent codeFilter,
      KarProcessingData kd) {
    // find the attribute by the path element in the code filter: this may be a list of codes or
    // codableconcepts
    // if the filter is contains a valueset match against that
    // if the filter contains codes match, against them -- at this stage the matches are ORs.  If
    // the
    // vs or
    // any of the codes match its a match.

    // we dont know what this will return
    List<IBase> search = FHIR_PATH.evaluate(resource, codeFilter.getPath(), IBase.class);
    if (search == null || search.isEmpty()) {
      return false;
    }

    boolean retVal = false;

    for (IBase ib : search) {
      if (codeFilter.hasValueSet()) {
        logger.info("Code Filter has value set");
        if (matchesValueSet(ib, codeFilter.getValueSet(), kd)) {
          retVal = true;
          break;
        }
      }
      if (codeFilter.hasCode()) {
        logger.info("Code Filter has code");
        if (matchesCodes(ib, codeFilter.getCode(), kd)) {
          retVal = true;
          break;
        }
      }
    }
    return retVal;
  }

  public static boolean matchesValueSet(IBase ib, String url, KarProcessingData kd) {
    ValueSet vs = (ValueSet) kd.getKar().getDependentResource(ResourceType.ValueSet, url);
    if (ib instanceof Coding) {
      Coding coding = (Coding) ib;
      return isCodePresentInValueSet(vs, coding.getSystem(), coding.getCode());
    } else if (ib instanceof CodeableConcept) {
      return isCodeableConceptPresentInValueSet(vs, (CodeableConcept) ib);
    }
    return false;
  }

  public static boolean matchesCodes(IBase ib, List<Coding> codes, KarProcessingData kd) {
    logger.info("KarProcessingData in matchesCose:{}", kd);
    if (ib instanceof Coding) {
      Coding ibc = (Coding) ib;
      return codes
          .stream()
          .anyMatch(
              coding ->
                  ibc.getSystem().equals(coding.getSystem())
                      && ibc.getCode().equals(coding.getCode()));
    }
    if (ib instanceof CodeableConcept) {
      CodeableConcept ibc = (CodeableConcept) ib;
      List<Coding> ibcCodings = ibc.getCoding();
      return ibcCodings
          .stream()
          .anyMatch(
              ibcCoding ->
                  codes
                      .stream()
                      .anyMatch(
                          coding ->
                              ibcCoding.getSystem().equals(coding.getSystem())
                                  && ibcCoding.getCode().equals(coding.getCode())));
    }
    return false;
  }

  public static boolean matchesDateFilter(
      Resource r, DataRequirement.DataRequirementDateFilterComponent drdfc, KarProcessingData kd) {
    logger.info(
        "Resource :{} DataRequirementDateFilterComponent :{} KarProcessingData:{}", r, drdfc, kd);

    return true;
  }

  public static Boolean isCodeableConceptPresentInValueSet(ValueSet vs, CodeableConcept cc) {
    return cc.getCoding()
        .stream()
        .anyMatch(coding -> isCodePresentInValueSet(vs, coding.getSystem(), coding.getCode()));
  }

  public static Pair<Boolean, MatchedTriggerCodes> isCodeableConceptPresentInValueSet(
      ValueSet vs, CodeableConcept cd, String path, boolean valElem) {

    Pair<Boolean, MatchedTriggerCodes> retVal = null;
    boolean matchFound = false;
    MatchedTriggerCodes mtc = null;

    if (cd != null && !cd.getCoding().isEmpty()) {

      for (Coding c : cd.getCoding()) {

        Pair<Boolean, Pair<String, String>> retInfo = isCodingPresentInValueSet(vs, c);

        if (retInfo != null) {

          logger.info(" Match Found for code {} | {}", retInfo.getValue0(), retInfo.getValue1());

          if (mtc == null) {
            mtc = new MatchedTriggerCodes();
            mtc.setValueSet(vs.getUrl());
            mtc.setValueSetOid(getValueSetOid(vs));
            mtc.setValueSetVersion(vs.getVersion());
            mtc.setMatchedPath(path);
            matchFound = true;

            if (valElem) {
              logger.info(" Matched Code is part of a Value Element ");
              mtc.addValue(retInfo.getValue1().getValue0() + "|" + retInfo.getValue1().getValue1());
            } else {
              logger.info(" Matched Code is part of a Code Element ");
              mtc.addCode(retInfo.getValue1().getValue0() + "|" + retInfo.getValue1().getValue1());
            }
          }
        }
      }
    }

    if (matchFound) {
      retVal = new Pair<>(true, mtc);
    }

    return retVal;
  }

  private static String getValueSetOid(ValueSet vs) {

    String vsOid = FhirGeneratorConstants.RCTC_OID;

    if (vs.hasIdentifier()) {

      List<Identifier> ids = vs.getIdentifier();

      for (Identifier id : ids) {
        if (id.hasSystem()
            && id.getSystem().contentEquals(FhirGeneratorConstants.DOC_ID_SYSTEM)
            && id.hasValue()) vsOid = id.getValue();
      }
    }

    return vsOid;
  }

  public static Set<String> getMatchableCodes(CodeableConcept cc) {

    Set<String> mtcs = new HashSet<>();

    if (cc != null && !cc.getCoding().isEmpty()) {

      for (Coding c : cc.getCoding()) {

        if (c.getSystem() != null && c.getCode() != null) {

          mtcs.add(c.getSystem() + "|" + c.getCode());
        }
      }
    }

    return mtcs;
  }

  public static Pair<Boolean, Pair<String, String>> isCodingPresentInValueSet(
      ValueSet vs, Coding coding) {

    Pair<Boolean, Pair<String, String>> retVal = null;

    if (coding != null
        && coding.hasCode()
        && coding.hasSystem()
        && Boolean.TRUE.equals(isCodePresentInValueSet(vs, coding.getSystem(), coding.getCode()))) {
      Pair<String, String> matchedCodeInfo = new Pair<>(coding.getSystem(), coding.getCode());
      retVal = new Pair<>(true, matchedCodeInfo);
    }

    return retVal;
  }

  public static Boolean isCodePresentInValueSet(ValueSet vs, String system, String code) {

    boolean retVal = false;

    if (vs != null && vs.hasCompose()) {

      ValueSetComposeComponent vsc = vs.getCompose();

      List<ConceptSetComponent> cscs = vsc.getInclude();

      if (cscs != null) {

        for (ConceptSetComponent csc : cscs) {

          if (csc.getSystem() != null && csc.getSystem().contentEquals(system)) {

            logger.info(" Found Code System {} in value set ", system);

            List<ConceptReferenceComponent> crcs = csc.getConcept();

            if (crcs != null) {

              for (ConceptReferenceComponent crc : crcs) {

                if (crc.getCode().contentEquals(code)) {
                  logger.info(" Found code system {} and code {} in value set ", system, code);
                  retVal = true;
                  break;
                }
              }
            }
          }
        }
      }
    }

    if (!retVal && vs.hasExpansion()) {

      ValueSetExpansionComponent vsec = vs.getExpansion();

      if (vsec.hasContains()) {

        List<ValueSetExpansionContainsComponent> expansion = vsec.getContains();

        for (ValueSetExpansionContainsComponent vsecc : expansion) {

          if (vsecc.getSystem() != null
              && vsecc.getSystem().contentEquals(system)
              && vsecc.getCode() != null
              && vsecc.getCode().contentEquals(code)) {

            logger.info(
                " Found Match for CodeSystem {} and Code {} in ValueSet {}", system, code, vs);
            retVal = true;
            break;
          }
        }
      }
    }

    return retVal;
  }

  /**
   * Find all document references in the bundle.
   *
   * @param res
   * @param docs
   */
  public static MessageHeader findMessageHeaderAndDocumentReferences(
      Resource res, List<DocumentReference> docs) {

    MessageHeader header = null;

    if (res.getResourceType() == ResourceType.Bundle) {

      Bundle b = (Bundle) res;

      List<BundleEntryComponent> becs = b.getEntry();

      for (BundleEntryComponent bec : becs) {

        if (bec.getResource().getResourceType() == ResourceType.MessageHeader) {

          header = (MessageHeader) bec.getResource();

        } else if (bec.getResource().getResourceType() == ResourceType.DocumentReference) {

          docs.add((DocumentReference) (bec.getResource()));

        } // if it is a doc ref
        else if (bec.getResource().getResourceType() == ResourceType.Bundle) {

          findMessageHeaderAndDocumentReferences(bec.getResource(), docs);
        }
      } // For entries
    } // if res is a bundle

    return header;
  }

  /**
   * The method takes a FHIR Resource and checks to see if the Document Bundle contains a Document
   * Reference resource When a Document Reference resource is found, it saves the attachment part of
   * the DocumentReference in an XML file. The attachment is expected to be a CDA document.
   *
   * @param res
   * @return - Pair<String, String>, the first value is the Id of the Payload, the second value is
   *     the Payload.
   */
  public static List<Pair<String, String>> saveCdaDocumentFromDocumentBundleToFile(
      String logDirectory, String actionType, Resource res) {

    List<DocumentReference> docs = new ArrayList<>();
    List<Pair<String, String>> outputs = new ArrayList<>();

    if (SAVE_DEBUG_TO_FILES) {

      findMessageHeaderAndDocumentReferences(res, docs);

      for (DocumentReference docRef : docs) {

        logger.info(" Found a document reference that needs to be saved ");
        String fileName =
            logDirectory
                + actionType
                + "_"
                + docRef.getSubject().getReferenceElement().getIdPart()
                + "_"
                + docRef.getId()
                + ".xml";

        if (!docRef.getContent().isEmpty()) {

          DocumentReferenceContentComponent drcc = docRef.getContentFirstRep();

          if (drcc.getAttachment() != null) {
            Attachment att = drcc.getAttachment();

            String payload = new String(att.getData());

            logger.debug("Saving data to file {}", fileName);
            saveDataToFile(payload, fileName);

            Pair<String, String> p = new Pair<>(docRef.getId(), payload);
            outputs.add(p);
          } // attachment not null
        } // DocRef has content
      } // For all document references.
    }

    return outputs;
  }

  /**
   * The method saves the provided data to a file.
   *
   * @param data -- The data to be saved.
   * @param filename -- The filename to be used for saving the data.
   */
  public static void saveDataToFile(String data, String filename) {

    if (SAVE_DEBUG_TO_FILES) {
      try (DataOutputStream outStream =
          new DataOutputStream(new BufferedOutputStream(new FileOutputStream(filename)))) {

        logger.info(" Writing data to file: {}", filename);
        outStream.writeBytes(data);
      } catch (IOException e) {
        logger.debug(" Unable to write data to file: {}", filename, e);
      }
    }
  }

  /**
   * The method can save the resource to a file as a JSON object.
   *
   * @param res
   */
  public void saveResourceToFile(Resource res) {

    if (SAVE_DEBUG_TO_FILES) {
      String fileName =
          debugDirectory + res.getResourceType().toString() + "_" + res.getId() + ".json";

      String data = jsonParser.encodeResourceToString(res);

      saveDataToFile(data, fileName);
    }
  }

  public static void saveFhirResourceToFile(Resource res, String filename) {

    if (SAVE_DEBUG_TO_FILES) {
      String fileName = DEBUG_DIRECTORY + filename + ".json";

      String data = FHIR_JSON_PARSER.encodeResourceToString(res);

      saveDataToFile(data, fileName);
    }
  }

  /**
   * Checks the data provided in the bundle to examine if it is a message type bundle and if it
   * contains data related to a CDA document.
   *
   * @param res - Bundle to be examined.
   * @return
   */
  public static Boolean hasCdaData(Resource res) {

    if (res.getResourceType() == ResourceType.Bundle) {

      Bundle b = (Bundle) res;

      if (b.getType() == BundleType.MESSAGE && b.getEntry().size() > 1) {

        BundleEntryComponent bec = b.getEntryFirstRep();

        if (bec.getResource().getResourceType() == ResourceType.MessageHeader) {

          MessageHeader mh = (MessageHeader) bec.getResource();

          if (mh.getEventCoding() != null
              && mh.getEventCoding()
                  .getCode()
                  .equals(BsaTypes.getMessageTypeString(MessageType.CDA_EICR_MESSAGE))) {

            return true;
          } // If Message Type
        } // MEssage Header
      } // Message Type
    } // Bundle

    return false;
  }

  public static void convertDataToParameters(
      String dataReqId, String fhirType, String limit, Set<Resource> resources, Parameters params) {

    if (resources != null && !resources.isEmpty()) {
      for (Resource res : resources) {

        logger.info(" Creating Parameter for name {}", dataReqId);
        ParametersParameterComponent parameter =
            new ParametersParameterComponent().setName("%" + String.format("%s", dataReqId));
        parameter.addExtension(
            "http://hl7.org/fhir/uv/cpg/StructureDefinition/cpg-parameterDefinition",
            new ParameterDefinition().setMax(limit).setName("%" + dataReqId).setType(fhirType));
        parameter.setResource(res);
        params.addParameter(parameter);
      }
    } else {
      logger.info(" Creating Parameter for name {} with no resources", dataReqId);
      ParametersParameterComponent parameter =
          new ParametersParameterComponent().setName("%" + String.format("%s", dataReqId));
      parameter.addExtension(
          "http://hl7.org/fhir/uv/cpg/StructureDefinition/cpg-parameterDefinition",
          new ParameterDefinition().setMax(limit).setName("%" + dataReqId).setType(fhirType));
      params.addParameter(parameter);
    }
  }

  public static Map<String, FhirQueryFilter> getDefaultQueriesForAction(
      BsaAction act, KnowledgeArtifact art) {

    // Get the default queries.
    Map<String, FhirQueryFilter> queries = act.getInputDataRequirementQueries();

    // Get the related data elements for Inputs
    Map<String, String> relatedDataReqIds = act.getInputDataIdToRelatedDataIdMap();

    for (Map.Entry<String, String> entry : relatedDataReqIds.entrySet()) {

      // If the query existed, they should already have been collected.
      if (!queries.containsKey(entry.getKey())) {

        FhirQueryFilter filter = art.getQueryFilter(entry.getKey(), entry.getValue());

        // This will not be null only when there is a related data Id mapped.
        if (filter != null) {
          queries.put(entry.getValue(), filter);
        }
      }
    }

    return queries;
  }

  public static CheckTriggerCodeStatusList getTriggerMatchStatus(String data) {

    ObjectMapper mapper = new ObjectMapper();
    CheckTriggerCodeStatusList state = null;

    try {

      state = mapper.readValue(data, CheckTriggerCodeStatusList.class);

    } catch (JsonProcessingException e1) {
      String msg = "Unable to read/write Trigger Match state";
      logger.error(msg, e1);
      throw new RuntimeException(msg, e1);
    }

    return state;
  }

  public static String getEncodedTriggerMatchStatus(
      CheckTriggerCodeStatusList ctc, KarProcessingData kd, String eicrId) {
    ObjectMapper mapper = new ObjectMapper();

    String state = null;
    try {

      for (CheckTriggerCodeStatus ctcs : ctc.getStatuses()) {

        ctcs.setActionId(
            kd.getScheduledJobData().getActionId() + kd.getScheduledJobData().getJobId());
        ctcs.setActionType(kd.getScheduledJobData().getActionType());
        ctcs.setActionStatus(BsaActionStatusType.COMPLETED);
        Set<String> eicrIds = new HashSet<String>();
        eicrIds.add(eicrId);
        ctcs.setOutputProduced(eicrIds);
      }

      state = mapper.writeValueAsString(ctc);
    } catch (JsonProcessingException e) {

      String msg = "Unable to update execution state";
      logger.error(msg, e);
      throw new RuntimeException(msg, e);
    }

    return state;
  }
}
