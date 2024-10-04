package com.drajer.bsa.kar.action;

import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.eca.model.MatchedTriggerCodes;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import org.apache.commons.collections4.SetUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 *
 * <h1>CheckTriggerCodeStatus</h1>
 *
 * The class provides the elements required to track check trigger code action exeution status.
 *
 * @author nbashyam
 */
public class CheckTriggerCodeStatus extends BsaActionStatus {

  private final Logger logger = LoggerFactory.getLogger(CheckTriggerCodeStatus.class);

  /** The attribute indicates if something matched or not. */
  private Boolean triggerMatchStatus; // Did anything match or not

  /**
   * The attribute maintains the state of the specific codes that resulted in a match against the
   * value set.
   */
  private List<MatchedTriggerCodes> matchedCodes;

  /** The map stores the matches by specific data requirement ids specified for the action. */
  private Map<String, Set<Resource>> matchedResources;

  public Pair<Boolean, ReportableMatchedTriggerCode> getMatchedCode(CodeableConcept cc) {

    Pair<Boolean, ReportableMatchedTriggerCode> rmtc = new Pair<>(false, null);

    Set<String> codes = BsaServiceUtils.getMatchableCodes(cc);

    for (MatchedTriggerCodes mtc : matchedCodes) {

      Set<String> codesToMatch = mtc.getMatchedCodes();
      Set<String> matches = SetUtils.intersection(codes, codesToMatch);

      if (matches == null || matches.isEmpty()) {
        matches = SetUtils.intersection(codes, mtc.getMatchedValues());
      }

      if (matches != null && !matches.isEmpty()) {
        ReportableMatchedTriggerCode rc = new ReportableMatchedTriggerCode();
        rc.setValueSet(mtc.getValueSet());
        rc.setValueSetOid(mtc.getValueSetOid());
        rc.setValueSetVersion(mtc.getValueSetVersion());

        Optional<String> value = matches.stream().findFirst();
        if (value.isPresent()) {
          String rcode = value.get();
          String[] rcodes = rcode.split("\\|");

          rc.setCode(rcodes[1]);
          rc.setCodeSystem(rcodes[0]);
        }

        rc.setAllMatches(matches);

        rmtc = rmtc.setAt0(true);
        rmtc = rmtc.setAt1(rc);

        break;
      }
    }

    return rmtc;
  }

  public MatchedTriggerCodes getMatchedTriggerCodes(
      String path, String valueSet, String valuesetVersion) {

    for (MatchedTriggerCodes mtc : matchedCodes) {

      if (mtc.getMatchedPath().contains(path)
          && mtc.getValueSet().contains(valueSet)
          && mtc.getValueSetVersion().contains(valuesetVersion)) return mtc;
    }

    return null;
  }

  public Boolean containsMatches(ResourceType rt) {

    for (MatchedTriggerCodes mtc : matchedCodes) {

      if (Boolean.TRUE.equals(mtc.containsMatch(rt))) return true;
    }

    return false;
  }

  public void addMatchedTriggerCodes(MatchedTriggerCodes mtc) {

    if (mtc != null) {
      matchedCodes.add(mtc);
    }
  }

  public void addMatchedCodes(
      Set<String> codes, String valueSet, String path, String valueSetVersion) {

    MatchedTriggerCodes mtcCodes = getMatchedTriggerCodes(path, valueSet, valueSetVersion);

    if (mtcCodes == null) {
      mtcCodes = new MatchedTriggerCodes();
      mtcCodes.setMatchedCodes(codes);
      mtcCodes.setValueSet(valueSet);
      mtcCodes.setValueSetVersion(valueSetVersion);
      mtcCodes.setMatchedPath(path);
      matchedCodes.add(mtcCodes);
      triggerMatchStatus = true;
    } else {
      mtcCodes.addCodes(codes);
      triggerMatchStatus = true;
    }
  }

  public CheckTriggerCodeStatus() {
    super();
    matchedCodes = new ArrayList<>();
    triggerMatchStatus = false;
  }

  public Boolean getTriggerMatchStatus() {
    return triggerMatchStatus;
  }

  public void setTriggerMatchStatus(Boolean triggerMatchStatus) {
    this.triggerMatchStatus = triggerMatchStatus;
  }

  public List<MatchedTriggerCodes> getMatchedCodes() {
    return matchedCodes;
  }

  public void setMatchedCodes(List<MatchedTriggerCodes> matchedCodes) {
    this.matchedCodes = matchedCodes;
  }

  public Map<String, Set<Resource>> getMatchedResources() {
    return matchedResources;
  }

  public void setMatchedResources(Map<String, Set<Resource>> matchedResources) {
    this.matchedResources = matchedResources;
  }

  public void copyFrom(CheckTriggerCodeStatus ctc) {

    for (MatchedTriggerCodes mtc : ctc.getMatchedCodes()) {

      matchedCodes.add(mtc);
    }
  }

  public void log() {

    logger.info(" *** START Printing Check Trigger Code Status *** ");
    logger.info(" Trigger Match Status {}", triggerMatchStatus);

    for (MatchedTriggerCodes mtc : matchedCodes) {

      mtc.log();
    }

    logger.info(" *** End Printing Check Trigger Code Status *** ");
  }

  public boolean isCodePresent(String code, String matchedPath) {

    for (MatchedTriggerCodes mtc : matchedCodes) {

      if (mtc.isCodePresent(code, matchedPath)) {

        logger.info(" Found Code {} for Path {}", code, matchedPath);
        return true;
      }
    }

    return false;
  }
}
