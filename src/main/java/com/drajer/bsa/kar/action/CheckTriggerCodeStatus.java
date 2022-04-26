package com.drajer.bsa.kar.action;

import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.eca.model.MatchedTriggerCodes;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.apache.commons.collections4.SetUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
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

  private Boolean triggerMatchStatus; // Did anything match or not
  private List<MatchedTriggerCodes> matchedCodes;

  public Pair<Boolean, ReportableMatchedTriggerCode> getMatchedCode(CodeableConcept cc) {

    Pair<Boolean, ReportableMatchedTriggerCode> rmtc = new Pair<>(false, null);

    Set<String> codes = BsaServiceUtils.getMatchableCodes(cc);

    for (MatchedTriggerCodes mtc : matchedCodes) {

      Set<String> codesToMatch = mtc.getMatchedCodes();
      Set<String> matches = SetUtils.intersection(codes, codesToMatch);

      if (matches != null && !matches.isEmpty()) {

        ReportableMatchedTriggerCode rc = new ReportableMatchedTriggerCode();
        rc.setValueSet(mtc.getValueSet());
        rc.setValueSetVersion(mtc.getValueSetVersion());

        String rcode = matches.stream().findFirst().get();

        String[] rcodes = rcode.split("\\|");

        rc.setCode(rcodes[1]);
        rc.setCodeSystem(rcodes[0]);
        rc.setAllMatches(matches);

        rmtc = new Pair<>(true, rc);

        break;
      } else {

        matches = SetUtils.intersection(codes, mtc.getMatchedValues());

        if (matches != null && !matches.isEmpty()) {
          ReportableMatchedTriggerCode rc = new ReportableMatchedTriggerCode();
          rc.setValueSet(mtc.getValueSet());
          rc.setValueSetVersion(mtc.getValueSetVersion());

          String rcode = matches.stream().findFirst().get();
          String[] rcodes = rcode.split("|");

          rc.setCode(rcodes[1]);
          rc.setCodeSystem(rcodes[0]);
          rc.setAllMatches(matches);

          rmtc.setAt0(true);
          rmtc.setAt1(rc);

          break;
        }
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
      Set<String> codes, String valueSet, String path, String valuesetVersion) {

    MatchedTriggerCodes mtc = getMatchedTriggerCodes(path, valueSet, valuesetVersion);

    if (mtc == null) {
      mtc = new MatchedTriggerCodes();
      mtc.setMatchedCodes(codes);
      mtc.setValueSet(valueSet);
      mtc.setValueSetVersion(valuesetVersion);
      mtc.setMatchedPath(path);
      matchedCodes.add(mtc);
      triggerMatchStatus = true;
    } else {
      mtc.addCodes(codes);
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
}
