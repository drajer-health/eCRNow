package com.drajer.bsa.kar.action;

import com.drajer.eca.model.MatchedTriggerCodes;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

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

  private Boolean triggerMatchStatus; // Did anything match or not
  private List<MatchedTriggerCodes> matchedCodes;

  public MatchedTriggerCodes getMatchedTriggerCodes(
      String path, String valueSet, String valuesetVersion) {

    for (MatchedTriggerCodes mtc : matchedCodes) {

      if (mtc.getMatchedPath().contains(path)
          && mtc.getValueSet().contains(valueSet)
          && mtc.getValueSetVersion().contains(valuesetVersion)) return mtc;
    }

    return null;
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

    this.triggerMatchStatus = ctc.triggerMatchStatus;
    this.matchedCodes.addAll(ctc.getMatchedCodes());
  }
}
