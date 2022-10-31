package com.drajer.bsa.kar.action;

import com.drajer.eca.model.MatchedTriggerCodes;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CheckTriggerCodeStatusList {

  private final Logger logger = LoggerFactory.getLogger(CheckTriggerCodeStatusList.class);

  Set<CheckTriggerCodeStatus> statuses;

  public CheckTriggerCodeStatusList() {
    statuses = new HashSet<>();
  }

  public Set<CheckTriggerCodeStatus> getStatuses() {
    return statuses;
  }

  public void setStatuses(Set<CheckTriggerCodeStatus> statuses) {
    this.statuses = statuses;
  }

  public void addCheckTriggerCodeStatus(CheckTriggerCodeStatus ctcs) {

    if (statuses != null) {
      statuses.add(ctcs);
    } else {
      statuses = new HashSet<>();
      statuses.add(ctcs);
    }
  }

  public Boolean compareCodes(CheckTriggerCodeStatusList currentTriggerMatchStatus) {

    Boolean retVal = false;
    if (statuses != null && !statuses.isEmpty()) {

      Set<CheckTriggerCodeStatus> currentStatuses = currentTriggerMatchStatus.getStatuses();

      for (CheckTriggerCodeStatus ctc : currentStatuses) {

        List<MatchedTriggerCodes> mtcs = ctc.getMatchedCodes();

        for (MatchedTriggerCodes m : mtcs) {

          Set<String> matchedCodes = m.getMatchedCodes();
          matchedCodes.addAll(m.getMatchedValues());

          String matchedPath = m.getMatchedPath();

          for (String s : matchedCodes) {

            // If any of the current ones do not match, then it is sufficient to continue
            // creation of a new message.
            if (Boolean.FALSE.equals(isCodePresent(s, matchedPath))) {

              logger.info(" Found new Code {} that matched for path {}", s, matchedPath);
              retVal = true;
              break;
            }
          }
        }
      }

    } else if (currentTriggerMatchStatus.getStatuses() != null
        && !currentTriggerMatchStatus.getStatuses().isEmpty()) {

      // Nothing to compare, since old is not present.
      retVal = true;
    }

    return retVal;
  }

  public Boolean isCodePresent(String code, String matchedPath) {

    for (CheckTriggerCodeStatus ctc : statuses) {

      if (ctc.isCodePresent(code, matchedPath)) {

        logger.info(" Found Code {} for Path {}", code, matchedPath);
        return true;
      }
    }

    return false;
  }
}
