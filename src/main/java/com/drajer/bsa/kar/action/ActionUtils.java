package com.drajer.bsa.kar.action;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.Meta;

/**
 * This is a utlities class for processing actions.
 *
 * @author nbashyam
 */
public class ActionUtils {

  public static Meta getMeta(String version, String profile) {

    Meta m = new Meta();
    m.setVersionId(version);
    CanonicalType ct = new CanonicalType();
    ct.setValueAsString(profile);
    List<CanonicalType> profiles = new ArrayList<>();
    profiles.add(ct);
    m.setProfile(profiles);
    m.setLastUpdated(Date.from(Instant.now()));

    return m;
  }
}
