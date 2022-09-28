package com.drajer.bsa.kar.action;

import com.drajer.bsa.model.KarProcessingData;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.codesystems.V3ParticipationType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ReportCreationUtilities {

  private static final Logger logger = LoggerFactory.getLogger(ReportCreationUtilities.class);

  public static List<Practitioner> getPractitioners(
      KarProcessingData data, V3ParticipationType type) {

    List<Practitioner> result = new ArrayList<>();
    Set<Resource> res = data.getResourcesByType(ResourceType.Practitioner);

    for (Resource r : res) {
      Practitioner p = (Practitioner) r;
      result.add(p);
    }

    // In the future check and retrieve practitioners from the encounter resource.
    return result;
  }

  public static Organization getOrganization(KarProcessingData kd) {

    Set<Resource> res = kd.getResourcesByType(ResourceType.Organization);
    Organization org = null;
    if (res != null && res.size() > 0) {
      org = (Organization) (res.iterator().next());
    }

    return org;
  }
}
