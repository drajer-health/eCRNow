package com.drajer.test.util;

import java.util.List;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.ContactPoint.ContactPointSystem;
import org.hl7.fhir.r4.model.Identifier;

public class FhirR4Utils {

  public static Identifier getIdentifierBySystem(List<Identifier> r4Identifiers, String system) {

    for (Identifier identifier : r4Identifiers) {

      if (identifier.getSystem() != null && identifier.getSystem().contentEquals(system)) {
        return identifier;
      }
    }

    return null;
  }

  public static Identifier getIdentiferByType(List<Identifier> r4Identifiers, String type) {

    for (Identifier identifier : r4Identifiers) {

      if (identifier.getType() != null) {

        List<Coding> codings = identifier.getType().getCoding();

        for (Coding coding : codings) {

          if (coding.getCode().contentEquals(type)) {
            return identifier;
          }
        }
      }
    }

    return null;
  }

  public static ContactPoint getTelecomByType(
      List<ContactPoint> r4Telecoms, ContactPointSystem type) {

    for (ContactPoint telecom : r4Telecoms) {

      if (telecom.getSystem() == type) {
        return telecom;
      }
    }

    return null;
  }
}
