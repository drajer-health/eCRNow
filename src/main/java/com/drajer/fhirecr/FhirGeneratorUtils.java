package com.drajer.fhirecr;

import com.microsoft.sqlserver.jdbc.StringUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Composition.SectionComponent;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;

public class FhirGeneratorUtils {

  private FhirGeneratorUtils() {}

  public static Coding getCoding(String system, String code, String display) {

    Coding cd = null;

    if (system != null
        && code != null
        && !StringUtils.isEmpty(system)
        && !StringUtils.isEmpty(code)) {

      cd = new Coding();
      cd.setSystem(system);
      cd.setCode(code);

      if (!StringUtils.isEmpty(display)) cd.setDisplay(display);
    }

    return cd;
  }

  public static CodeableConcept getCodeableConcept(String system, String code, String display) {

    Coding cd = getCoding(system, code, display);
    CodeableConcept cc = null;

    if (cd != null) {

      cc = new CodeableConcept();

      cc.addCoding(cd);

      if (!StringUtils.isEmpty(display)) cc.setText(display);
    }

    return cc;
  }

  public static Reference getReference(Resource res) {

    Reference ref = new Reference();

    if (res != null) {
      ref.setResource(res);
    }

    return ref;
  }

  public static SectionComponent getSectionComponent(String system, String code, String display) {

    CodeableConcept cc = getCodeableConcept(system, code, display);
    SectionComponent sc = null;

    if (cc != null) {

      sc = new SectionComponent();
      sc.setCode(cc);
    }

    return sc;
  }

  public static String getText(CodeableConcept cc) {

    String retVal = FhirGeneratorConstants.UNKNOWN_VALUE;
    if (cc != null && !StringUtils.isEmpty(cc.getText())) {
      retVal = cc.getText();
    } else if (cc != null && cc.getCoding() != null && !cc.getCoding().isEmpty()) {
      StringBuilder sb = new StringBuilder();
      for (Coding cd : cc.getCoding()) {
        if (cd.getDisplay() != null && !StringUtils.isEmpty(cd.getDisplay())) {
          if (sb.toString().isEmpty()) {
            sb.append(cd.getDisplay());
          } else {
            sb.append(",").append(cd.getDisplay());
          }
        }
      }
      if (!sb.toString().isEmpty()) {
        retVal = sb.toString();
      }
    }
    return retVal;
  }

  public static String getReasonForVisitNarrativeText(Encounter en) {

    String retVal = FhirGeneratorConstants.UNKNOWN_VALUE;

    if (en != null && en.getReasonCodeFirstRep() != null) {

      retVal = getText(en.getReasonCodeFirstRep());
    }

    return retVal;
  }
}
