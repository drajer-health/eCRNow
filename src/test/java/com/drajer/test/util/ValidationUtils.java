package com.drajer.test.util;

import static org.junit.Assert.assertEquals;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.ecrapp.model.Eicr;
import java.util.List;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.v3.CD;
import org.hl7.v3.POCDMT000040ClinicalDocument;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ValidationUtils {

  private static final Logger logger = LoggerFactory.getLogger(ValidationUtils.class);

  public static POCDMT000040ClinicalDocument getClinicalDocXml(Eicr eicr) {

    JAXBContext jaxbContext;
    Unmarshaller jaxbUnmarshaller;
    POCDMT000040ClinicalDocument clinicalDoc = null;
    try {
      jaxbContext = JAXBContext.newInstance("org.hl7.v3:org.hl7.sdtc");
      jaxbUnmarshaller = jaxbContext.createUnmarshaller();

      Source source = new StreamSource(IOUtils.toInputStream(eicr.getData()));

      JAXBElement<POCDMT000040ClinicalDocument> root =
          jaxbUnmarshaller.unmarshal(source, POCDMT000040ClinicalDocument.class);

      clinicalDoc = root.getValue();
    } catch (JAXBException e) {
      logger.error("Error in unmarshalling EIRC. " + e.getMessage());
    }

    return clinicalDoc;
  }

  public static void validateAddress() {}

  public static void validateCode() {}

  public static void validateIdentifier() {}

  public static void validateTelecom() {}

  public static void validateCodeWithTranslation(List<CodeableConcept> codes, CD code) {

    if (codes != null && codes.size() > 0) {

      CodeableConcept cd = codes.get(0);
      List<Coding> codings = cd.getCoding();

      Boolean translation = false;
      int idx = -1;

      for (Coding c : codings) {

        Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(c.getSystem());

        if (!StringUtils.isEmpty(csd.getValue0())) {

          if (translation == false) {

            translation = true;
            assertEquals(c.getCode(), code.getCode());
            assertEquals(c.getDisplay(), code.getDisplayName());
            assertEquals(csd.getValue0(), code.getCodeSystem());
            assertEquals(csd.getValue1(), code.getCodeSystemName());
          } else {

            idx++;
            CD transCode = code.getTranslation().get(idx);
            assertEquals(c.getCode(), transCode.getCode());
            assertEquals(c.getDisplay(), transCode.getDisplayName());
            assertEquals(csd.getValue0(), transCode.getCodeSystem());
            assertEquals(csd.getValue1(), transCode.getCodeSystemName());
          }
        }
      }
    }
  }
}
