package com.drajer.test.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

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
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Address.AddressUse;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.v3.AD;
import org.hl7.v3.ANY;
import org.hl7.v3.AdxpCity;
import org.hl7.v3.AdxpCountry;
import org.hl7.v3.AdxpPostalCode;
import org.hl7.v3.AdxpState;
import org.hl7.v3.AdxpStreetAddressLine;
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

  public static void validateAddress(List<Address> r4addr, List<AD> cdaAddr) {

    int idx = -1;

    for (Address addr : r4addr) {

      // Only HOME and WORK addresses are Supported currently.
      // May need to change this validation once more address type are supported.
      if (addr.getUseElement().getValue() == AddressUse.HOME) {

        idx++;
        AD ad = cdaAddr.get(idx);
        assertNotNull(ad);
        validateAddress(addr, ad);

      } else if (addr.getUseElement().getValue() == AddressUse.WORK) {

        idx++;
        AD ad = cdaAddr.get(idx);
        assertNotNull(ad);
        validateAddress(addr, ad);
      }
    }
  }

  public static void validateAddress(Address addr, AD ad) {

    int adIdx = 0;
    // street address
    List<StringType> lines = addr.getLine();
    if (lines != null && lines.size() > 0) {

      for (StringType line : lines) {

        AdxpStreetAddressLine streetAddressLine =
            (AdxpStreetAddressLine) ad.getContent().get(adIdx);
        assertNotNull(streetAddressLine);
        assertEquals(line.getValue(), streetAddressLine.getPartType().get(0));
        adIdx++;
      }
    }

    // city
    AdxpCity city = (AdxpCity) ad.getContent().get(adIdx);
    assertNotNull(city);
    adIdx++;
    if (addr.getCity() != null) {
      assertEquals(addr.getCity(), city.getPartType().get(0));
    } else {
      validateNullFlavor(city, "NI");
    }

    // state
    AdxpState state = (AdxpState) ad.getContent().get(adIdx);
    assertNotNull(state);
    adIdx++;
    if (addr.getState() != null) {
      assertEquals(addr.getState(), state.getPartType().get(0));
    } else {
      validateNullFlavor(state, "NI");
    }

    // postal code
    AdxpPostalCode postCode = (AdxpPostalCode) ad.getContent().get(adIdx);
    assertNotNull(postCode);
    adIdx++;
    if (addr.getPostalCode() != null) {
      assertEquals(addr.getPostalCode(), postCode.getPartType().get(0));
    } else {
      validateNullFlavor(postCode, "NI");
    }

    // country
    AdxpCountry country = (AdxpCountry) ad.getContent().get(adIdx);
    assertNotNull(country);
    if (addr.getCountry() != null) {
      assertEquals(addr.getCountry(), country.getPartType().get(0));
    } else {
      validateNullFlavor(country, "NI");
    }
  }

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

  public static void validateNullFlavor(ANY instance, String nullFlavor) {
    assertNotNull(instance);
    assertEquals(1, instance.getNullFlavor().size());
    assertEquals(nullFlavor, instance.getNullFlavor().get(0));
  }
}
