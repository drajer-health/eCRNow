package com.drajer.cda.utils;

import static org.junit.Assert.assertFalse;

import com.drajer.eca.model.ActionRepo;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.test.util.ReflectionTestUtils;

public class CdaValidatorUtilTest {

  private static final String SCHEMATRON =
      "src/test/resources/AppData/schematrons/CdaR11/CDAR2_IG_PHCASERPT_R2_STU1.1_SCHEMATRON.sch";

  private static Object savedActionRepo;

  @BeforeClass
  public static void useTheRealSchemaLocation() {
    savedActionRepo = ReflectionTestUtils.getField(ActionRepo.class, "instance");
    ReflectionTestUtils.setField(ActionRepo.class, "instance", null);
    ActionRepo.getInstance()
        .setXsdSchemasLocation("src/test/resources/AppData/Schema/infrastructure/cda/CDA_SDTC.xsd");
  }

  @AfterClass
  public static void restoreActionRepo() {
    ReflectionTestUtils.setField(ActionRepo.class, "instance", savedActionRepo);
  }

  @Test
  public void validateEicrXMLData_rejectsDocumentsThatBreakTheCdaSchema() {
    assertFalse(
        CdaValidatorUtil.validateEicrXMLData("<ClinicalDocument xmlns=\"urn:hl7-org:v3\"/>"));
    assertFalse(CdaValidatorUtil.validateEicrXMLData("<ClinicalDocument"));
  }

  @Test
  public void validateEicrToSchematron_failsForAMissingSchematronOrUnreadableData() {
    assertFalse(CdaValidatorUtil.validateEicrToSchematron("<ClinicalDocument/>", "missing.sch"));
    assertFalse(CdaValidatorUtil.validateEicrToSchematron("<ClinicalDocument", SCHEMATRON));
  }
}
