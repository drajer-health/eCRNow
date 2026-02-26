package com.drajer.cda.utils;

import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.drajer.eca.model.ActionRepo;
import com.drajer.test.util.TestUtils;
import java.lang.reflect.Constructor;
import org.junit.Before;
import org.junit.Test;

public class CdaValidatorUtilTest {

  @Before
  public void setUp() {

    ActionRepo.getInstance()
        .setXsdSchemasLocation("src/test/resources/AppData/Schema/infrastructure/cda/CDA_SDTC.xsd");

    ActionRepo.getInstance()
        .setSchematronFileLocation("src/test/resources/DSTU2/Misc/CCDAR21_updated.sch");
  }

  @Test
  public void testValidateEicrXMLData() {

    String xmlData = TestUtils.getFileContentAsString("DSTU2/Misc/CDA.xml");

    Boolean result = CdaValidatorUtil.validateEicrXMLData(xmlData);

    assertTrue(result);
  }

  @Test
  public void testValidateEicrXMLDatawithNegativeXmlData() {

    String xmlData = TestUtils.getFileContentAsString("DSTU2/Misc/ExpectedEICR/EICR_expected.xml");

    Boolean result = CdaValidatorUtil.validateEicrXMLData(xmlData);

    assertFalse(result);
  }

  @Test
  public void testValidateEicrToSchematron() {

    String eicrData = TestUtils.getFileContentAsString("DSTU2/Misc/ExpectedEICR/EICR_expected.xml");

    Boolean result = CdaValidatorUtil.validateEicrToSchematron(eicrData);

    assertTrue(result);
  }

  @Test
  public void testValidateEicrToSchematronWithNegationSchematron() {

    String eicrData = TestUtils.getFileContentAsString("DSTU2/Misc/ExpectedEICR/EICR_expected.xml");

    ActionRepo.getInstance()
        .setSchematronFileLocation(
            "src/test/resources/AppData/Schema/infrastructure/cda/CDA_SDTC.xsd");

    Boolean result = CdaValidatorUtil.validateEicrToSchematron(eicrData);

    assertFalse(result);
  }

  @Test
  public void testValidateEicrXmlDataWithNegationSchema() {

    ActionRepo.getInstance()
        .setXsdSchemasLocation("src/test/resources/DSTU2/Misc/ExpectedEICR/EICR_expected.xml");

    String eicrData = TestUtils.getFileContentAsString("DSTU2/Misc/ExpectedEICR/EICR_expected.xml");

    boolean validationResult = CdaValidatorUtil.validateEicrXMLData(eicrData);

    assertFalse(validationResult);
  }

  @Test
  public void testPrivateConstructor() throws Exception {

    Constructor<CdaValidatorUtil> constructor = CdaValidatorUtil.class.getDeclaredConstructor();

    constructor.setAccessible(true);

    assertThatThrownBy(() -> constructor.newInstance()).isInstanceOf(Throwable.class);
  }
}
