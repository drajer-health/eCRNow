package com.drajer.cda.utils;

import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.doThrow;

import com.drajer.eca.model.ActionRepo;
import com.drajer.test.util.TestUtils;
import java.lang.reflect.Constructor;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class CdaValidatorUtilTest {

  @Before
  public void setUp() {
    ActionRepo.getInstance()
        .setXsdSchemasLocation("src/test/resources/AppData/Schema/infrastructure/cda/CDA_SDTC.xsd");
    ActionRepo.getInstance()
        .setSchematronFileLocation("src/test/resources/DSTU2/Misc/CCDAR21_updated.sch");
  }

  @Test
  public void testValidateEicrXMLData() throws Exception {
    String xmlData = TestUtils.getFileContentAsString("DSTU2/Misc/CDA.xml");
    Boolean validateEicrXMLData = CdaValidatorUtil.validateEicrXMLData(xmlData);
    assertTrue(validateEicrXMLData);
  }

  @Test
  public void testValidateEicrXMLDatawithNegativeXmlData() throws Exception {
    String xmlData = TestUtils.getFileContentAsString("DSTU2/Misc/ExpectedEICR/EICR_expected.xml");
    Boolean validateEicrXMLData = CdaValidatorUtil.validateEicrXMLData(xmlData);
    assertFalse(validateEicrXMLData);
  }

  @Test
  public void testValidateEicrXMLDataByException() throws Exception {
    assertThatExceptionOfType(RuntimeException.class)
        .isThrownBy(
            () -> {
              doThrow(RuntimeException.class).when(CdaValidatorUtil.validateEicrXMLData(""));
            });
  }

  @Test
  public void testValidateEicrToSchematron() throws Exception {
    String eicrData = TestUtils.getFileContentAsString("DSTU2/Misc/ExpectedEICR/EICR_expected.xml");
    Boolean validateEicrToSchematron = CdaValidatorUtil.validateEicrToSchematron(eicrData);
    assertTrue(validateEicrToSchematron);
  }

  @Test
  public void testValidateEicrToSchematronWithNegationSchematron() throws Exception {
    String eicrData = TestUtils.getFileContentAsString("DSTU2/Misc/ExpectedEICR/EICR_expected.xml");
    ActionRepo.getInstance()
        .setSchematronFileLocation(
            "src/test/resources/AppData/Schema/infrastructure/cda/CDA_SDTC.xsd");
    Boolean validateEicrToSchematron = CdaValidatorUtil.validateEicrToSchematron(eicrData);
    assertFalse(validateEicrToSchematron);
  }

  @Test
  public void testValidateEicrToSchematronByException() throws Exception {
    assertThatExceptionOfType(RuntimeException.class)
        .isThrownBy(
            () -> {
              doThrow(RuntimeException.class).when(CdaValidatorUtil.validateEicrToSchematron(""));
            });
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
  public void testPrivateConstructor() {
    Constructor<CdaValidatorUtil> constructor = null;
    try {
      constructor = CdaValidatorUtil.class.getDeclaredConstructor();
      constructor.setAccessible(true);
      CdaValidatorUtil cdaValidatorUtil = constructor.newInstance();
      assertTrue(false);
    } catch (IllegalStateException e) {
      assertTrue(true);
    } catch (Exception e) {
      assertTrue(true);
    }
  }
}
