package com.drajer.test.Assertion;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.drajer.test.util.ValueSetMapping;
import java.util.List;
import javax.xml.bind.JAXBElement;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.v3.ANY;
import org.hl7.v3.CD;
import org.hl7.v3.CE;
import org.hl7.v3.CS;
import org.hl7.v3.II;
import org.hl7.v3.IVLTS;
import org.hl7.v3.QTY;
import org.hl7.v3.StrucDocTable;
import org.hl7.v3.StrucDocTh;
import org.hl7.v3.StrucDocThead;
import org.hl7.v3.TEL;
import org.hl7.v3.TS;

public class AssertCdaElement {

  public static void assertTelecomPhone(ContactPoint r4Telecom, TEL cdaTelecom) {

    assertTrue(cdaTelecom.getValue().startsWith("tel:"));
    if (r4Telecom.getUse().toCode() != "") {
      String useCode =
          ValueSetMapping.telecomUseCodes.get(r4Telecom.getUse().toCode().toUpperCase());
      assertEquals(1, cdaTelecom.getUse().size());
      assertEquals(useCode, cdaTelecom.getUse().get(0));
    }
  }

  public static void assertTelecomPhone(TEL cdaTelecom, String value, String use) {

    assertTrue(cdaTelecom.getValue().startsWith("tel:"));
    if (use != null && !use.isEmpty()) {
      String useCode = ValueSetMapping.telecomUseCodes.get(use.toUpperCase());
      assertEquals(1, cdaTelecom.getUse().size());
      assertEquals(useCode, cdaTelecom.getUse().get(0));
    }
  }

  public static void assertTelecomEmail(ContactPoint r4Telecom, TEL cdaTelecom) {

    assertTrue(cdaTelecom.getValue().startsWith("mailto:"));
    assertEquals(r4Telecom.getValue(), cdaTelecom.getValue().substring(6));
    if (r4Telecom.getUse().toCode() != "") {
      String useCode =
          ValueSetMapping.telecomUseCodes.get(r4Telecom.getUse().toCode().toUpperCase());
      assertEquals(1, cdaTelecom.getUse().size());
      assertEquals(useCode, cdaTelecom.getUse().get(0));
    }
  }

  public static void assertTelecomEmail(TEL cdaTelecom, String value, String use) {

    assertTrue(cdaTelecom.getValue().startsWith("mailto:"));
    assertEquals(value, cdaTelecom.getValue().substring(6));
    if (use != null && !use.isEmpty()) {
      String useCode = ValueSetMapping.telecomUseCodes.get(use.toUpperCase());
      assertEquals(1, cdaTelecom.getUse().size());
      assertEquals(useCode, cdaTelecom.getUse().get(0));
    }
  }

  public static void assertCodeCE(
      CE codes, String code, String codeSystem, String codeSystemName, String displayName) {

    assertNotNull(codes);
    assertEquals(code, codes.getCode());
    assertEquals(codeSystem, codes.getCodeSystem());
    assertEquals(codeSystemName, codes.getCodeSystemName());
    assertEquals(displayName, codes.getDisplayName());
  }

  public static void assertTemplateID(II templateID, String root, String extension) {

    assertNotNull(templateID);
    assertEquals(root, templateID.getRoot());
    if (extension != null) {
      assertEquals(extension, templateID.getExtension());
    }
  }

  public static void assertID(II id, String root, String extension) {

    assertNotNull(id);
    assertEquals(root, id.getRoot());
    if (extension != null) {
      assertEquals(extension, id.getExtension());
    }
  }

  public static void assertNullFlavor(ANY instance, String nullFlavor) {
    assertNotNull(instance);
    assertEquals(1, instance.getNullFlavor().size());
    assertEquals(nullFlavor, instance.getNullFlavor().get(0));
  }

  public static void assertEffectiveDtTm(IVLTS effDtTm, String high, String low) {

    assertNotNull(effDtTm);
    if (low != null && !low.isEmpty()) {
      TS lowTime = (TS) ((JAXBElement<? extends QTY>) effDtTm.getRest().get(0)).getValue();
      assertEquals(low, lowTime.getValue());
    }

    if (high != null && !high.isEmpty()) {
      TS highTime = (TS) ((JAXBElement<? extends QTY>) effDtTm.getRest().get(1)).getValue();
      assertEquals(high, highTime.getValue());
    }

    // Note: Spec doesn't say anything on nullflavour and hence not supported.

  }

  public static void assertTableBorderAndWidth(StrucDocTable table, String border, String width) {

    assertNotNull(table);
    assertEquals(border, table.getBorder());
    assertEquals(width, table.getWidth());
  }

  public static void assertTableHeader(StrucDocTable table, List<String> tableHeadingTitles) {

    StrucDocThead thead = table.getThead();
    assertNotNull(thead);

    // assert number of columns
    List<Object> ths = thead.getTr().get(0).getThOrTd();
    assertEquals(tableHeadingTitles.size(), ths.size());

    // assert column headers
    int loopIndex = 0;
    for (String s : tableHeadingTitles) {
      StrucDocTh th = (StrucDocTh) ths.get(loopIndex++);
      assertEquals(s, th.getContent().get(0));
    }
  }

  public static void assertCodeCD(
      CD codeObj, String code, String codeSystem, String codeSystemName, String displayName) {
    assertEquals(code, codeObj.getCode());
    assertEquals(codeSystem, codeObj.getCodeSystem());
    assertEquals(codeSystemName, codeObj.getCodeSystemName());
    assertEquals(displayName, codeObj.getDisplayName());
  }

  public static void assertCodeCS(
      CS codeObj, String code, String codeSystem, String codeSystemName, String displayName) {
    assertEquals(code, codeObj.getCode());
    assertEquals(codeSystem, codeObj.getCodeSystem());
    assertEquals(codeSystemName, codeObj.getCodeSystemName());
    assertEquals(displayName, codeObj.getDisplayName());
  }
}
