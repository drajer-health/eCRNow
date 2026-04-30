package com.drajer.cda.parser;

import static org.junit.Assert.*;

import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class CdaParserUtilitiesTest {

  private Element mockElement;
  private NodeList mockNodeList;

  @Before
  public void setUp() {
    mockElement = Mockito.mock(Element.class);
    mockNodeList = Mockito.mock(NodeList.class);
  }

  @Test
  public void testIsNodeListEmptyWithEmptyNodeList() {
    Mockito.when(mockNodeList.getLength()).thenReturn(0);
    assertTrue(CdaParserUtilities.isNodeListEmpty(mockNodeList));
  }

  @Test
  public void testReadTemplateId() {
    // Mocking the behavior of the Element object
    Mockito.when(mockElement.getAttribute("root")).thenReturn("rootValue");
    Mockito.when(mockElement.getAttribute("extension")).thenReturn("extValue");

    CdaIi result = CdaParserUtilities.readTemplateId(mockElement);

    assertNotNull(result);
    assertEquals("rootValue", result.getRootValue());
    assertEquals("extValue", result.getExtValue());
  }

  @Test
  public void testReadTemplateIdList() {
    NodeList nodeList = Mockito.mock(NodeList.class);
    Mockito.when(nodeList.getLength()).thenReturn(1);
    Mockito.when(nodeList.item(0)).thenReturn(mockElement);

    CdaIi cdaIi = new CdaIi();
    Mockito.when(mockElement.getAttribute("root")).thenReturn("root");
    Mockito.when(mockElement.getAttribute("extension")).thenReturn("ext");

    List<CdaIi> resultList = CdaParserUtilities.readTemplateIdList(nodeList);

    assertNotNull(resultList);
    assertEquals(1, resultList.size());
    assertEquals("root", resultList.get(0).getRootValue());
    assertEquals("ext", resultList.get(0).getExtValue());
  }

  @Test
  public void testReadCode() {

    Mockito.when(mockElement.getAttribute("code")).thenReturn("code123");
    Mockito.when(mockElement.getAttribute("codeSystem")).thenReturn("codeSystem123");
    Mockito.when(mockElement.getAttribute("codeSystemName")).thenReturn("SystemName123");
    Mockito.when(mockElement.getAttribute("displayName")).thenReturn("Display Name");
    Mockito.when(mockElement.getAttribute("xsi:type")).thenReturn("type123");
    Mockito.when(mockElement.getAttribute("nullFlavor")).thenReturn("null123");

    CdaCode result = CdaParserUtilities.readCode(mockElement);

    assertNotNull(result);
    assertEquals("code123", result.getCode());
    assertEquals("codeSystem123", result.getCodeSystem());
    assertEquals("SystemName123", result.getCodeSystemName());
    assertEquals("Display Name", result.getDisplayName());
    assertEquals("type123", result.getXpath());
    assertEquals("null123", result.getNullFlavor());
  }
}
