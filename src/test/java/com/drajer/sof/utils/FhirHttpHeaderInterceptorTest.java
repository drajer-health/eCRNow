package com.drajer.sof.utils;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

public class FhirHttpHeaderInterceptorTest {

  private FhirHttpHeaderInterceptor interceptor;

  @Before
  public void setup() {
    interceptor = new FhirHttpHeaderInterceptor("REQ-123");
  }

  @Test
  public void testConstructor_WithRequestId() {
    assertNotNull(interceptor);
    assertEquals("REQ-123", interceptor.getXReqId());

    String value = interceptor.getValue();
    assertNotNull(value);
    assertTrue(value.startsWith("REQ-123-"));
    assertTrue(value.contains("p1"));
    assertTrue(value.contains("r0"));
  }

  @Test
  public void testConstructor_WithNullRequestId() {
    FhirHttpHeaderInterceptor interceptorNull = new FhirHttpHeaderInterceptor(null);

    assertNotNull(interceptorNull.getXReqId());
    assertFalse(interceptorNull.getXReqId().isEmpty());

    String value = interceptorNull.getValue();
    assertNotNull(value);
    assertTrue(value.contains("p1"));
    assertTrue(value.contains("r0"));
  }

  @Test
  public void testNewRandomNum() {
    String random1 = interceptor.newRandomNum();
    String random2 = interceptor.newRandomNum();

    assertNotNull(random1);
    assertEquals(5, random1.length());
    assertNotNull(random2);

    assertEquals(5, random2.length());

    String value = interceptor.getValue();
    assertTrue(value.contains(random2));
  }

  @Test
  public void testSetPageNum() {
    interceptor.setPageNum(5);

    String value = interceptor.getValue();
    assertTrue(value.contains("p5"));
  }

  @Test
  public void testIncrementPageNum() {
    int page = interceptor.incrementPageNum();

    assertEquals(2, page);
    assertTrue(interceptor.getValue().contains("p2"));

    page = interceptor.incrementPageNum();
    assertEquals(3, page);
    assertTrue(interceptor.getValue().contains("p3"));
  }

  @Test
  public void testSetRetryCount() {
    interceptor.setRetryCount(3);

    String value = interceptor.getValue();
    assertTrue(value.contains("r3"));
  }

  @Test
  public void testReset() {
    interceptor.setPageNum(10);
    interceptor.setRetryCount(5);

    String beforeReset = interceptor.getValue();

    interceptor.reset();

    String afterReset = interceptor.getValue();

    assertTrue(afterReset.contains("p1"));
    assertTrue(afterReset.contains("r0"));
    assertNotEquals(beforeReset, afterReset);
  }

  @Test
  public void testSetXReqId() {
    interceptor.setXReqId("NEW-REQ-ID");

    assertEquals("NEW-REQ-ID", interceptor.getXReqId());

    String value = interceptor.getValue();
    assertTrue(value.startsWith("NEW-REQ-ID-"));
  }
}
