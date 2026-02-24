package com.drajer.sof.utils;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.*;

import org.junit.Test;
import org.springframework.web.servlet.config.annotation.ViewControllerRegistration;
import org.springframework.web.servlet.config.annotation.ViewControllerRegistry;

public class WebConfigurationTest {

  @Test
  public void testAddViewControllers() {
    WebConfiguration config = new WebConfiguration();
    ViewControllerRegistry registry = mock(ViewControllerRegistry.class);
    ViewControllerRegistration registration = mock(ViewControllerRegistration.class);
    when(registry.addViewController(anyString())).thenReturn(registration);
    config.addViewControllers(registry);
    verify(registry).addViewController("/{spring:\\w+}");
    verify(registry).addViewController("/**/{spring:\\w+}");
    verify(registry).addViewController("/{spring:\\w+}/**{spring:?!(\\.js|\\.css)$}");
    assertNotNull(registration);
    assertTrue(true);
  }
}
