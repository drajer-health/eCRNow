package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import org.springframework.test.util.ReflectionTestUtils;

public class ReportCreatorTest {

  private static final String HCS_PROFILE =
      "http://hl7.org/fhir/us/health-care-surveys-reporting/StructureDefinition/hcs-reporting-bundle";

  @Before
  public void setUp() {
    ReportCreator.getReportCreator(HCS_PROFILE);
  }

  @Test
  public void testInitReportingClasses_ClassNotFound() throws Exception {
    Field field = ReportCreator.class.getDeclaredField("reportingClasses");
    field.setAccessible(true);
    HashMap<String, ReportCreator> map = (HashMap<String, ReportCreator>) field.get(null);
    map.clear();
    map.put(HCS_PROFILE, null);
    ReportCreator result = ReportCreator.getReportCreator(HCS_PROFILE);
    assertNull(result);
    assertEquals(1, map.size());
    assertTrue(map.containsKey(HCS_PROFILE));
  }

  @Test
  public void testGetReportCreator_loadsEveryConfiguredCreatorFromTheProperties() {
    Object original = ReflectionTestUtils.getField(ReportCreator.class, "reportingClasses");
    try {
      ReflectionTestUtils.setField(
          ReportCreator.class, "reportingClasses", new HashMap<String, ReportCreator>());

      assertEquals(
          EcrReportCreator.class,
          ReportCreator.getReportCreator(
                  "http://hl7.org/fhir/us/ecr/StructureDefinition/eicr-document-bundle")
              .getClass());
      assertEquals(
          EcrMeasureReportCreator.class,
          ReportCreator.getReportCreator(
                  "http://hl7.org/fhir/us/ecr/StructureDefinition/eicr-measurereport-bundle")
              .getClass());
      assertEquals(HcsReportCreator.class, ReportCreator.getReportCreator(HCS_PROFILE).getClass());
      assertEquals(
          CcrrReportCreator.class,
          ReportCreator.getReportCreator(
                  "http://hl7.org/fhir/us/central-cancer-registry-reporting/StructureDefinition/"
                      + "ccrr-reporting-bundle")
              .getClass());
      assertEquals(
          RespnetReportCreator.class,
          ReportCreator.getReportCreator(
                  "http://hl7.org/fhir/us/resp-net/StructureDefinition/resp-net-reporting-bundle")
              .getClass());
      assertEquals(
          MedMorphReportCreator.class,
          ReportCreator.getReportCreator(
                  "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-reporting-bundle")
              .getClass());
      assertNull(ReportCreator.getReportCreator("http://example.org/unknown-profile"));
    } finally {
      ReflectionTestUtils.setField(ReportCreator.class, "reportingClasses", original);
    }
  }

  @Test
  public void testInitReportingClasses_registersOnlyTheConfiguredCreators() {
    Object original = ReflectionTestUtils.getField(ReportCreator.class, "reportingClasses");
    HashMap<String, ReportCreator> registry = new HashMap<>();
    try {
      ReflectionTestUtils.setField(ReportCreator.class, "reportingClasses", registry);

      // initializeReportingClass(String, Properties) was folded into initReportingClasses(),
      // which always reads create-report.properties from the classpath.
      ReportCreator.initReportingClasses();

      assertFalse("the configured creators must be registered", registry.isEmpty());
      assertTrue(
          "every registered entry must be a real ReportCreator",
          registry.values().stream().allMatch(c -> c instanceof ReportCreator));
      assertTrue(registry.get(HCS_PROFILE) instanceof HcsReportCreator);
      assertFalse(
          "an unconfigured profile must not be registered",
          registry.containsKey("http://example.org/unknown-profile"));
    } finally {
      ReflectionTestUtils.setField(ReportCreator.class, "reportingClasses", original);
    }
  }

  // =====================================================================================
  // New tests added for more coverage. The existing tests above are unchanged.
  //
  // initReportingClasses() always reads "create-report.properties" through
  // ReportCreator.class.getClassLoader(). To reach its catch blocks without touching the real
  // properties file, ReportCreator is loaded once more in a small isolated class loader that
  // serves its own version of that file. JaCoCo still records the coverage for ReportCreator.
  // =====================================================================================

  @Test
  public void testInitReportingClasses_badEntriesAreLoggedAndSkipped() throws Exception {
    // missing            -> ClassNotFoundException
    // abstractClass      -> InstantiationException
    // privateConstructor -> IllegalAccessException
    String properties =
        """
                missing=com.example.DoesNotExist
                abstractClass=com.drajer.bsa.kar.action.ReportCreator
                privateConstructor=java.lang.Math
                """;

    Map<?, ?> registry =
        runInitReportingClassesWith(
            new ByteArrayInputStream(properties.getBytes(StandardCharsets.UTF_8)));

    assertTrue("none of the bad entries may be registered", registry.isEmpty());
  }

  @Test
  public void testInitReportingClasses_unreadablePropertiesFile_isLogged() throws Exception {
    InputStream unreadable =
        new InputStream() {
          @Override
          public int read() throws IOException {
            throw new IOException("disk error");
          }
        };

    Map<?, ?> registry = runInitReportingClassesWith(unreadable);

    assertTrue(registry.isEmpty());
  }

  // ---------------- helpers for the new tests ----------------

  /**
   * Loads a separate copy of ReportCreator whose class loader returns {@code propertiesFile} for
   * "create-report.properties", runs initReportingClasses() on it and returns its registry.
   */
  private static Map<?, ?> runInitReportingClassesWith(InputStream propertiesFile)
      throws Exception {
    IsolatedReportCreatorLoader loader = new IsolatedReportCreatorLoader(propertiesFile);
    Class<?> isolated = loader.loadClass(ReportCreator.class.getName());
    assertNotSame("must be a separate copy of the class", ReportCreator.class, isolated);

    isolated.getMethod("initReportingClasses").invoke(null);

    Field registry = isolated.getDeclaredField("reportingClasses");
    registry.setAccessible(true);
    return (Map<?, ?>) registry.get(null);
  }

  private static final class IsolatedReportCreatorLoader extends ClassLoader {

    private final InputStream propertiesFile;

    IsolatedReportCreatorLoader(InputStream propertiesFile) {
      super(ReportCreatorTest.class.getClassLoader());
      this.propertiesFile = propertiesFile;
    }

    @Override
    protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
      if (!ReportCreator.class.getName().equals(name)) {
        return super.loadClass(name, resolve);
      }
      synchronized (getClassLoadingLock(name)) {
        Class<?> loaded = findLoadedClass(name);
        if (loaded == null) {
          String resource = name.replace('.', '/') + ".class";
          try (InputStream in = getParent().getResourceAsStream(resource)) {
            byte[] bytes = in.readAllBytes();
            loaded = defineClass(name, bytes, 0, bytes.length);
          } catch (IOException e) {
            throw new ClassNotFoundException(name, e);
          }
        }
        return loaded;
      }
    }

    @Override
    public InputStream getResourceAsStream(String name) {
      if ("create-report.properties".equals(name)) {
        return propertiesFile;
      }
      return super.getResourceAsStream(name);
    }
  }
}
