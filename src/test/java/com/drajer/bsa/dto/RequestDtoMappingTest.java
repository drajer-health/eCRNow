package com.drajer.bsa.dto;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KnowledgeArtifactRepository;
import com.drajer.bsa.model.PublicHealthAuthority;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import org.junit.Test;
import org.springframework.core.io.ClassPathResource;
import tools.jackson.databind.json.JsonMapper;

/**
 * The request DTOs replace the JPA entities as {@code @RequestBody} types (Sonar java:S4684). These
 * tests check that the JSON contract is unchanged: the same JSON read into the entity directly (the
 * old binding) and read into the DTO then converted with {@code toEntity()} (the new binding)
 * produce entities with identical property values, with both Jackson 2 and Jackson 3.
 */
public class RequestDtoMappingTest {

  private static final ObjectMapper JACKSON2 =
      new ObjectMapper().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

  private static final JsonMapper JACKSON3 = JsonMapper.builder().build();

  @Test
  public void healthcareSettingRequest_bindsLikeTheEntity() throws Exception {
    assertSameBinding(
        HealthcareSettingRequest.class,
        HealthcareSetting.class,
        HealthcareSettingRequest::toEntity);
  }

  @Test
  public void healthcareSettingRequest_sampleFileBindsLikeTheEntity() throws Exception {
    String json =
        new String(
            new ClassPathResource("Bsa/HealthCareSettings.json").getInputStream().readAllBytes());

    HealthcareSetting direct = JACKSON2.readValue(json, HealthcareSetting.class);
    HealthcareSetting viaDto = JACKSON2.readValue(json, HealthcareSettingRequest.class).toEntity();

    assertNotNull(direct.getFhirServerBaseURL());
    assertSameProperties(direct, viaDto);
  }

  @Test
  public void knowledgeArtifactRepositoryRequest_bindsLikeTheEntity() throws Exception {
    assertSameBinding(
        KnowledgeArtifactRepositoryRequest.class,
        KnowledgeArtifactRepository.class,
        KnowledgeArtifactRepositoryRequest::toEntity);
  }

  @Test
  public void knowledgeArtifactStatusRequest_bindsLikeTheEntity() throws Exception {
    assertSameBinding(
        KnowledgeArtifactStatusRequest.class,
        KnowledgeArtifactStatus.class,
        KnowledgeArtifactStatusRequest::toEntity);
  }

  @Test
  public void publicHealthAuthorityRequest_bindsLikeTheEntity() throws Exception {
    assertSameBinding(
        PublicHealthAuthorityRequest.class,
        PublicHealthAuthority.class,
        PublicHealthAuthorityRequest::toEntity);
  }

  @Test
  public void emptyJson_keepsTheEntityDefaults() throws Exception {
    assertSameProperties(
        JACKSON2.readValue("{}", HealthcareSetting.class),
        JACKSON2.readValue("{}", HealthcareSettingRequest.class).toEntity());
    assertSameProperties(
        JACKSON2.readValue("{}", PublicHealthAuthority.class),
        JACKSON2.readValue("{}", PublicHealthAuthorityRequest.class).toEntity());

    KnowledgeArtifactStatus direct = JACKSON2.readValue("{}", KnowledgeArtifactStatus.class);
    KnowledgeArtifactStatus viaDto =
        JACKSON2.readValue("{}", KnowledgeArtifactStatusRequest.class).toEntity();
    assertEquals(direct.getKarId(), viaDto.getKarId());
    assertEquals(direct.getIsActive(), viaDto.getIsActive());
    assertEquals(direct.getSubscriptionsEnabled(), viaDto.getSubscriptionsEnabled());
    assertEquals(direct.getOutputFormat(), viaDto.getOutputFormat());
    assertEquals(direct.getSubscriptions(), viaDto.getSubscriptions());
  }

  /**
   * Fills every simple property of a request, writes it to JSON with the mapper (so the JSON uses
   * the mapper's own property names), and reads that JSON back both ways.
   */
  private static <D, E> void assertSameBinding(
      Class<D> dtoType, Class<E> entityType, Function<D, E> toEntity) throws Exception {
    D sample = populated(dtoType);

    String json2 = JACKSON2.writeValueAsString(sample);
    assertSameProperties(
        JACKSON2.readValue(json2, entityType), toEntity.apply(JACKSON2.readValue(json2, dtoType)));

    String json3 = JACKSON3.writeValueAsString(sample);
    assertSameProperties(
        JACKSON3.readValue(json3, entityType), toEntity.apply(JACKSON3.readValue(json3, dtoType)));
  }

  private static <D> D populated(Class<D> type) throws Exception {
    D dto = type.getDeclaredConstructor().newInstance();
    int n = 1;
    int filled = 0;
    for (Method setter : type.getMethods()) {
      if (!setter.getName().startsWith("set") || setter.getParameterCount() != 1) {
        continue;
      }
      Object value = sampleValue(setter, n++);
      if (value != null) {
        setter.invoke(dto, value);
        filled++;
      }
    }
    assertTrue("no properties were populated for " + type.getSimpleName(), filled > 0);
    return dto;
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  private static Object sampleValue(Method setter, int n) {
    Class<?> t = setter.getParameterTypes()[0];
    if (t == String.class) return "value-" + n;
    if (t == Integer.class || t == int.class) return n;
    if (t == Boolean.class || t == boolean.class) return n % 2 == 0;
    if (t == Date.class) return new Date(1_700_000_000_000L + n * 1000L);
    if (t.isEnum()) return t.getEnumConstants()[n % t.getEnumConstants().length];
    if (t == Set.class
        && setter.getGenericParameterTypes()[0] instanceof ParameterizedType pt
        && pt.getActualTypeArguments()[0] == String.class) {
      Set set = new TreeSet<>();
      set.add("a-" + n);
      set.add("b-" + n);
      return set;
    }
    return null; // nested entities are left at their defaults on both sides
  }

  private static void assertSameProperties(Object expected, Object actual) throws Exception {
    List<String> checked = new ArrayList<>();
    for (Method getter : expected.getClass().getMethods()) {
      String name = getter.getName();
      boolean isGetter =
          (name.startsWith("get") || name.startsWith("is"))
              && getter.getParameterCount() == 0
              && !Modifier.isStatic(getter.getModifiers())
              && !name.equals("getClass");
      if (!isGetter) {
        continue;
      }
      assertEquals(
          expected.getClass().getSimpleName() + "." + name,
          getter.invoke(expected),
          getter.invoke(actual));
      checked.add(name);
    }
    assertTrue(checked.size() > 1);
  }
}
