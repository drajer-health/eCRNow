package com.drajer.ecrapp.repository;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.api.IQueryParameterType;
import ca.uhn.fhir.rest.api.MethodOutcome;
import java.util.List;
import java.util.Map;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.instance.model.api.IBaseParameters;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.instance.model.api.IIdType;
import org.opencds.cqf.fhir.api.Repository;

public class EcrRepository implements Repository {
  private FhirContext ctx;

  public EcrRepository(FhirContext ctx) {
    this.ctx = ctx;
  }

  @Override
  public <T extends IBaseResource, I extends IIdType> T read(
      Class<T> aClass, I i, Map<String, String> map) {
    return null;
  }

  @Override
  public <T extends IBaseResource> MethodOutcome create(T t, Map<String, String> map) {
    return null;
  }

  @Override
  public <T extends IBaseResource> MethodOutcome update(T t, Map<String, String> map) {
    return null;
  }

  @Override
  public <T extends IBaseResource, I extends IIdType> MethodOutcome delete(
      Class<T> aClass, I i, Map<String, String> map) {
    return null;
  }

  @Override
  public <B extends IBaseBundle, T extends IBaseResource> B search(
      Class<B> aClass,
      Class<T> aClass1,
      Map<String, List<IQueryParameterType>> map,
      Map<String, String> map1) {
    return null;
  }

  @Override
  public <R extends IBaseResource, P extends IBaseParameters, T extends IBaseResource> R invoke(
      Class<T> aClass, String s, P p, Class<R> aClass1, Map<String, String> map) {
    return null;
  }

  @Override
  public <R extends IBaseResource, P extends IBaseParameters, I extends IIdType> R invoke(
      I i, String s, P p, Class<R> aClass, Map<String, String> map) {
    return null;
  }

  @Override
  public FhirContext fhirContext() {
    return ctx;
  }
}
