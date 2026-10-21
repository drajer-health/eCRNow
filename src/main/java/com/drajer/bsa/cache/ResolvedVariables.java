package com.drajer.bsa.cache;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import org.hl7.fhir.r4.model.Type;

/**
 * Immutable holder for the STATIC PlanDefinition variables resolved for a single Knowledge
 * Artifact. Each instance belongs to exactly one KAR (keyed externally by {@code
 * KnowledgeArtifact.getVersionUniqueId()}) and holds only that KAR's own variables - values are
 * never shared or merged across KARs.
 *
 * @author nbashyam
 */
public final class ResolvedVariables {

  private final Map<String, Type> variables;

  public ResolvedVariables(Map<String, Type> variables) {
    this.variables =
        Collections.unmodifiableMap(variables == null ? new HashMap<>() : new HashMap<>(variables));
  }

  public Optional<Type> getVariable(String name) {
    return Optional.ofNullable(variables.get(name));
  }

  public boolean hasVariable(String name) {
    return variables.containsKey(name);
  }

  public Map<String, Type> getVariables() {
    return variables;
  }

  public int size() {
    return variables.size();
  }
}
