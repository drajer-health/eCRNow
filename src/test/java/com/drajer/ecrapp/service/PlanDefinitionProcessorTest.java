package com.drajer.ecrapp.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import com.drajer.eca.model.AbstractAction;
import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.ecrapp.config.ValueSetSingleton;
import com.drajer.ersd.service.ValueSetService;
import com.drajer.test.util.TestUtils;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.commons.io.FileUtils;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.ValueSet;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class PlanDefinitionProcessorTest {

  private static final String ERSD_EXAMPLE = "Bsa/Scenarios/kars/ErsdV2/ErsdV2Bundle1.json";
  private static final String ERSD_EXAMPLE_FILE = "src/test/resources/" + ERSD_EXAMPLE;

  private final IParser parser = FhirContext.forR4Cached().newJsonParser();
  private final ValueSetService valueSetService = mock(ValueSetService.class);
  private Set<ValueSet> savedValueSets;
  private Set<ValueSet> savedGrouperValueSets;
  private Set<ValueSet> savedEmergentValueSets;
  private Map<EcrActionTypes, Set<AbstractAction>> savedActions;
  private String savedRctcVersion;

  @Before
  public void saveSharedState() {
    ValueSetSingleton singleton = ValueSetSingleton.getInstance();
    savedValueSets = singleton.getValueSets();
    savedGrouperValueSets = singleton.getGrouperValueSets();
    savedEmergentValueSets = singleton.getEmergentValueSets();
    savedActions = ActionRepo.getInstance().getActions();
    savedRctcVersion = ActionRepo.getInstance().getRctcVersion();
  }

  /**
   * PlanDefinitionProcessor now uses @Autowired/@Value field injection (no-arg constructor only).
   * This test is in the same package, so the package-private fields are set directly. The ERSD
   * client stays null, as it was before.
   */
  private PlanDefinitionProcessor newProcessor(String ersdFileLocation) {
    PlanDefinitionProcessor processor = new PlanDefinitionProcessor();
    processor.jsonParser = parser;
    processor.valueSetService = valueSetService;
    processor.ersdFileLocation = ersdFileLocation;
    return processor;
  }

  @After
  public void restoreSharedState() {
    ValueSetSingleton singleton = ValueSetSingleton.getInstance();
    singleton.setValueSets(savedValueSets);
    singleton.setGrouperValueSets(savedGrouperValueSets);
    singleton.setEmergentValueSets(savedEmergentValueSets);
    ActionRepo.getInstance().setActions(savedActions);
    ActionRepo.getInstance().setRctcVersion(savedRctcVersion);
  }

  @Test
  public void processResourceBundle_withTheErsdFile_loadsEveryValueSetOfTheBundle() {
    newProcessor(ERSD_EXAMPLE_FILE).processResourceBundle();

    ValueSetSingleton singleton = ValueSetSingleton.getInstance();
    assertFalse(singleton.getValueSets().isEmpty());
    assertEquals(
        "every value set of the eRSD bundle is either a grouper or a regular/emergent set",
        34,
        singleton.getValueSets().size() + singleton.getGrouperValueSets().size());
    assertTrue(singleton.getValueSets().containsAll(singleton.getEmergentValueSets()));
  }

  @Test
  public void processResourceBundle_withASearchsetWrappingTheRctcBundle_usesTheInnerBundle()
      throws Exception {
    Bundle inner = TestUtils.loadBundleFromFile(ERSD_EXAMPLE);
    inner.setId("rctc-release-2022-03-29-Bundle-rctc");
    Bundle searchset = new Bundle();
    searchset.setType(Bundle.BundleType.SEARCHSET);
    searchset.addEntry().setResource(inner);
    File wrapped = File.createTempFile("ersd-searchset-", ".json");
    wrapped.deleteOnExit();
    FileUtils.writeStringToFile(
        wrapped, parser.encodeResourceToString(searchset), StandardCharsets.UTF_8);

    newProcessor(wrapped.getPath()).processResourceBundle();

    ValueSetSingleton singleton = ValueSetSingleton.getInstance();
    assertEquals(34, singleton.getValueSets().size() + singleton.getGrouperValueSets().size());
  }

  @Test
  public void processResourceBundle_withAMissingFile_leavesTheValueSetsUntouched() {
    Set<ValueSet> existing = new HashSet<>();
    existing.add(new ValueSet());
    ValueSetSingleton.getInstance().setValueSets(existing);

    newProcessor("src/test/resources/missing.json").processResourceBundle();

    assertEquals(existing, ValueSetSingleton.getInstance().getValueSets());
    assertEquals(1, ValueSetSingleton.getInstance().getValueSets().size());
  }
}
