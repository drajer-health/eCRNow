package com.drajer.ersd;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.parser.IParser;
import com.drajer.ecrapp.config.ValueSetSingleton;
import com.drajer.ersd.dao.ValueSetDao;
import com.drajer.ersd.model.ValueSetGrouperModel;
import com.drajer.ersd.model.ValueSetModel;
import com.drajer.ersd.service.ValueSetService;
import com.drajer.ersd.service.impl.ValueSetServiceImpl;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.TriggerDefinition;
import org.hl7.fhir.r4.model.ValueSet;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class ValueSetServiceImplTest {

  @Mock private ValueSetService valueSetService;

  @Mock private IParser jsonParser;

  @Mock private ValueSetDao valueSetDao;

  @InjectMocks private ValueSetServiceImpl valueSetServiceImpl;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void createValueSet_Test() {
    ValueSet valueSet = new ValueSet();
    valueSet.setId("1");
    valueSet.setName("valueset123");
    when(jsonParser.encodeResourceToString(valueSet)).thenReturn("Mock Data");
    valueSetServiceImpl.createValueSet(valueSet);

    ArgumentCaptor<ValueSetModel> captor = ArgumentCaptor.forClass(ValueSetModel.class);
    verify(valueSetDao).createValueset(captor.capture());
    ValueSetModel capturedModel = captor.getValue();
    assertEquals("1", capturedModel.getValueSetId());
    assertEquals("Mock Data", capturedModel.getData());
  }

  @Test
  public void createValueSetGrouper_Test() {
    ValueSet valueSet = new ValueSet();
    valueSet.setUrl("http://mockurl");
    ValueSet.ValueSetComposeComponent valueSetComposeComponent =
        new ValueSet.ValueSetComposeComponent();
    valueSetComposeComponent.setId("cc1");
    ValueSet.ConceptSetComponent conceptSetComponent = new ValueSet.ConceptSetComponent();
    conceptSetComponent.setId("csc1");
    CanonicalType canonicalType = new CanonicalType();
    canonicalType.setValue("mockCanonical");
    canonicalType.setId("canonical1");
    List<CanonicalType> canonicalTypes = new ArrayList<>();
    canonicalTypes.add(canonicalType);
    conceptSetComponent.setValueSet(canonicalTypes);
    List<ValueSet.ConceptSetComponent> list = new ArrayList<>();
    list.add(conceptSetComponent);
    valueSetComposeComponent.setInclude(list);
    valueSet.setCompose(valueSetComposeComponent);

    valueSetServiceImpl.createValueSetGrouper(valueSet);

    ArgumentCaptor<ValueSetGrouperModel> argumentCaptor =
        ArgumentCaptor.forClass(ValueSetGrouperModel.class);

    verify(valueSetDao).createValuesetGrouper(argumentCaptor.capture());
    assertEquals("http://mockurl", valueSet.getUrl());
    assertEquals("cc1", valueSetComposeComponent.getId());
    assertEquals("mockCanonical", canonicalType.getValue());
  }

  @Test
  public void createPlanDefinitionAction_Test_with_ValueSetListFromGrouper() {
    TriggerDefinition triggerDefinition = new TriggerDefinition();
    triggerDefinition.setId("1");
    DataRequirement dataRequirement = new DataRequirement();
    dataRequirement.setId("DR1");
    dataRequirement.setType("DataTyprValue");

    List<DataRequirement> dataRequirementList = new ArrayList<>();
    dataRequirementList.add(dataRequirement);
    triggerDefinition.setData(dataRequirementList);
    DataRequirement.DataRequirementCodeFilterComponent dataRequirementCodeFilterComponent =
        new DataRequirement.DataRequirementCodeFilterComponent();
    dataRequirementCodeFilterComponent.setValueSet("mockvalue");
    dataRequirementCodeFilterComponent.setPath("mockpath");
    List<DataRequirement.DataRequirementCodeFilterComponent> list = new ArrayList<>();
    list.add(dataRequirementCodeFilterComponent);
    dataRequirement.setCodeFilter(list);

    ValueSet valueSet = new ValueSet();
    valueSet.setId("mockId");
    valueSet.setUrl("http://example.com/valueset");

    Set<ValueSet> grouperValueSet = new HashSet<>();
    grouperValueSet.add(valueSet);
    ValueSetSingleton.getInstance().setGrouperValueSets(grouperValueSet);

    Set<ValueSet> vs = new HashSet<>();
    vs.add(valueSet);
    ValueSetSingleton.getInstance().setValueSets(vs);

    ValueSetSingleton.getInstance().setEmergentValueSets(vs);

    valueSetServiceImpl.createPlanDefinitionAction(triggerDefinition);

    assertEquals("mockId", valueSet.getId());
    assertEquals("DR1", dataRequirement.getId());
    assertEquals("1", triggerDefinition.getId());
    assertEquals("DataTyprValue", dataRequirement.getType());
  }
}
