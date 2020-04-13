package com.drajer.plandefinition.service;

import java.util.List;

import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DataRequirement.DataRequirementCodeFilterComponent;
import org.hl7.fhir.r4.model.TriggerDefinition;
import org.hl7.fhir.r4.model.ValueSet;
import org.hl7.fhir.r4.model.ValueSet.ConceptSetComponent;
import org.hl7.fhir.r4.model.ValueSet.ValueSetComposeComponent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import com.drajer.plandefinition.dao.ValueSetDao;
import com.drajer.plandefinition.model.ValueSetGrouperModel;
import com.drajer.plandefinition.model.ValueSetModel;
import com.drajer.plandefinition.model.planDefinitionActionModel;

import ca.uhn.fhir.parser.IParser;

@Service("valueSetServiceImpl")
public class ValueSetServiceImpl implements ValueSetService {
	
	@Autowired
	@Qualifier("jsonParser")
	IParser jsonParser;
	
	@Autowired
	@Qualifier("valueSetDaoImpl")
	ValueSetDao valueSetDao;
	
	@Override
	public void createValueSet(ValueSet valueSet) {
		ValueSetModel valueSetModel = new ValueSetModel();
		valueSetModel.setValueSetId(valueSet.getId());
		valueSetModel.setData(jsonParser.encodeResourceToString(valueSet));
		
		valueSetDao.createValueset(valueSetModel);
	}

	@Override
	public void createValueSetGrouper(ValueSet valueSet) {
		
		List<CanonicalType> valuesetList= null;
		ValueSetGrouperModel valueSetGrouperModel = null;
		ValueSetComposeComponent valueSetComposeComponent = valueSet.getCompose();
		List<ConceptSetComponent> conceptSetComponentList = valueSetComposeComponent.getInclude();
		for(ConceptSetComponent conceptSetComponent : conceptSetComponentList) {
			valuesetList = conceptSetComponent.getValueSet();
			for (CanonicalType canonicalType : valuesetList) {
				valueSetGrouperModel = new ValueSetGrouperModel();
				valueSetGrouperModel.setValueSetGrouper(valueSet.getUrl());
				valueSetGrouperModel.setValueSetId(canonicalType.getValueAsString());
				valueSetDao.createValuesetGrouper(valueSetGrouperModel);
			}
		}
	
	}

	@Override
	public void createPlanDefinitionAction(TriggerDefinition triggerDefinition) {
		planDefinitionActionModel model = new planDefinitionActionModel();
		DataRequirement dataRequirement = triggerDefinition.getDataFirstRep();
		DataRequirementCodeFilterComponent codeFilter = dataRequirement.getCodeFilterFirstRep();
		
		model.setActionId("match-trigger");
		model.setActionTriggerType(triggerDefinition.getType().toCode());
		model.setActionElementType(dataRequirement.getType());
		model.setActionPath(codeFilter.getPath());
		model.setActionValueSetGrouper(codeFilter.getValueSet());

		valueSetDao.createPlanDefinitionActions(model);
	}
}
