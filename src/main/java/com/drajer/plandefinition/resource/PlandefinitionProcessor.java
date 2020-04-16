package com.drajer.plandefinition.resource;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import javax.annotation.PostConstruct;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.PlanDefinition;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionComponent;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.TriggerDefinition;
import org.hl7.fhir.r4.model.UsageContext;
import org.hl7.fhir.r4.model.ValueSet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.drajer.plandefinition.config.ValueSetSingleton;
import com.drajer.plandefinition.service.ValueSetService;

import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;

@Service
public class PlandefinitionProcessor {

	@Autowired
	@Qualifier("esrdGenericClient")
	private IGenericClient esrdClient;

	@Autowired
	@Qualifier("jsonParser")
	IParser jsonParser;

	@Autowired
	@Qualifier("valueSetServiceImpl")
	ValueSetService valueSetService;

	@Value("${esrd.file.location}")
	String esrdFileLocation;

	@Value("${esrd.covid19}")
	Boolean covid;

	@PostConstruct
	public void initializeClientMethods() {
		processResourceBundle();
	}

	public void processResourceBundle() {

		// Reading Bundle with Id 506 from ersd server.
		// Bundle esrdBundle =
		// esrdClient.read().resource(Bundle.class).withId("506").execute();

		Bundle esrdBundle = readEsrdBundleFromFile();

		if (esrdBundle != null) {
			List<BundleEntryComponent> bundleEntries = esrdBundle.getEntry();
			ValueSet valueSet = null;
			List<UsageContext> usageContextList;
			PlanDefinition planDefinition = null;
			List<PlanDefinitionActionComponent> actions = null;
			List<TriggerDefinition> triggerDefinitionsList = null;
			Set<ValueSet> covidValuesets = new HashSet<>();
			Set<ValueSet> valuesets = new HashSet<>();

			for (BundleEntryComponent bundleEntry : bundleEntries) {
				if (Optional.ofNullable(bundleEntry).isPresent()) {
					if (bundleEntry.getResource().getResourceType().equals(ResourceType.ValueSet)) {
						valueSet = (ValueSet) bundleEntry.getResource();
						usageContextList = valueSet.getUseContext();
						if (!usageContextList.isEmpty()) {
								for (UsageContext usageContext : usageContextList) {
									if (Optional.ofNullable(usageContext).isPresent()) {
										if(covid) {
											if (usageContext.getValueCodeableConcept() != null && usageContext
													.getValueCodeableConcept().getText().equalsIgnoreCase("COVID-19")) {
												System.out.println("Processing value set with id : " + valueSet.getId());
												valueSetService.createValueSet(valueSet);
												covidValuesets.add(valueSet);
											}
										}else {
											valueSetService.createValueSet(valueSet);
											if (usageContext.getValueCodeableConcept() != null && usageContext
													.getValueCodeableConcept().getText().equalsIgnoreCase("COVID-19")) {
												covidValuesets.add(valueSet);
											}else {
												valuesets.add(valueSet);
											}
											
										}
									}
								}
						} else {
							valueSetService.createValueSetGrouper(valueSet);
						}
					} else if (bundleEntry.getResource().getResourceType().equals(ResourceType.PlanDefinition)) {
						planDefinition = (PlanDefinition) bundleEntry.getResource();
						actions = planDefinition.getAction();
						if (actions != null && actions.size() > 0) {
							for (PlanDefinitionActionComponent action : actions) {
								if (action.getId().equals("match-trigger")) {
									triggerDefinitionsList = action.getTrigger();
									if (triggerDefinitionsList != null && triggerDefinitionsList.size() > 0) {
										for (TriggerDefinition triggerDefinition : triggerDefinitionsList) {
											valueSetService.createPlanDefinitionAction(triggerDefinition);
										}
									}
								}
							}
						}
					}
				}
			}
			ValueSetSingleton.getInstance().setCovidValueSets(covidValuesets);
			ValueSetSingleton.getInstance().setValueSets(valuesets);
		}
	}

	private Bundle readEsrdBundleFromFile() {
		InputStream in = null;
		Bundle bundle = null;
		try {
			in = new FileInputStream(new File(esrdFileLocation));
			if (in != null) {
				bundle = jsonParser.parseResource(Bundle.class, in);
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (in != null) {
				try {
					in.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		return bundle;
	}
}
