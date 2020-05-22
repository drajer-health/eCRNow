package com.drajer.ecrapp.service;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import javax.annotation.PostConstruct;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DataRequirement.DataRequirementCodeFilterComponent;
import org.hl7.fhir.r4.model.Enumerations.FHIRAllTypes;
import org.hl7.fhir.r4.model.PlanDefinition;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionComponent;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionConditionComponent;
import org.hl7.fhir.r4.model.PlanDefinition.PlanDefinitionActionRelatedActionComponent;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.Timing;
import org.hl7.fhir.r4.model.Timing.TimingRepeatComponent;
import org.hl7.fhir.r4.model.TriggerDefinition;
import org.hl7.fhir.r4.model.TriggerDefinition.TriggerType;
import org.hl7.fhir.r4.model.UsageContext;
import org.hl7.fhir.r4.model.ValueSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.drajer.eca.model.AbstractAction;
import com.drajer.eca.model.ActionData;
import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.CQLExpressionCondition;
import com.drajer.eca.model.CloseOutEicrAction;
import com.drajer.eca.model.CreateEicrAction;
import com.drajer.eca.model.EventTypes;
import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.MatchTriggerAction;
import com.drajer.eca.model.PeriodicUpdateEicrAction;
import com.drajer.eca.model.RelatedAction;
import com.drajer.eca.model.SubmitEicrAction;
import com.drajer.eca.model.TimingSchedule;
import com.drajer.eca.model.ValidateEicrAction;
import com.drajer.ecrapp.config.ValueSetSingleton;
import com.drajer.ersd.service.ValueSetService;

import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;

@Service
public class PlanDefinitionProcessor {

	@Autowired
	@Qualifier("esrdGenericClient")
	private IGenericClient esrdClient;

	@Autowired
	@Qualifier("jsonParser")
	IParser jsonParser;

	@Autowired
	@Qualifier("valueSetServiceImpl")
	ValueSetService valueSetService;

	@Value("${ersd.file.location}")
	String ersdFileLocation;

	@Value("${ersd.covid19}")
	Boolean covid;
	
	private final Logger logger = LoggerFactory.getLogger(PlanDefinitionProcessor.class);

	@PostConstruct
	public void initializeClientMethods() {
		processResourceBundle();
	}

	public void processResourceBundle() {

		// Reading Bundle with Id 506 from ersd server.
		// Bundle esrdBundle =
		// esrdClient.read().resource(Bundle.class).withId("506").execute();

		Bundle esrdBundle = readErsdBundleFromFile();

		if (esrdBundle != null) {
			
			List<BundleEntryComponent> bundleEntries = esrdBundle.getEntry();
			
			ValueSet valueSet = null;
			List<UsageContext> usageContextList;
			PlanDefinition planDefinition = null;
			List<PlanDefinitionActionComponent> actions = null;
			List<TriggerDefinition> triggerDefinitionsList = null;
			Set<ValueSet> covidValuesets = new HashSet<>();
			Set<ValueSet> valuesets = new HashSet<>();
			Set<ValueSet> grouperValueSets = new HashSet<>();
			Map<EventTypes.EcrActionTypes, Set<AbstractAction> > acts = new HashMap<EventTypes.EcrActionTypes, Set<AbstractAction> >();

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
							grouperValueSets.add(valueSet);
						}
					}
				}
			}
			
			ValueSetSingleton.getInstance().setCovidValueSets(covidValuesets);
			ValueSetSingleton.getInstance().setValueSets(valuesets);
			ValueSetSingleton.getInstance().setGrouperValueSets(grouperValueSets);
			
			for (BundleEntryComponent bundleEntry : bundleEntries) {
				
				if (Optional.ofNullable(bundleEntry).isPresent()) {
					
					if (bundleEntry.getResource().getResourceType().equals(ResourceType.PlanDefinition)) {
						planDefinition = (PlanDefinition) bundleEntry.getResource();
						actions = planDefinition.getAction();
											
						if (actions != null && actions.size() > 0) {
							
							for (PlanDefinitionActionComponent action : actions) {
								
								if (action.getId().equals("match-trigger")) {
									
									logger.info(" Identified Match Trigger EICR Action ");
									
									MatchTriggerAction mta = new MatchTriggerAction();
									
									populateActionData(mta, acts, action, EcrActionTypes.MATCH_TRIGGER);
										
									triggerDefinitionsList = action.getTrigger();
									
									if (triggerDefinitionsList != null && triggerDefinitionsList.size() > 0) {
										
										for (TriggerDefinition triggerDefinition : triggerDefinitionsList) {
											
											valueSetService.createPlanDefinitionAction(triggerDefinition);
										}
									}
									
								}
								else if(action.getId().equals("create-eicr")) {
									
									logger.info(" Identified Create EICR Action ");
									
									CreateEicrAction mta = new CreateEicrAction();
									
									populateActionData(mta, acts, action, EcrActionTypes.CREATE_EICR);
									
								}
								else if(action.getId().equals("periodic-update-eicr")) {
									
									logger.info(" Identified Periodic Update EICR Action ");
									
									PeriodicUpdateEicrAction mta = new PeriodicUpdateEicrAction();
									
									populateActionData(mta, acts, action, EcrActionTypes.PERIODIC_UPDATE_EICR);
									
								}
								else if(action.getId().equals("close-out-eicr")) {
									
									logger.info(" Identified Close Out EICR Action ");
									
									CloseOutEicrAction mta = new CloseOutEicrAction();
									
									populateActionData(mta, acts, action, EcrActionTypes.CLOSE_OUT_EICR);
									
	
								}
								else if(action.getId().equals("validate-eicr")) {
									
									logger.info(" Identified Validate EICR Action ");
									
									ValidateEicrAction mta = new ValidateEicrAction();
									
									populateActionData(mta, acts, action, EcrActionTypes.VALIDATE_EICR);
									
									
								}
								else if(action.getId().equals("route-and-send-eicr")) {
									
									logger.info(" Identified Submit EICR Action ");
									
									SubmitEicrAction mta = new SubmitEicrAction();
									
									populateActionData(mta, acts, action, EcrActionTypes.SUBMIT_EICR);
									
								}
							}
						}
					}
				}
			}
			
			if(acts != null) {
				ActionRepo.getInstance().setActions(acts);
				
				ActionRepo.getInstance().setupTriggerBasedActions();
				
				ActionRepo.getInstance().print();
				
			}
		}
	}

	private Bundle readErsdBundleFromFile() {
		
		InputStream in = null;
		Bundle bundle = null;
		try {
			in = new FileInputStream(new File(ersdFileLocation));
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
	
	private void populateActionData(AbstractAction act, Map<EventTypes.EcrActionTypes, Set<AbstractAction> > acts, PlanDefinitionActionComponent action, EventTypes.EcrActionTypes type) {
		
		act.setActionId(action.getId());
		
		if(action.hasTrigger())
			processTriggerDefinitions(action.getTrigger(), act, acts);
		
		if(action.hasCondition())
			processConditions(action.getCondition(), act, acts);
		
		if(action.hasRelatedAction())
			processRelatedActions(action.getRelatedAction(), act, acts);
		
		if(action.hasTimingTiming()) {
			TimingSchedule ts = getTimingSchedule(action.getTimingTiming(), TriggerType.DATACHANGED);
			if(ts != null)
				act.addTimingData(ts);
		}
		
		if(acts.containsKey(type)) {
			
			acts.get(type).add(act);
			
			logger.info(" Map contained " + type.toString() + ", so added to map resulting in size " + acts.size());
		}
		else {
			Set<AbstractAction> aa = new HashSet<AbstractAction>();
			aa.add(act);
			acts.put(type, aa);
			
			logger.info(" Map did not contain " + type.toString() + ", so added to map resulting in size " + acts.size());
		}
	}
	
	private void processTriggerDefinitions(List<TriggerDefinition> tdlist, AbstractAction act, Map<EventTypes.EcrActionTypes, Set<AbstractAction> > acts) {
		
		if (tdlist != null && tdlist.size() > 0) {
			
			for (TriggerDefinition triggerDefinition : tdlist) {
				
				if(triggerDefinition.getType() != TriggerType.NAMEDEVENT && 
				   triggerDefinition.getType() != TriggerType.PERIODIC && 
				   triggerDefinition.hasData() ) {
					
					logger.info(" Identified Data Trigger for Act " + act.getActionId());

					List<DataRequirement> dr = triggerDefinition.getData();
					
					for(DataRequirement d: dr) {
						
						// Create ActionData object
						ActionData ad = new ActionData();
						
						ad.setTriggerType(triggerDefinition.getType());
						
						ad.setFhirDataType(FHIRAllTypes.valueOf(d.getType().toUpperCase()));
						
						if(d.hasProfile())
							ad.setProfiles(d.getProfile());
						
						if(d.hasCodeFilter()) {
							
							DataRequirementCodeFilterComponent cf =  d.getCodeFilterFirstRep();
							
							if(cf.hasPath())
								ad.setPath(cf.getPath());
							
							if(cf.hasValueSet())
								ad.setValueSet(cf.getValueSetElement());
							
						}
						
						act.addActionData(ad);
													
					}
					
				}
				else if(triggerDefinition.getType() == TriggerType.PERIODIC) {
					
					
					if(triggerDefinition.hasTimingTiming()) {
						
						Timing t = triggerDefinition.getTimingTiming();
						
						if(t.hasRepeat()) {
							
							TimingSchedule ts = getTimingSchedule(t,triggerDefinition.getType());
							
							if(ts != null) {
								act.addTimingData(ts);
								
							}
							
						}
						
					}
					else {
						
						// Not handling the others for eCR
					}
					
					
				}
				else {
					
					// Ignore other types for eCR
				}
			}
		}
		
	}
	
	private TimingSchedule getTimingSchedule(Timing t, TriggerType type) {
		
		if(t != null && t.hasRepeat()) {
			
			TimingRepeatComponent rc = t.getRepeat();
			
			// Create Timing Data 
			TimingSchedule ts = new TimingSchedule();
			
			ts.setTriggerType(type);
			
			ts.setNumOfRepeat(rc.getCount());
			ts.setMaxRepeat(rc.getCountMax());
			ts.setFrequency(rc.getFrequency());
			ts.setFrequencyMax(rc.getFrequencyMax());
			ts.setFrequencyPeriod(rc.getPeriod());
			ts.setFrequencyPeriodUnit(rc.getPeriodUnitElement().getValue());
			
			return ts;
			
		}
		
		return null;
		
	}
	
	private void processConditions(List<PlanDefinitionActionConditionComponent> condlist, AbstractAction act, Map<EventTypes.EcrActionTypes, Set<AbstractAction> > acts) {
		
		if (condlist != null && condlist.size() > 0) {
			
			for (PlanDefinitionActionConditionComponent cond : condlist) {
								
				if(cond.hasKind() && cond.hasExpression()) {
					
					CQLExpressionCondition cd = new CQLExpressionCondition();
					cd.setConditionType(cond.getKind());
					cd.setExpression(cond.getExpression().getExpression());
					
					act.addCondition(cd);					
					
					
				}
			}			
					
		}
	}

	private void processRelatedActions(List<PlanDefinitionActionRelatedActionComponent> rdlist, AbstractAction act, Map<EventTypes.EcrActionTypes, Set<AbstractAction> > acts) {
		
		if (rdlist != null && rdlist.size() > 0) {
			
			for (PlanDefinitionActionRelatedActionComponent rc : rdlist) {
				
				RelatedAction ra = new RelatedAction();
				ra.setRelationship(rc.getRelationship());
				
				if(rc.hasOffsetDuration())
					ra.setDuration(rc.getOffsetDuration());
				
				AbstractAction a = getActionById(rc.getActionId(), acts);
				
				if(a != null) {					
					ra.setRelatedAction(a);
				}
				
				act.addRelatedAction(ra);
				
				
			}			
					
		}
	}
	
	private AbstractAction getActionById(String actId, Map<EventTypes.EcrActionTypes, Set<AbstractAction> > acts) {
		
		if(acts != null && acts.size() > 0) {
			
			for(Map.Entry<EventTypes.EcrActionTypes, Set<AbstractAction> > ent : acts.entrySet() ) {
				
				Set<AbstractAction> aa = ent.getValue();
				
				if(aa != null && aa.size() > 0) {
					
					for(AbstractAction a : aa) {
						
						if(a.getActionId().equalsIgnoreCase(actId)) 
							return a;
					}
					
				}				
				
			}
		}
		
		return null;
		
	}
}
