package com.drajer.eca.model;

import java.util.ArrayList;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.hl7.fhir.r4.model.TriggerDefinition.TriggerType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.service.TriggerQueryService;

@Service
public class ActionRepo {
	
	private static ActionRepo instance;
	
	private Map<EcrActionTypes, List<AbstractAction> > actions;
	
	private Map<TriggerType, List<AbstractAction> >	   actionsByTriggers;
	
	TriggerQueryService triggerQueryService;
	
	LoadingQueryService loadingQueryService;
	
	private final Logger logger = LoggerFactory.getLogger(ActionRepo.class);

	public static ActionRepo getInstance() {
		if (instance == null) {
			instance = new ActionRepo();
		}
		return instance;
	}
	
	public Map<TriggerType, List<AbstractAction>> getActionsByTriggers() {
		return actionsByTriggers;
	}



	public void setActionsByTriggers(Map<TriggerType, List<AbstractAction>> actionsByTriggers) {
		this.actionsByTriggers = actionsByTriggers;
	}



	public TriggerQueryService getTriggerQueryService() {
		return triggerQueryService;
	}



	public void setTriggerQueryService(TriggerQueryService triggerQueryService) {
		this.triggerQueryService = triggerQueryService;
	}



	public LoadingQueryService getLoadingQueryService() {
		return loadingQueryService;
	}



	public void setLoadingQueryService(LoadingQueryService loadingQueryService) {
		this.loadingQueryService = loadingQueryService;
	}



	public Map<EcrActionTypes, List<AbstractAction>> getActions() {
		return actions;
	}



	public void setActions(Map<EcrActionTypes, List<AbstractAction>> actions) {
		this.actions = actions;
		
	}
	
	public void setupTriggerBasedActions() {
		
		if(actions != null) {
			
			for(Map.Entry<EcrActionTypes, List<AbstractAction> > ent : actions.entrySet()) {
				
				List<AbstractAction> aa = ent.getValue();
				
				if(aa != null) {
					
					for(AbstractAction a : aa) {
						
						// if Trigger is populated then we can add it.
						List<ActionData> td = a.getTriggerData();
						
						if(td != null && td.size() > 0) {
							
							if(actionsByTriggers == null)
								actionsByTriggers = new HashMap<TriggerType, List<AbstractAction> >();
								
							for(ActionData ad : td) {
								
								if(actionsByTriggers.containsKey(ad.getTriggerType())) {
									
									actionsByTriggers.get(ad.getTriggerType()).add(a);
									
								}
								else {
									List<AbstractAction> la = new ArrayList<AbstractAction>();
									la.add(a);
									
									actionsByTriggers.put(ad.getTriggerType(), la);
																		
								}
								
								
							}
							
						}			
						
						// Add for Timing data
						// if Trigger is populated then we can add it.
						List<TimingSchedule> ts = a.getTimingData();
						
						if(ts != null && ts.size() > 0) {
							
							if(actionsByTriggers == null)
								actionsByTriggers = new HashMap<TriggerType, List<AbstractAction> >();
								
							for(TimingSchedule tsd : ts) {
								
								if(actionsByTriggers.containsKey(tsd.getTriggerType())) {
									
									actionsByTriggers.get(tsd.getTriggerType()).add(a);
									
								}
								else {
									List<AbstractAction> la = new ArrayList<AbstractAction>();
									la.add(a);
									
									actionsByTriggers.put(tsd.getTriggerType(), la);
																		
								}
								
								
							}
							
						}	
					}
				}				
			}
			
		}
	}
	



	private ActionRepo() {
	}
	
	public void print() {
		
		logger.info(" ***** Printing ACTION Repository ***** " + "\n" );
		
		logger.info(" *************** Printing EicrTypes Repository **************** " + "\n" );
		
		if(actions != null) {
			
			for(Map.Entry<EcrActionTypes, List<AbstractAction> > ent : actions.entrySet()) {
				
				List<AbstractAction> aa = ent.getValue();
				
				if(aa != null) {
					
					for(AbstractAction a : aa) {
						a.print();
					}
				}	
			}
		}
		
		logger.info(" *************** End Printing EicrTypes Repository **************** " + "\n" );
		
		logger.info(" *************** Start Printing Trigger Types Repository **************** " + "\n" );
		
		if(actionsByTriggers != null) {
			
			for(Map.Entry<TriggerType, List<AbstractAction> > ent : actionsByTriggers.entrySet()) {
				
				List<AbstractAction> aa = ent.getValue();
				
				if(aa != null) {
					
					for(AbstractAction a : aa) {
						a.print();
					}
				}
			}
		}
		
		logger.info(" *************** End Printing Trigger Types Repository **************** " + "\n" );
		
		logger.info(" ***** End Printing ACTION Repository ***** " + "\n" );
		
	}
}
