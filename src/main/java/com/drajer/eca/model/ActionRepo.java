package com.drajer.eca.model;

import java.util.Dictionary;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.eca.model.EventTypes.EcrActionTypes;

public class ActionRepo {
	
	private static ActionRepo instance;
	
	private Map<EcrActionTypes, List<AbstractAction> > actions;
	
	private final Logger logger = LoggerFactory.getLogger(ActionRepo.class);

	public static ActionRepo getInstance() {
		if (instance == null) {
			instance = new ActionRepo();
		}
		return instance;
	}
	
	public Map<EcrActionTypes, List<AbstractAction>> getActions() {
		return actions;
	}



	public void setActions(Map<EcrActionTypes, List<AbstractAction>> actions) {
		this.actions = actions;
	}



	private ActionRepo() {
	}
	
	public void print() {
		
		logger.info(" ***** Printing ACTION Repository ***** ");
		
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
		
		logger.info(" ***** End Printing ACTION Repository ***** ");
		
	}
}
