package com.drajer.plandefinition.dao;

import java.util.List;

import com.drajer.plandefinition.model.ValueSetGrouperModel;
import com.drajer.plandefinition.model.ValueSetModel;
import com.drajer.plandefinition.model.planDefinitionActionModel;

public interface ValueSetDao {
	
	public ValueSetModel getValueSetById(int id);
	 
	 public List<ValueSetModel> getAllValuesets();
	 
	 public void createValueset(ValueSetModel valueset);
	 
	 public void createValuesetGrouper(ValueSetGrouperModel valuesetGrouper);
	 
	 public void createPlanDefinitionActions(planDefinitionActionModel planDefinitionAction);

}
