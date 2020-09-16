package com.drajer.ersd.dao;

import com.drajer.ersd.model.PlanDefinitionActionModel;
import com.drajer.ersd.model.ValueSetGrouperModel;
import com.drajer.ersd.model.ValueSetModel;
import java.util.List;

public interface ValueSetDao {

  public ValueSetModel getValueSetById(int id);

  public List<ValueSetModel> getAllValuesets();

  public void createValueset(ValueSetModel valueset);

  public void createValuesetGrouper(ValueSetGrouperModel valuesetGrouper);

  public void createPlanDefinitionActions(PlanDefinitionActionModel planDefinitionAction);
}
