package com.drajer.plandefinition.dao;

import java.util.List;

import com.drajer.plandefinition.model.ValueSetModel;

public interface ValueSetDao {
	
	public ValueSetModel getValueSetById(int id);
	 
	 public List<ValueSetModel> getAllValuesets();
	 
	 public void createValueset(ValueSetModel valueset);

}
