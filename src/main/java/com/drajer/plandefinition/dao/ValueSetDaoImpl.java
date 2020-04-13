package com.drajer.plandefinition.dao;

import java.util.List;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.drajer.plandefinition.model.ValueSetGrouperModel;
import com.drajer.plandefinition.model.ValueSetModel;
import com.drajer.plandefinition.model.planDefinitionActionModel;

@Repository("valueSetDaoImpl")
public class ValueSetDaoImpl extends AbstractDao implements ValueSetDao {

	@Autowired
	private SessionFactory sessionFactory;

	@Override
	public ValueSetModel getValueSetById(int id) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<ValueSetModel> getAllValuesets() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void createValueset(ValueSetModel valueSetModel) {
		Session session = sessionFactory.openSession();
		session.beginTransaction();
		session.save(valueSetModel);
		session.getTransaction().commit();
		session.close();
	}

	@Override
	public void createValuesetGrouper(ValueSetGrouperModel valuesetGrouper) {
		Session session = sessionFactory.openSession();
		session.beginTransaction();
		session.save(valuesetGrouper);
		session.getTransaction().commit();
		session.close();
		
	}

	@Override
	public void createPlanDefinitionActions(planDefinitionActionModel planDefinitionAction) {
		Session session = sessionFactory.openSession();
		session.beginTransaction();
		session.save(planDefinitionAction);
		session.getTransaction().commit();
		session.close();
		
	}

}
