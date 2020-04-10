package com.drajer.plandefinition.dao;

import java.util.List;

import javax.transaction.Transactional;
import javax.transaction.Transactional.TxType;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.drajer.plandefinition.model.ValueSetModel;

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
	@Transactional(value = TxType.REQUIRES_NEW)
	public void createValueset(ValueSetModel valueSetModel) {
		Session session = sessionFactory.openSession();
		session.beginTransaction();
		session.save(valueSetModel);
		session.getTransaction().commit();
	}

}
