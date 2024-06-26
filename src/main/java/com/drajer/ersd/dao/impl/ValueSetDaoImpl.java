package com.drajer.ersd.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ersd.dao.ValueSetDao;
import com.drajer.ersd.model.PlanDefinitionActionModel;
import com.drajer.ersd.model.ValueSetGrouperModel;
import com.drajer.ersd.model.ValueSetModel;
import java.util.Collections;
import java.util.List;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository("valueSetDaoImpl")
public class ValueSetDaoImpl extends AbstractDao implements ValueSetDao {

  @Autowired private SessionFactory sessionFactory;

  @Override
  public ValueSetModel getValueSetById(int id) {

    return null;
  }

  @Override
  public List<ValueSetModel> getAllValuesets() {

    return Collections.emptyList();
  }

  @Override
  public void createValueset(ValueSetModel valueSetModel) {
    Session session = sessionFactory.openSession();
    session.beginTransaction();

    session.getTransaction().commit();
    session.close();
  }

  @Override
  public void createValuesetGrouper(ValueSetGrouperModel valuesetGrouper) {
    Session session = sessionFactory.openSession();
    session.beginTransaction();
    session.getTransaction().commit();
    session.close();
  }

  @Override
  public void createPlanDefinitionActions(PlanDefinitionActionModel planDefinitionAction) {
    Session session = sessionFactory.openSession();
    session.beginTransaction();
    session.getTransaction().commit();
    session.close();
  }
}
