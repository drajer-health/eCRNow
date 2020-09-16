package com.drajer.ersd.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ersd.dao.ValueSetDao;
import com.drajer.ersd.model.PlanDefinitionActionModel;
import com.drajer.ersd.model.ValueSetGrouperModel;
import com.drajer.ersd.model.ValueSetModel;
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
    //	session.save(valueSetModel);
    session.getTransaction().commit();
    session.close();
  }

  @Override
  public void createValuesetGrouper(ValueSetGrouperModel valuesetGrouper) {
    Session session = sessionFactory.openSession();
    session.beginTransaction();
    //	session.save(valuesetGrouper);
    session.getTransaction().commit();
    session.close();
  }

  @Override
  public void createPlanDefinitionActions(PlanDefinitionActionModel planDefinitionAction) {
    Session session = sessionFactory.openSession();
    session.beginTransaction();
    //	session.save(planDefinitionAction);
    session.getTransaction().commit();
    session.close();
  }
}
