package com.drajer.ecrapp.dao;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class AbstractDao {

  protected final SessionFactory sessionFactory;

  /**
   * Instantiates a new abstract DAO.
   *
   * @param sessionFactory the Hibernate session factory
   */
  @Autowired
  public AbstractDao(SessionFactory sessionFactory) {
    this.sessionFactory = sessionFactory;
  }

  protected Session getSession() {
    return sessionFactory.getCurrentSession();
  }

  public void persist(Object entity) {
    getSession().persist(entity);
  }

  public void delete(Object entity) {
    getSession().delete(entity);
  }
}
