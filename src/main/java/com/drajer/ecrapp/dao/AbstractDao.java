package com.drajer.ecrapp.dao;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class AbstractDao {

  @Autowired private SessionFactory sessionFactory;

  protected Session getSession() {
    return sessionFactory.getCurrentSession();
  }

  public void persist(Object entity) {
    getSession().persist(entity);
  }

  public void delete(Object entity) {
    remove(entity);
  }

  /**
   * Replacement for the Session.saveOrUpdate() that was removed in Hibernate 6/7.
   *
   * <p>All entities here use a generated identifier, so a null id means the entity is still
   * transient and has to be persisted. persist() also populates the generated id on the instance
   * handed in by the caller, which merge() would not do. An entity that already has an id is
   * merged.
   *
   * @param entity the entity to insert or update.
   * @param id the identifier of the entity, null when it has not been persisted yet.
   * @return the managed entity.
   */
  protected <T> T persistOrMerge(T entity, Object id) {

    Session session = getSession();

    if (id == null) {
      session.persist(entity);
      return entity;
    }

    return session.merge(entity);
  }

  /**
   * Replacement for the Session.delete() that was removed in Hibernate 6/7. Session.remove() only
   * accepts a managed instance, and these entities are usually read in an earlier transaction, so
   * the entity is merged back into the current session before it is removed.
   *
   * @param entity the entity to delete.
   */
  protected void remove(Object entity) {

    Session session = getSession();
    session.remove(session.merge(entity));
  }
}
