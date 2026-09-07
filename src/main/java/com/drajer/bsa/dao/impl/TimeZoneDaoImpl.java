package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.TimeZoneDao;
import com.drajer.ecrapp.dao.AbstractDao;
import jakarta.transaction.Transactional;
import org.hibernate.SessionFactory;
import org.hibernate.query.NativeQuery;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
@Transactional
public class TimeZoneDaoImpl extends AbstractDao implements TimeZoneDao {

  /**
   * Instantiates a new timezone DAO implementation.
   *
   * @param sessionFactory the Hibernate session factory
   */
  @Autowired
  public TimeZoneDaoImpl(SessionFactory sessionFactory) {
    super(sessionFactory);
  }

  @Override
  public String getDatabaseTimezone(String query) {

    NativeQuery<String> nativequery = getSession().createNativeQuery(query);
    Object singleResult = nativequery.getSingleResult();
    return singleResult.toString();
  }

  @Override
  public void setDatabaseTimezone(String timeZone) {
    String query = "SET timezone = :timeZone";

    getSession().createNativeQuery(query).setParameter("timeZone", timeZone).executeUpdate();
  }
}
