package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.TimeZoneDao;
import com.drajer.ecrapp.dao.AbstractDao;
import jakarta.transaction.Transactional;
import org.hibernate.query.NativeQuery;
import org.springframework.stereotype.Repository;

@Repository
@Transactional
public class TimeZoneDaoImpl extends AbstractDao implements TimeZoneDao {

  @Override
  public String getDatabaseTimezone(String query) {

    NativeQuery<String> nativequery = getSession().createNativeQuery(query);
    Object singleResult = nativequery.getSingleResult();
    return singleResult.toString();
  }

  @Override
  public void setDatabaseTimezone(String query, String timeZone) {
    String fullQuery = query + " '" + timeZone + "'";
    NativeQuery<?> nativequery = getSession().createNativeQuery(fullQuery);
    nativequery.executeUpdate();
  }
}
