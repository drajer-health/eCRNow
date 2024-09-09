package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.TimeZoneDao;
import com.drajer.ecrapp.dao.AbstractDao;
import javax.transaction.Transactional;
import org.hibernate.query.NativeQuery;
import org.springframework.stereotype.Repository;

@Repository
@Transactional
public class TimeZoneDaoImpl extends AbstractDao implements TimeZoneDao {

  @Override
  public String getDatabaseTimezone() {

    NativeQuery<String> query =
        getSession().createNativeQuery("SELECT current_setting('timezone')");
    Object singleResult = query.getSingleResult();
    return singleResult.toString();
  }

  @Override
  public void setDatabaseTimezone(String tz) {}
}
