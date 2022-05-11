package com.drajer.bsa.dao.impl;

import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.ecrapp.dao.AbstractDao;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class PublicHealthMessagesDaoImpl extends AbstractDao implements PublicHealthMessagesDao {

  public static final String SUBMITTED_DATA_ID = "submittedDataId";

  @Override
  public PublicHealthMessage saveOrUpdate(PublicHealthMessage message) {
    getSession().saveOrUpdate(message);
    return message;
  }

  @Override
  public PublicHealthMessage getById(UUID id) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public Integer getMaxVersionId(PublicHealthMessage message) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public PublicHealthMessage getByCorrelationId(String coorelId) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public List<PublicHealthMessage> getPublicHealthMessage(Map<String, String> searchParams) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public List<PublicHealthMessage> getByXRequestId(String xRequestId) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public PublicHealthMessage getBySubmittedMessageId(String messageId) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public PublicHealthMessage getByResponseMessageId(String id) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public void delete(PublicHealthMessage message) {
    // TODO Auto-generated method stub

  }

  @Override
  public PublicHealthMessage getBySubmittedDataId(String subId) {

    Criteria criteria = getSession().createCriteria(PublicHealthMessage.class);
    criteria.add(Restrictions.eq(SUBMITTED_DATA_ID, subId));

    return (PublicHealthMessage) criteria.uniqueResult();
  }
}
