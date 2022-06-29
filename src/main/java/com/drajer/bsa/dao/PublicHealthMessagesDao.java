package com.drajer.bsa.dao;

import com.drajer.bsa.model.PublicHealthMessage;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public interface PublicHealthMessagesDao {

  PublicHealthMessage saveOrUpdate(PublicHealthMessage message);

  PublicHealthMessage getById(UUID id);

  Integer getMaxVersionId(PublicHealthMessage message);

  PublicHealthMessage getByCorrelationId(String coorelId);

  PublicHealthMessage getBySubmittedDataId(String subId);

  List<PublicHealthMessage> getPublicHealthMessage(Map<String, String> searchParams);

  List<PublicHealthMessage> getByXRequestId(String xRequestId);

  PublicHealthMessage getBySubmittedMessageId(String messageId);

  PublicHealthMessage getByResponseMessageId(String id);

  void delete(PublicHealthMessage message);
}
