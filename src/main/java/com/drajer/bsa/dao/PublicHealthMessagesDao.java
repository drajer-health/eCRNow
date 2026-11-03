package com.drajer.bsa.dao;

import com.drajer.bsa.model.PublicHealthMessage;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public interface PublicHealthMessagesDao {

  PublicHealthMessage saveOrUpdate(PublicHealthMessage message);

  /**
   * Saves the message in its own new transaction, committed immediately and independent of the
   * caller's transaction. Used to record a report right after it has been submitted, so that the
   * record survives even if the rest of the job later fails and rolls back.
   */
  PublicHealthMessage saveInNewTransaction(PublicHealthMessage message);

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
