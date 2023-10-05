package com.drajer.ecrapp.dao;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.sof.model.PublicHealthMessageData;
import java.util.List;
import java.util.Map;

public interface PhMessageDao {

  List<PublicHealthMessage> getPhMessageData(Map<String, String> searchParams);

  List<PublicHealthMessage> getPhMessageByXRequestIds(List<String> xRequestId);

  List<PublicHealthMessage> getPhMessagesContainingXRequestIds(List<String> xRequestIds);

  List<PublicHealthMessage> getPhMessageByParameters(
      PublicHealthMessageData publicHealthMessageData);

  void delete(PublicHealthMessage message);
}
