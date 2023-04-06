package com.drajer.ecrapp.dao;

import com.drajer.bsa.model.PublicHealthMessage;
import java.util.List;
import java.util.Map;

public interface PhMessageDao {

  PublicHealthMessage saveOrUpdate(PublicHealthMessage publicHealthMessage);

  List<PublicHealthMessage> getPhMessageData(Map<String, String> searchParams);
}
