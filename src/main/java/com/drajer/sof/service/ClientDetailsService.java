package com.drajer.sof.service;

import com.drajer.sof.model.ClientDetails;
import java.util.List;

public interface ClientDetailsService {

  ClientDetails saveOrUpdate(ClientDetails clientDetails);

  ClientDetails getClientDetailsById(Integer id);

  ClientDetails getClientDetailsByUrl(String url);

  List<ClientDetails> getAllClientDetails();
}
