package com.drajer.sof.service.impl;

import com.drajer.sof.dao.ClientDetailsDao;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.service.ClientDetailsService;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class ClientDetailsServiceImpl implements ClientDetailsService {

  @Autowired ClientDetailsDao clientDetailsDao;

  public ClientDetails saveOrUpdate(ClientDetails clientDetails) {
    clientDetailsDao.saveOrUpdate(clientDetails);
    return clientDetails;
  }

  public ClientDetails getClientDetailsById(Integer id) {
    return clientDetailsDao.getClientDetailsById(id);
  }

  public ClientDetails getClientDetailsByUrl(String url) {
    return clientDetailsDao.getClientDetailsByUrl(url);
  }

  public List<ClientDetails> getAllClientDetails() {
    return clientDetailsDao.getAllClientDetails();
  }
}
