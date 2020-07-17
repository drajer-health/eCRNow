package com.drajer.sof.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.drajer.sof.dao.ClientDetailsDao;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.service.ClientDetailsService;

@Service
@Transactional
public class ClientDetailsServiceImpl implements ClientDetailsService{

	@Autowired
	ClientDetailsDao clientDetailsDao;
	
	public ClientDetails saveOrUpdate(ClientDetails clientDetails) {
		clientDetailsDao.saveOrUpdate(clientDetails);
		return clientDetails;
	}

	public ClientDetails getClientDetailsById(Integer id) {
		ClientDetails clientDetails = clientDetailsDao.getClientDetailsById(id);
		return clientDetails;
	}
	
	public ClientDetails getClientDetailsByUrl(String url,boolean isSystem) {
		ClientDetails clientDetails = clientDetailsDao.getClientDetailsByUrl(url,isSystem);
		return clientDetails;
	}
	
	public List<ClientDetails> getAllClientDetails() {
		List<ClientDetails> clientDetailsList = clientDetailsDao.getAllClientDetails();
		return clientDetailsList;
	}

}
