package com.drajer.sof.dao;

import com.drajer.sof.model.ClientDetails;

public interface ClientDetailsDao {

	ClientDetails saveOrUpdate(ClientDetails clientDetails);
	
	ClientDetails getClientDetailsById(Integer id);
	
	ClientDetails getClientDetailsByUrl(String url);
}
