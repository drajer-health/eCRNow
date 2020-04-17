package com.drajer.sof.service;

import com.drajer.sof.model.LaunchDetails;

public interface LaunchService {

	LaunchDetails saveOrUpdate(LaunchDetails authDetails);
	
	LaunchDetails getAuthDetailsById(Integer id);
}
