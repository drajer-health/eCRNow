package com.drajer.sof.dao;

import com.drajer.sof.model.LaunchDetails;

public interface LaunchDetailsDao {
	
	LaunchDetails saveOrUpdate(LaunchDetails authDetails);
	
	LaunchDetails getAuthDetailsById(Integer id);

}
