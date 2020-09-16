package com.drajer.sof.service;

import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;
import java.util.Date;

public interface AbstractQueryService {

  public FhirData getData(LaunchDetails details, Date start, Date end);
}
