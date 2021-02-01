package com.drajer.ecrapp.dao;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;

public interface EicrDao {

  Eicr saveOrUpdate(Eicr eicr);

  Eicr getEicrById(Integer id);

  ReportabilityResponse saveOrUpdate(ReportabilityResponse rr);

  ReportabilityResponse getRRById(Integer id);

  Integer getMaxVersionId(Eicr eicr);

  Eicr getEicrByCoorrelationId(String coorelId);

  Eicr getEicrByDocId(String docId);
}
