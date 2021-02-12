package com.drajer.ecrapp.dao;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
import java.util.List;
import java.util.Map;

public interface EicrDao {

  Eicr saveOrUpdate(Eicr eicr);

  Eicr getEicrById(Integer id);

  ReportabilityResponse saveOrUpdate(ReportabilityResponse rr);

  ReportabilityResponse getRRById(Integer id);

  Integer getMaxVersionId(Eicr eicr);

  Eicr getEicrByCoorrelationId(String coorelId);

  List<Eicr> getEicrData(Map<String, String> searchParams);

  List<Eicr> getRRData(Map<String, String> searchParams);

  Eicr getEicrByDocId(String docId);
}
