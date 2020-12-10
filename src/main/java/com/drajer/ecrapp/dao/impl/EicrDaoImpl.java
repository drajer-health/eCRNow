package com.drajer.ecrapp.dao.impl;

import com.drajer.ecrapp.dao.AbstractDao;
import com.drajer.ecrapp.dao.EicrDao;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
import java.util.List;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public class EicrDaoImpl extends AbstractDao implements EicrDao {

  public Eicr saveOrUpdate(Eicr eicr) {
    getSession().saveOrUpdate(eicr);
    return eicr;
  }

  public Eicr getEicrById(Integer id) {
    return getSession().get(Eicr.class, id);
  }

  public ReportabilityResponse saveOrUpdate(ReportabilityResponse rr) {
    getSession().saveOrUpdate(rr);
    return rr;
  }

  public ReportabilityResponse getRRById(Integer id) {
    return getSession().get(ReportabilityResponse.class, id);
  }

  public Integer getMaxSetId(Eicr eicr) {
    String query =
        "SELECT * from eicr where fhir_server_url='"
            + eicr.getFhirServerUrl()
            + "' AND launch_patient_id='"
            + eicr.getLaunchPatientId()
            + "' AND encounter_id='"
            + eicr.getEncounterId()
            + "' ORDER BY set_id DESC LIMIT 1;";
    List<Eicr> eicrList = getSession().createNativeQuery(query, Eicr.class).getResultList();
    return eicrList.get(0).getSetId();
  }
}
