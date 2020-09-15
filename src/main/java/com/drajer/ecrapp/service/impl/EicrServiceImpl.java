package com.drajer.ecrapp.service.impl;

import com.drajer.ecrapp.dao.EicrDao;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
import com.drajer.ecrapp.service.EicrRRService;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class EicrServiceImpl implements EicrRRService {

  @Autowired EicrDao eicrDao;

  public Eicr saveOrUpdate(Eicr eicr) {
    eicrDao.saveOrUpdate(eicr);
    return eicr;
  }

  public Eicr getEicrById(Integer id) {
    return eicrDao.getEicrById(id);
  }

  public ReportabilityResponse saveOrUpdate(ReportabilityResponse rr) {
    eicrDao.saveOrUpdate(rr);
    return rr;
  }

  public ReportabilityResponse getRRById(Integer id) {
    return eicrDao.getRRById(id);
  }
}
