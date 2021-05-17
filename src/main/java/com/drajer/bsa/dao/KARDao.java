package com.drajer.bsa.dao;

import com.drajer.bsa.model.KAR;
import java.util.List;

public interface KARDao {

  public KAR saveOrUpdate(KAR kar);

  public KAR getKARById(Integer id);

  public KAR getKARByUrl(String url);

  public List<KAR> getAllKARs();
}
