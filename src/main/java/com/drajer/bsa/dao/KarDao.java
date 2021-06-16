package com.drajer.bsa.dao;

import com.drajer.bsa.model.KAR;
import java.util.List;

/**
 *
 *
 * <h1>KarDao</h1>
 *
 * This interface declares methods to perform CRUD operations on the Knowledge Artifact.
 *
 * @author nbashyam
 */
public interface KarDao {

  public KAR saveOrUpdate(KAR kar);

  public KAR getKARById(Integer id);

  public KAR getKARByUrl(String url);

  public List<KAR> getAllKARs();
}
