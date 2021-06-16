package com.drajer.bsa.dao;

import com.drajer.bsa.model.KarExecutionState;
import java.util.List;

/**
 *
 *
 * <h1>KarExecutionStateDao</h1>
 *
 * This interface declares methods to perform CRUD operations on the KarExecutionState.
 *
 * @author nbashyam
 */
public interface KarExecutionStateDao {

  public KarExecutionState saveOrUpdate(KarExecutionState kar);

  public KarExecutionState getKarExecutionStateById(Integer id);

  public List<KarExecutionState> getAllKarExecutionStates();
}
