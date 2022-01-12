package com.drajer.bsa.dao;

import com.drajer.bsa.model.KarExecutionState;
import java.util.List;
import java.util.UUID;

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

  public KarExecutionState getKarExecutionStateById(UUID id);

  public List<KarExecutionState> getAllKarExecutionStates();
}
