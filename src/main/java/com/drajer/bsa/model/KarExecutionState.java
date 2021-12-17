package com.drajer.bsa.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;
import org.hibernate.annotations.DynamicUpdate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 *
 * <h1>KarExecutionState</h1>
 *
 * The Entity represents the execution context that can be saved for processing across timers and
 * jobs. In other words as data is collected by executing a series of actions and jobs are scheduled
 * to execute events at a later time, there is no need to re-create all the data that future actions
 * in the same workflow require. For example, if the check-trigger-codes action finds a match but
 * the create-report action has to be executed after a period of 1 hour, then a timer will be set
 * for 1 hour. When the timer expires instead of recomputing the check-trigger-codes result, it is
 * optimal to store the result temporarily until the workflow completes and then reuse the data.
 * This will reduce the burden on the EHRs and also will enable the cleanup of state after the
 * workflow is completed.
 *
 * <p>The KarExecutionState contains all the information to recreate the KarProcessingData which is
 * the transient version and is used across actions for computation. The reason for splitting the
 * two is to ensure that minmimum data is stored in the db temporarily to establish state and not
 * dump everything that is represented in different formats for processing purposes.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
@Entity
@Table(name = "kar_execution_state")
@DynamicUpdate
public class KarExecutionState {

  @Transient private final Logger logger = LoggerFactory.getLogger(KarExecutionState.class);

  /** The attribute represents the primary key for the table and is auto incremented. */
  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Integer id;

  /** The attribute represents the key to retrieve the notification context for executing the Kar */
  @Column(name = "nc_id", nullable = false, columnDefinition = "INTEGER")
  private Integer ncId;

  /**
   * The attribute represents the key to retrieve the health care setting information for executing
   * the Kar
   */
  @Column(name = "hs_fhir_server_url", nullable = false, columnDefinition = "TEXT")
  private String hsFhirServerUrl;

  /**
   * The attribute represents the key to retrieve the Knowledge Artifact information for executing
   * the Kar
   */
  @Column(name = "kar_unique_id", nullable = false, columnDefinition = "TEXT")
  private String karUniqueId;

  KarExecutionState() {}

  public Integer getId() {
    return id;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public Integer getNcId() {
    return ncId;
  }

  public void setNcId(Integer ncId) {
    this.ncId = ncId;
  }

  public String getHsFhirServerUrl() {
    return hsFhirServerUrl;
  }

  public void setHsFhirServerUrl(String hsFhirServerUrl) {
    this.hsFhirServerUrl = hsFhirServerUrl;
  }

  public String getKarUniqueId() {
    return karUniqueId;
  }

  public void setKarUniqueId(String karUniqueId) {
    this.karUniqueId = karUniqueId;
  }
}
