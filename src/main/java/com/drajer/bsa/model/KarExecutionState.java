package com.drajer.bsa.model;

import com.drajer.bsa.kar.action.BsaActionStatus;
import jakarta.persistence.*;
import java.util.List;
import java.util.UUID;
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

  /** The attribute represents the primary key for the table and is auto generated. */
  @Id @GeneratedValue private UUID id;

  /** The attribute represents the key to retrieve the notification context for executing the Kar */
  @Column(name = "nc_id", nullable = false, columnDefinition = "uuid")
  private UUID ncId;

  /** The attribute links back to the Notification Context */
  @ManyToOne
  @JoinColumn(name = "nc_fk")
  private NotificationContext nc;

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

  /**
   * The attribute represents the state of the execution for the various actions that have been
   * completed during a single execution either due to a timer or due to a notification.
   */
  @Column(name = "action_status", nullable = true, columnDefinition = "TEXT")
  private String actionStatus;

  /**
   * This attribute used for convenience to exchange the information as objects. The data itself
   * gets stored in the actionStatus attribute in the database.
   */
  @Transient private List<BsaActionStatus> actionStatuses;

  KarExecutionState() {}

  public UUID getId() {
    return id;
  }

  public void setId(UUID id) {
    this.id = id;
  }

  public UUID getNcId() {
    return ncId;
  }

  public void setNcId(UUID ncId) {
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
