package com.drajer.ecrapp.model;

import java.io.Serializable;
import java.util.Date;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.Table;
import org.hibernate.annotations.Type;

@Entity
@Table(name = "scheduled_tasks")
public class ScheduledTasks implements Serializable {

  @Id
  @Column(name = "task_instance")
  private String task_instance;

  @Id
  @Column(name = "task_name")
  private String task_name;

  @Lob
  @Column(name = "task_data", length = 1000000)
  @Type(type = "org.hibernate.type.BinaryType")
  private byte[] task_data;

  @Column(name = "execution_time")
  private Date execution_time;

  @Column(name = "picked")
  private boolean picked;

  @Column(name = "picked_by")
  private String picked_by;

  @Column(name = "last_success")
  private Date last_success;

  @Column(name = "last_failure")
  private Date last_failure;

  @Column(name = "consecutive_failures")
  private int consecutive_failures;

  @Column(name = "last_heartbeat")
  private Date last_heartbeat;

  @Column(name = "version")
  private int version;

  public byte[] getTask_data() {
    return task_data;
  }

  public void setTask_data(byte[] task_data) {
    this.task_data = task_data;
  }

  public Date getExecution_time() {
    return execution_time;
  }

  public void setExecution_time(Date execution_time) {
    this.execution_time = execution_time;
  }

  public boolean isPicked() {
    return picked;
  }

  public void setPicked(boolean picked) {
    this.picked = picked;
  }

  public String getPicked_by() {
    return picked_by;
  }

  public void setPicked_by(String picked_by) {
    this.picked_by = picked_by;
  }

  public Date getLast_success() {
    return last_success;
  }

  public void setLast_success(Date last_success) {
    this.last_success = last_success;
  }

  public Date getLast_failure() {
    return last_failure;
  }

  public void setLast_failure(Date last_failure) {
    this.last_failure = last_failure;
  }

  public int getConsecutive_failures() {
    return consecutive_failures;
  }

  public void setConsecutive_failures(int consecutive_failures) {
    this.consecutive_failures = consecutive_failures;
  }

  public Date getLast_heartbeat() {
    return last_heartbeat;
  }

  public void setLast_heartbeat(Date last_heartbeat) {
    this.last_heartbeat = last_heartbeat;
  }

  public int getVersion() {
    return version;
  }

  public void setVersion(int version) {
    this.version = version;
  }

  public String getTask_instance() {
    return task_instance;
  }

  public void setTask_instance(String task_instance) {
    this.task_instance = task_instance;
  }

  public String getTask_name() {
    return task_name;
  }

  public void setTask_name(String task_name) {
    this.task_name = task_name;
  }
}
