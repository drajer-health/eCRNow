package com.drajer.ecrapp.util;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.scheduler.ScheduledJobData;
import com.drajer.ecrapp.dao.SchedulerDao;
import com.drajer.ecrapp.model.ScheduledTasks;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.slf4j.Logger;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
@PrepareForTest({FileUtils.class})
public class ScheduledTaskUtilTest {
  @Mock private SchedulerDao schedulerDao;
  @Mock private Logger logger;
  @Mock private ObjectMapper objectMapper;
  @InjectMocks private ScheduledTaskUtil scheduledTaskUtil;

  private String MOCK_FILE_PATH = "ecrTestData/ScheduleUtils/schedule.json";

  private static final TypeReference<List<Map<String, Object>>> LIST_MAP_TYPE_REF =
      new TypeReference<List<Map<String, Object>>>() {};

  private static class SampleObject {
    public String name = "John";
    public int age = 30;
  }

  @Before
  public void setUp() {

    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(scheduledTaskUtil, "scheduledTaskFilePath", MOCK_FILE_PATH);
  }

  @Test
  public void testExportScheduledTasks() throws IOException {

    ScheduledTasks mockTask = new ScheduledTasks();
    mockTask.setTask_instance("task_001");
    mockTask.setTask_name("Test Task");
    mockTask.setTask_data(scheduledTaskUtil.serialize(createMockJobData()));

    when(schedulerDao.getScheduledTasks()).thenReturn(Collections.singletonList(mockTask));

    String resultFilePath = scheduledTaskUtil.exportScheduledTasks();

    assertEquals(MOCK_FILE_PATH, resultFilePath);
    verify(schedulerDao, times(1)).getScheduledTasks();
  }

  private ScheduledJobData createMockJobData() {
    return new ScheduledJobData(
        UUID.randomUUID(),
        "action123",
        BsaTypes.ActionType.EVALUATE_MEASURE,
        null,
        "job456",
        "req-789",
        BsaTypes.BsaJobType.IMMEDIATE_REPORTING,
        new HashMap<>());
  }

  @Test
  public void testConvertScheduledTaskToMap_Success() {
    ScheduledTasks task = new ScheduledTasks();
    task.setTask_instance("task_001");
    task.setTask_name("Test Task");
    task.setExecution_time(new Date());
    task.setPicked(true);
    task.setPicked_by("worker_01");
    task.setConsecutive_failures(3);
    task.setVersion(1);

    Map<String, Object> result = scheduledTaskUtil.convertScheduledTaskToMap(task);

    assertNotNull(result);
    assertEquals("task_001", result.get("task_instance"));
    assertEquals("Test Task", result.get("task_name"));
    assertEquals(true, result.get("picked"));
    assertEquals("worker_01", result.get("picked_by"));
    assertEquals(3, result.get("consecutive_failures"));
    assertEquals(1, result.get("version"));
  }

  @Test
  public void testSerialize_Success() throws IOException {

    ScheduledJobData jobData =
        new ScheduledJobData(
            UUID.randomUUID(),
            "action123",
            BsaTypes.ActionType.EVALUATE_MEASURE,
            null,
            "job456",
            "req-789",
            BsaTypes.BsaJobType.IMMEDIATE_REPORTING,
            new HashMap<>());

    byte[] serializedData = scheduledTaskUtil.serialize(jobData);

    assertNotNull(serializedData);
    assertTrue(serializedData.length > 0);
  }

  @Test
  public void testDeserialize_Success() throws IOException {

    Map<String, String> mdcContext = new HashMap<>();
    mdcContext.put("requestId", "req-789");

    ScheduledJobData jobData =
        new ScheduledJobData(
            UUID.randomUUID(),
            "action123",
            BsaTypes.ActionType.EVALUATE_MEASURE,
            null,
            "job456",
            "req-789",
            BsaTypes.BsaJobType.IMMEDIATE_REPORTING,
            mdcContext);

    byte[] serializedData = scheduledTaskUtil.serialize(jobData);

    ScheduledJobData deserializedJobData = scheduledTaskUtil.deserialize(serializedData);

    assertNotNull(deserializedJobData);
    assertEquals(jobData.getActionId(), deserializedJobData.getActionId());
    assertEquals(jobData.getActionType(), deserializedJobData.getActionType());
    assertEquals(jobData.getJobId(), deserializedJobData.getJobId());
    assertEquals(jobData.getJobType(), deserializedJobData.getJobType());

    assertEquals(
        jobData.getMdcContext().get("requestId"),
        deserializedJobData.getMdcContext().get("requestId"));
  }

  @Test
  public void testDeserialize_InvalidObjectType_ThrowsIllegalArgumentException()
      throws IOException {

    byte[] invalidSerializedData;
    try (ByteArrayOutputStream byteStream = new ByteArrayOutputStream();
        ObjectOutputStream objectStream = new ObjectOutputStream(byteStream)) {
      objectStream.writeObject("This is not a ScheduledJobData");
      objectStream.flush();
      invalidSerializedData = byteStream.toByteArray();
    }

    IllegalArgumentException exception =
        assertThrows(
            IllegalArgumentException.class,
            () -> {
              scheduledTaskUtil.deserialize(invalidSerializedData);
            });

    assertEquals(
        "Serialized data does not represent a ScheduledJobData object", exception.getMessage());
  }

  @Test
  public void testDeserialize_InvalidType_ShouldThrowIllegalArgumentException() throws IOException {

    byte[] invalidSerializedData;
    try (ByteArrayOutputStream bos = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(bos)) {
      oos.writeObject("This is a string, not ScheduledJobData");
      invalidSerializedData = bos.toByteArray();
    }

    IllegalArgumentException thrown =
        assertThrows(
            IllegalArgumentException.class,
            () -> scheduledTaskUtil.deserialize(invalidSerializedData));

    assertTrue(
        thrown
            .getMessage()
            .contains("Serialized data does not represent a ScheduledJobData object"));
  }

  @Test
  public void testConvertObjectToMap_Success() {

    ScheduledTasks task = new ScheduledTasks();
    task.setTask_instance("task_001");
    task.setTask_name("Test Task");
    task.setExecution_time(new Date());
    task.setPicked(true);
    task.setPicked_by("worker_01");
    task.setConsecutive_failures(3);
    task.setVersion(1);

    Map<String, Object> result = scheduledTaskUtil.convertObjectToMap(task);

    assertNotNull(result);
    assertEquals("task_001", result.get("task_instance"));
    assertEquals("Test Task", result.get("task_name"));
    assertEquals(true, result.get("picked"));
    assertEquals("worker_01", result.get("picked_by"));
    assertEquals(3, result.get("consecutive_failures"));
    assertEquals(1, result.get("version"));
  }
}
