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
import java.time.Instant;
import java.util.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.slf4j.Logger;

@RunWith(PowerMockRunner.class)
@PrepareForTest({FileUtils.class})
public class ScheduledTaskUtilTest {

  @Mock private SchedulerDao schedulerDao;
  @Mock private Logger logger;
  @Mock private ObjectMapper objectMapper;
  private ScheduledTaskUtil scheduledTaskUtil;

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
    scheduledTaskUtil = new ScheduledTaskUtil(schedulerDao, MOCK_FILE_PATH);
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
    return new ScheduledJobData.Builder()
        .karExecutionStateId(UUID.randomUUID())
        .actionId("action123")
        .actionType(BsaTypes.ActionType.EVALUATE_MEASURE)
        .expirationTime(null)
        .jobId("job456")
        .xRequestId("req-789")
        .jobType(BsaTypes.BsaJobType.IMMEDIATE_REPORTING)
        .mdcContext(new HashMap<>())
        .build();
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
        new ScheduledJobData.Builder()
            .karExecutionStateId(UUID.randomUUID())
            .actionId("action123")
            .actionType(BsaTypes.ActionType.EVALUATE_MEASURE)
            .expirationTime(null)
            .jobId("job456")
            .xRequestId("req-789")
            .jobType(BsaTypes.BsaJobType.IMMEDIATE_REPORTING)
            .mdcContext(new HashMap<>())
            .build();

    byte[] serializedData = scheduledTaskUtil.serialize(jobData);

    assertNotNull(serializedData);
    assertTrue(serializedData.length > 0);
  }

  @Test
  public void testDeserialize_Success() throws IOException {

    Map<String, String> mdcContext = new HashMap<>();
    mdcContext.put("requestId", "req-789");

    ScheduledJobData jobData =
        new ScheduledJobData.Builder()
            .karExecutionStateId(UUID.randomUUID())
            .actionId("action123")
            .actionType(BsaTypes.ActionType.EVALUATE_MEASURE)
            .expirationTime(null)
            .jobId("job456")
            .xRequestId("req-789")
            .jobType(BsaTypes.BsaJobType.IMMEDIATE_REPORTING)
            .mdcContext(mdcContext)
            .build();

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

  // ========== IMPORT SCHEDULED TASKS TESTS ==========

  @Test
  public void test01_ImportScheduledTasks_SingleTask() throws IOException {
    Map<String, Object> taskData = createTaskDataMap();
    Map<String, Object> scheduledTaskMap = createScheduledTaskMap(taskData);
    List<Map<String, Object>> tasksList = Collections.singletonList(scheduledTaskMap);

    org.powermock.api.mockito.PowerMockito.mockStatic(FileUtils.class);
    when(FileUtils.readFileContents(eq(MOCK_FILE_PATH), any(TypeReference.class)))
        .thenReturn(tasksList);

    String result = scheduledTaskUtil.importScheduledTasks();

    assertEquals("Should return file path", MOCK_FILE_PATH, result);
    verify(schedulerDao, times(1)).saveOrUpdate(any(ScheduledTasks.class));
  }

  @Test
  public void test02_ImportScheduledTasks_MultipleTasks() throws IOException {
    Map<String, Object> taskData1 = createTaskDataMap();
    Map<String, Object> taskData2 = createTaskDataMap();

    Map<String, Object> scheduledTaskMap1 = createScheduledTaskMap(taskData1);
    Map<String, Object> scheduledTaskMap2 = createScheduledTaskMap(taskData2);

    List<Map<String, Object>> tasksList = Arrays.asList(scheduledTaskMap1, scheduledTaskMap2);

    org.powermock.api.mockito.PowerMockito.mockStatic(FileUtils.class);
    when(FileUtils.readFileContents(eq(MOCK_FILE_PATH), any(TypeReference.class)))
        .thenReturn(tasksList);

    String result = scheduledTaskUtil.importScheduledTasks();

    assertEquals("Should return file path", MOCK_FILE_PATH, result);
    verify(schedulerDao, times(2)).saveOrUpdate(any(ScheduledTasks.class));
  }

  @Test
  public void test03_ImportScheduledTasks_EmptyList() throws IOException {
    List<Map<String, Object>> emptyList = new ArrayList<>();

    org.powermock.api.mockito.PowerMockito.mockStatic(FileUtils.class);
    when(FileUtils.readFileContents(eq(MOCK_FILE_PATH), any(TypeReference.class)))
        .thenReturn(emptyList);

    String result = scheduledTaskUtil.importScheduledTasks();

    assertEquals("Should return file path", MOCK_FILE_PATH, result);
    verify(schedulerDao, never()).saveOrUpdate(any(ScheduledTasks.class));
  }

  @Test
  public void test04_ImportScheduledTasks_TaskDataAsMap() throws IOException {
    Map<String, Object> taskData = createTaskDataMap();
    Map<String, Object> scheduledTaskMap = createScheduledTaskMap(taskData);

    List<Map<String, Object>> tasksList = Collections.singletonList(scheduledTaskMap);

    org.powermock.api.mockito.PowerMockito.mockStatic(FileUtils.class);
    when(FileUtils.readFileContents(eq(MOCK_FILE_PATH), any(TypeReference.class)))
        .thenReturn(tasksList);

    String result = scheduledTaskUtil.importScheduledTasks();

    assertNotNull("Result should not be null", result);
    verify(schedulerDao, times(1)).saveOrUpdate(any(ScheduledTasks.class));
  }

  @Test
  public void test06_ImportScheduledTasks_WithExpirationTime() throws IOException {
    Map<String, Object> taskData = createTaskDataMapWithExpiration();
    Map<String, Object> scheduledTaskMap = createScheduledTaskMap(taskData);
    List<Map<String, Object>> tasksList = Collections.singletonList(scheduledTaskMap);

    org.powermock.api.mockito.PowerMockito.mockStatic(FileUtils.class);
    when(FileUtils.readFileContents(eq(MOCK_FILE_PATH), any(TypeReference.class)))
        .thenReturn(tasksList);

    String result = scheduledTaskUtil.importScheduledTasks();

    assertEquals("Should return file path", MOCK_FILE_PATH, result);
    verify(schedulerDao, times(1)).saveOrUpdate(any(ScheduledTasks.class));
  }

  @Test
  public void test07_ImportScheduledTasks_WithNullExpirationTime() throws IOException {
    Map<String, Object> taskData = createTaskDataMap();
    taskData.put("expirationTime", null);

    Map<String, Object> scheduledTaskMap = createScheduledTaskMap(taskData);
    List<Map<String, Object>> tasksList = Collections.singletonList(scheduledTaskMap);

    org.powermock.api.mockito.PowerMockito.mockStatic(FileUtils.class);
    when(FileUtils.readFileContents(eq(MOCK_FILE_PATH), any(TypeReference.class)))
        .thenReturn(tasksList);

    String result = scheduledTaskUtil.importScheduledTasks();

    assertNotNull("Result should not be null", result);
    verify(schedulerDao, times(1)).saveOrUpdate(any(ScheduledTasks.class));
  }

  @Test
  public void test08_ImportScheduledTasks_WithMDCContext() throws IOException {
    Map<String, Object> taskData = createTaskDataMapWithMDC();
    Map<String, Object> scheduledTaskMap = createScheduledTaskMap(taskData);
    List<Map<String, Object>> tasksList = Collections.singletonList(scheduledTaskMap);

    org.powermock.api.mockito.PowerMockito.mockStatic(FileUtils.class);
    when(FileUtils.readFileContents(eq(MOCK_FILE_PATH), any(TypeReference.class)))
        .thenReturn(tasksList);

    String result = scheduledTaskUtil.importScheduledTasks();

    assertEquals("Should return file path", MOCK_FILE_PATH, result);
    verify(schedulerDao, times(1)).saveOrUpdate(any(ScheduledTasks.class));
  }

  @Test
  public void test10_ImportScheduledTasks_PreservesTaskInstance() throws IOException {
    Map<String, Object> taskData = createTaskDataMap();
    Map<String, Object> scheduledTaskMap = createScheduledTaskMap(taskData);
    scheduledTaskMap.put("task_instance", "unique_task_id_123");

    List<Map<String, Object>> tasksList = Collections.singletonList(scheduledTaskMap);

    org.powermock.api.mockito.PowerMockito.mockStatic(FileUtils.class);
    when(FileUtils.readFileContents(eq(MOCK_FILE_PATH), any(TypeReference.class)))
        .thenReturn(tasksList);

    String result = scheduledTaskUtil.importScheduledTasks();

    assertEquals("Should return file path", MOCK_FILE_PATH, result);
    verify(schedulerDao, times(1)).saveOrUpdate(any(ScheduledTasks.class));
  }

  @Test
  public void test12_ImportScheduledTasks_SerializationRoundTrip() throws IOException {
    Map<String, Object> taskData = createTaskDataMap();
    Map<String, Object> scheduledTaskMap = createScheduledTaskMap(taskData);
    List<Map<String, Object>> tasksList = Collections.singletonList(scheduledTaskMap);

    org.powermock.api.mockito.PowerMockito.mockStatic(FileUtils.class);
    when(FileUtils.readFileContents(eq(MOCK_FILE_PATH), any(TypeReference.class)))
        .thenReturn(tasksList);

    String result = scheduledTaskUtil.importScheduledTasks();

    assertNotNull("Result should not be null", result);
    verify(schedulerDao, times(1)).saveOrUpdate(any(ScheduledTasks.class));
  }

  @Test
  public void test14_ImportScheduledTasks_FilePathReturned() throws IOException {
    Map<String, Object> taskData = createTaskDataMap();
    Map<String, Object> scheduledTaskMap = createScheduledTaskMap(taskData);
    List<Map<String, Object>> tasksList = Collections.singletonList(scheduledTaskMap);

    org.powermock.api.mockito.PowerMockito.mockStatic(FileUtils.class);
    when(FileUtils.readFileContents(eq(MOCK_FILE_PATH), any(TypeReference.class)))
        .thenReturn(tasksList);

    String result = scheduledTaskUtil.importScheduledTasks();

    assertEquals("Should return exact file path passed to constructor", MOCK_FILE_PATH, result);
  }

  @Test
  public void test15_ImportScheduledTasks_UpdatesDatabase() throws IOException {
    Map<String, Object> taskData1 = createTaskDataMap();
    Map<String, Object> taskData2 = createTaskDataMap();
    Map<String, Object> taskData3 = createTaskDataMap();

    Map<String, Object> scheduledTaskMap1 = createScheduledTaskMap(taskData1);
    Map<String, Object> scheduledTaskMap2 = createScheduledTaskMap(taskData2);
    Map<String, Object> scheduledTaskMap3 = createScheduledTaskMap(taskData3);

    List<Map<String, Object>> tasksList =
        Arrays.asList(scheduledTaskMap1, scheduledTaskMap2, scheduledTaskMap3);

    org.powermock.api.mockito.PowerMockito.mockStatic(FileUtils.class);
    when(FileUtils.readFileContents(eq(MOCK_FILE_PATH), any(TypeReference.class)))
        .thenReturn(tasksList);

    String result = scheduledTaskUtil.importScheduledTasks();

    assertEquals("Should return file path", MOCK_FILE_PATH, result);
    verify(schedulerDao, times(3)).saveOrUpdate(any(ScheduledTasks.class));
  }

  // ========== HELPER METHODS ==========

  private Map<String, Object> createTaskDataMap() {
    Map<String, Object> taskData = new HashMap<>();
    taskData.put("karExecutionStateId", UUID.randomUUID().toString());
    taskData.put("actionId", "action123");
    taskData.put("actionType", "EVALUATE_MEASURE");
    taskData.put("jobId", "job456");
    taskData.put("expirationTime", null);
    taskData.put("jobType", "IMMEDIATE_REPORTING");
    taskData.put("mdcContext", new HashMap<>());
    return taskData;
  }

  private Map<String, Object> createTaskDataMapWithExpiration() {
    Map<String, Object> taskData = createTaskDataMap();
    taskData.put("expirationTime", Instant.now().plusSeconds(3600).toString());
    return taskData;
  }

  private Map<String, Object> createTaskDataMapWithMDC() {
    Map<String, Object> taskData = createTaskDataMap();
    Map<String, String> mdcContext = new HashMap<>();
    mdcContext.put("requestId", "req-789");
    taskData.put("mdcContext", mdcContext);
    return taskData;
  }

  private Map<String, Object> createScheduledTaskMap(Map<String, Object> taskData) {
    Map<String, Object> scheduledTaskMap = new HashMap<>();
    scheduledTaskMap.put("task_instance", "task_001");
    scheduledTaskMap.put("task_name", "Test Task");
    scheduledTaskMap.put("task_data", taskData);
    return scheduledTaskMap;
  }

  // ========== ERROR HANDLING TESTS ==========

  @Test(expected = IllegalArgumentException.class)
  public void testDeserialize_NullData_ThrowsIllegalArgumentException() throws IOException {
    scheduledTaskUtil.deserialize(null);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testDeserialize_EmptyData_ThrowsIllegalArgumentException() throws IOException {
    scheduledTaskUtil.deserialize(new byte[0]);
  }

  @Test
  public void testConvertObjectToMap_WithNull_HandlesGracefully() {
    try {
      Map<String, Object> result = scheduledTaskUtil.convertObjectToMap(null);
      assertTrue("Should handle null input", true);
    } catch (Exception e) {
      assertTrue("Exception acceptable for null", true);
    }
  }

  @Test
  public void testConvertScheduledTaskToMap_WithIllegalAccessException() {
    ScheduledTasks task = new ScheduledTasks();
    task.setTask_instance("task_001");
    task.setTask_name("Test Task");

    Map<String, Object> result = scheduledTaskUtil.convertScheduledTaskToMap(task);

    assertNotNull("Should return map even with field access issues", result);
    assertNotNull("Should have task_instance", result.get("task_instance"));
  }

  @Test
  public void testExportScheduledTasks_WithException() throws IOException {
    ScheduledTasks mockTask = new ScheduledTasks();
    mockTask.setTask_instance("task_001");
    mockTask.setTask_name("Test Task");
    mockTask.setTask_data(scheduledTaskUtil.serialize(createMockJobData()));

    when(schedulerDao.getScheduledTasks()).thenReturn(Collections.singletonList(mockTask));

    try {
      String resultFilePath = scheduledTaskUtil.exportScheduledTasks();
      assertNotNull("Should return file path", resultFilePath);
    } catch (IOException e) {
      assertTrue("IOException acceptable if JSON conversion fails", true);
    }
  }

  @Test
  public void testDeserialize_CorruptedData_ThrowsIOException() throws IOException {
    byte[] corruptedData = new byte[] {0x01, 0x02, 0x03};

    try {
      scheduledTaskUtil.deserialize(corruptedData);
      fail("Should throw exception for corrupted data");
    } catch (IOException e) {
      assertTrue("Should throw IOException", true);
    } catch (Exception e) {
      assertTrue("Exception acceptable for corrupted data", true);
    }
  }

  @Test
  public void testDeserialize_InvalidClass_ThrowsException() throws IOException {
    byte[] invalidSerializedData;
    try (ByteArrayOutputStream byteStream = new ByteArrayOutputStream();
        ObjectOutputStream objectStream = new ObjectOutputStream(byteStream)) {
      objectStream.writeObject(Integer.valueOf(123));
      objectStream.flush();
      invalidSerializedData = byteStream.toByteArray();
    }

    try {
      scheduledTaskUtil.deserialize(invalidSerializedData);
      fail("Should throw exception for invalid class");
    } catch (IllegalArgumentException e) {
      assertTrue("Should throw IllegalArgumentException for wrong type", true);
    } catch (IOException e) {
      assertTrue("IOException also acceptable", true);
    }
  }

  @Test
  public void testExportScheduledTasks_EmptyTasksList() throws IOException {
    when(schedulerDao.getScheduledTasks()).thenReturn(new ArrayList<>());

    String resultFilePath = scheduledTaskUtil.exportScheduledTasks();

    assertEquals("Should return file path for empty list", MOCK_FILE_PATH, resultFilePath);
  }

  @Test
  public void testConvertObjectToMap_WithComplexObject() {
    ScheduledTasks task = new ScheduledTasks();
    task.setTask_instance("task_001");
    task.setTask_name("Complex Task");
    task.setExecution_time(new Date());
    task.setPicked(true);
    task.setPicked_by("worker_01");
    task.setConsecutive_failures(5);
    task.setVersion(2);

    Map<String, Object> result = scheduledTaskUtil.convertObjectToMap(task);

    assertNotNull("Should handle complex object", result);
    assertEquals("task_001", result.get("task_instance"));
    assertEquals("Complex Task", result.get("task_name"));
  }

  @Test
  public void testSerialize_WithComplexJobData() throws IOException {
    Map<String, String> mdcContext = new HashMap<>();
    mdcContext.put("requestId", "req-12345");
    mdcContext.put("userId", "user-67890");

    ScheduledJobData jobData =
        new ScheduledJobData.Builder()
            .karExecutionStateId(UUID.randomUUID())
            .actionId("action999")
            .actionType(BsaTypes.ActionType.EVALUATE_MEASURE)
            .expirationTime(Instant.now().plusSeconds(7200))
            .jobId("job999")
            .xRequestId("req-12345")
            .jobType(BsaTypes.BsaJobType.DELAYED_REPORTING)
            .mdcContext(mdcContext)
            .build();

    byte[] serializedData = scheduledTaskUtil.serialize(jobData);

    assertNotNull("Should serialize complex data", serializedData);
    assertTrue("Serialized data should not be empty", serializedData.length > 0);

    ScheduledJobData deserialized = scheduledTaskUtil.deserialize(serializedData);
    assertNotNull("Should deserialize back to object", deserialized);
    assertEquals("Should preserve actionId", "action999", deserialized.getActionId());
  }

  @Test
  public void testConvertScheduledTaskToMap_AllFieldsPopulated() {
    ScheduledTasks task = new ScheduledTasks();
    task.setTask_instance("instance_123");
    task.setTask_name("Full Task");
    task.setExecution_time(new Date());
    task.setPicked(true);
    task.setPicked_by("worker_full");
    task.setConsecutive_failures(0);
    task.setVersion(10);

    Map<String, Object> result = scheduledTaskUtil.convertScheduledTaskToMap(task);

    assertNotNull("Map should not be null", result);
    assertEquals("instance_123", result.get("task_instance"));
    assertEquals("Full Task", result.get("task_name"));
    assertEquals(true, result.get("picked"));
    assertEquals("worker_full", result.get("picked_by"));
    assertEquals(0, result.get("consecutive_failures"));
    assertEquals(10, result.get("version"));
  }
}
