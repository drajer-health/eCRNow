package com.drajer.ecrapp.util;

import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.scheduler.ScheduledJobData;
import com.drajer.ecrapp.dao.SchedulerDao;
import com.drajer.ecrapp.model.ScheduledTasks;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.reflect.Field;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class ScheduledTaskUtil {

  private static final Logger logger = LoggerFactory.getLogger(ScheduledTaskUtil.class);
  private static final String TASK_DATA_KEY = "task_data";
  private static final String EXPIRATION_TIME_KEY = "expirationTime";
  private static final String KAR_EXECUTION_STATE_ID_KEY = "karExecutionStateId";
  private static final String ACTION_ID_KEY = "actionId";
  private static final String ACTION_TYPE_KEY = "actionType";
  private static final String JOB_ID_KEY = "jobId";
  private static final String MDC_CONTEXT_KEY = "mdcContext";
  private static final String REQUEST_ID_KEY = "requestId";
  private static final String JOB_TYPE_KEY = "jobType";
  @Autowired private SchedulerDao schedulerDao;

  @Value("${scheduled.task.file.path}")
  private String scheduledTaskFilePath;

  private final ObjectMapper objectMapper = new ObjectMapper();

  /**
   * Updates scheduled tasks and stores them in JSON format.
   *
   * @return
   * @throws IOException if an I/O error occurs
   */
  public String exportScheduledTasks() throws IOException {
    List<ScheduledTasks> scheduledTasks = schedulerDao.getScheduledTasks();

    List<Map<String, Object>> taskJobDataMapList = new ArrayList<>();

    for (ScheduledTasks task : scheduledTasks) {
      ScheduledJobData scheduledJobData = deserialize(task.getTask_data());

      Map<String, Object> taskInfoMap = convertScheduledTaskToMap(task);
      taskInfoMap.put(TASK_DATA_KEY, convertObjectToMap(scheduledJobData));
      taskJobDataMapList.add(taskInfoMap);
    }

    try {
      String jsonData = objectMapper.writeValueAsString(taskJobDataMapList);
      FileUtils.saveDataToFile(jsonData, scheduledTaskFilePath);
      return scheduledTaskFilePath;
    } catch (JsonProcessingException e) {
      logger.error("Error converting scheduled tasks to JSON", e);
      throw new IOException("Error converting scheduled tasks to JSON", e);
    }
  }

  /**
   * Imports scheduled tasks from JSON file and saves them in the database.
   *
   * @return
   * @throws IOException
   */
  public String importScheduledTasks() throws IOException {
    List<Map<String, Object>> scheduledTasksList =
        FileUtils.readFileContents(
            scheduledTaskFilePath, new TypeReference<List<Map<String, Object>>>() {});

    for (Map<String, Object> scheduledTaskMap : scheduledTasksList) {
      Object taskData = scheduledTaskMap.get(TASK_DATA_KEY);

      if (taskData instanceof Map) {
        ScheduledJobData scheduledJobData =
            convertMapToScheduledJobData((Map<String, Object>) taskData);
        scheduledTaskMap.put(TASK_DATA_KEY, serialize(scheduledJobData));
      }

      ScheduledTasks scheduledTask =
          objectMapper.convertValue(scheduledTaskMap, ScheduledTasks.class);
      schedulerDao.saveOrUpdate(scheduledTask);
    }
    return scheduledTaskFilePath;
  }

  /**
   * Converts a ScheduledTasks object to a map.
   *
   * @param task the ScheduledTasks object to convert
   * @return a map representing the ScheduledTasks object
   */
  public Map<String, Object> convertScheduledTaskToMap(ScheduledTasks task) {
    Map<String, Object> taskInfoMap = new HashMap<>();
    Field[] fields = task.getClass().getDeclaredFields();

    for (Field field : fields) {
      field.setAccessible(true);
      try {
        Object value = field.get(task);
        taskInfoMap.put(field.getName(), value);
      } catch (IllegalAccessException e) {
        logger.error("Error accessing field {}", field.getName(), e);
      }
    }

    return taskInfoMap;
  }

  /**
   * Converts an object to a map.
   *
   * @param object the object to convert
   * @return a map representing the object
   */
  public Map<String, Object> convertObjectToMap(Object object) {
    try {
      return objectMapper.convertValue(object, Map.class);
    } catch (Exception e) {
      logger.error("Error converting object to map", e);
      throw e;
    }
  }

  /**
   * Deserializes byte array to ScheduledJobData object.
   *
   * @param serializedData the byte array to deserialize
   * @return the deserialized ScheduledJobData object
   * @throws IOException if an I/O error occurs
   */
  public ScheduledJobData deserialize(byte[] serializedData) throws IOException {
    if (serializedData == null || serializedData.length == 0) {
      throw new IllegalArgumentException("Cannot deserialize: task data is null or empty");
    }

    try (ByteArrayInputStream bis = new ByteArrayInputStream(serializedData);
        ObjectInputStream ois = new ObjectInputStream(bis)) {
      Object obj = ois.readObject();
      if (obj instanceof ScheduledJobData) {
        return (ScheduledJobData) obj;
      } else {
        throw new IllegalArgumentException(
            "Serialized data does not represent a ScheduledJobData object");
      }
    } catch (ClassNotFoundException | ClassCastException e) {
      throw new IOException("Deserialization failed", e);
    }
  }

  /**
   * Serializes ScheduledJobData object to byte array.
   *
   * @param obj the ScheduledJobData object to serialize
   * @return the byte array representing the serialized object
   * @throws IOException if an I/O error occurs
   */
  public byte[] serialize(ScheduledJobData obj) throws IOException {
    try (ByteArrayOutputStream byteStream = new ByteArrayOutputStream();
        ObjectOutputStream objectStream = new ObjectOutputStream(byteStream)) {
      objectStream.writeObject(obj);
      objectStream.flush();
      return byteStream.toByteArray();
    } catch (IOException e) {
      logger.error("Error serializing object", e);
      throw e;
    }
  }

  /**
   * Converts a map to ScheduledJobData object.
   *
   * @param map the map to convert
   * @return the converted ScheduledJobData object
   */
  public static ScheduledJobData convertMapToScheduledJobData(Map<String, Object> map) {
    UUID karExecutionStateId = UUID.fromString((String) map.get(KAR_EXECUTION_STATE_ID_KEY));
    String actionId = (String) map.get(ACTION_ID_KEY);
    String actionType = (String) map.get(ACTION_TYPE_KEY);
    String jobId = (String) map.get(JOB_ID_KEY);
    Instant expirationTime =
        map.containsKey(EXPIRATION_TIME_KEY) && map.get(EXPIRATION_TIME_KEY) != null
            ? Instant.parse((String) map.get(EXPIRATION_TIME_KEY))
            : null;
    @SuppressWarnings("unchecked")
    Map<String, String> mdcContext = (Map<String, String>) map.get(MDC_CONTEXT_KEY);
    String xRequestId = mdcContext != null ? mdcContext.get(REQUEST_ID_KEY) : null;

    BsaTypes.BsaJobType jobType = BsaTypes.BsaJobType.valueOf((String) map.get(JOB_TYPE_KEY));

    return new ScheduledJobData(
        karExecutionStateId,
        actionId,
        BsaTypes.ActionType.valueOf(actionType),
        expirationTime,
        jobId,
        xRequestId,
        jobType,
        mdcContext);
  }
}
