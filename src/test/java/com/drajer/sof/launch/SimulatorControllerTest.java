package com.drajer.sof.launch;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.times;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.when;

import com.drajer.eca.model.ActionRepo;
import com.drajer.ecrapp.util.ApplicationUtils;
import java.util.UUID;
import org.json.JSONObject;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@RunWith(PowerMockRunner.class)
@PrepareForTest({UUID.class, ActionRepo.class, ApplicationUtils.class})
public class SimulatorControllerTest {

  @Test
  public void testReceiveEICR_Success() throws Exception {

    SimulatorController controller = new SimulatorController();
    String requestBody = "{\"key\":\"value\"}";
    String mockDir = "/mocked/path";

    UUID mockUUID = UUID.fromString("123e4567-e89b-12d3-a456-556642440000");
    String expectedFileName = mockDir + "/ReceivedEICR-" + mockUUID + ".json";

    PowerMockito.mockStatic(UUID.class);
    when(UUID.randomUUID()).thenReturn(mockUUID);

    PowerMockito.mockStatic(ActionRepo.class);
    ActionRepo mockRepo = mock(ActionRepo.class);
    when(ActionRepo.getInstance()).thenReturn(mockRepo);
    when(mockRepo.getLogFileDirectory()).thenReturn(mockDir);

    PowerMockito.mockStatic(ApplicationUtils.class);
    PowerMockito.doNothing()
        .when(ApplicationUtils.class, "saveDataToFile", requestBody, expectedFileName);

    ResponseEntity<String> response = controller.receiveEICR(requestBody);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    JSONObject responseBody = new JSONObject(response.getBody());
    assertTrue(responseBody.getString("message").contains("Success!"));

    PowerMockito.verifyStatic(ActionRepo.class, times(1));
    ActionRepo.getInstance();
  }
}
