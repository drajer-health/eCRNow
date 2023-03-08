package com.drajer.sof.launch;

import com.drajer.eca.model.ActionRepo;
import com.drajer.ecrapp.util.ApplicationUtils;
import java.util.UUID;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class SimulatorController {

  private final Logger logger = LoggerFactory.getLogger(SimulatorController.class);

  @CrossOrigin
  @PostMapping(value = "/api/eicrReceiver/simulator")
  public ResponseEntity<String> receiveEICR(@RequestBody String request) {
    logger.info("Received EICR");
    String fileName =
        ActionRepo.getInstance().getLogFileDirectory()
            + "/ReceivedEICR-"
            + UUID.randomUUID().toString()
            + ".json";
    ApplicationUtils.saveDataToFile(request, fileName);

    JSONObject response = new JSONObject();
    response.put("message", "Success! The request is saved to file:::::" + fileName);

    return new ResponseEntity<>(response.toString(), HttpStatus.OK);
  }
}
