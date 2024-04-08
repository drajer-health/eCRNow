package com.drajer.sof.launch;

import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.ClientDetailsDTO;
import com.drajer.sof.service.ClientDetailsService;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.text.StringEscapeUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
public class ClientDetailsController {

  public static final String ERROR_IN_PROCESSING_THE_REQUEST = "Error in Processing the Request";

  @Autowired ClientDetailsService clientDetailsService;

  private final Logger logger = LoggerFactory.getLogger(ClientDetailsController.class);

  @CrossOrigin
  @GetMapping("/api/clientDetails/{clientId}")
  public ClientDetails getClientDetailsById(@PathVariable("clientId") Integer clientId) {
    return clientDetailsService.getClientDetailsById(clientId);
  }

  // POST method to create a Client
  @CrossOrigin
  @PostMapping(value = "/api/clientDetails")
  public ResponseEntity<Object> createClientDetails(
      @RequestBody ClientDetailsDTO clientDetailsDTO) {
    ClientDetails checkClientDetails =
        clientDetailsService.getClientDetailsByUrl(clientDetailsDTO.getFhirServerBaseURL());
    if (checkClientDetails == null) {
      logger.info("Adding the Client Details");
      ClientDetails newClientDetails = new ClientDetails();

      if (clientDetailsDTO.getIsEmergentReportingEnabled() == null
          && clientDetailsDTO.getIsCovid() != null) {
        clientDetailsDTO.setIsEmergentReportingEnabled(clientDetailsDTO.getIsCovid());
      }

      BeanUtils.copyProperties(clientDetailsDTO, newClientDetails);
      clientDetailsService.saveOrUpdate(newClientDetails);
      BeanUtils.copyProperties(newClientDetails, clientDetailsDTO);
      return new ResponseEntity<>(clientDetailsDTO, HttpStatus.OK);
    } else {
      logger.error("FHIR Server URL is already registered");
      JSONObject responseObject = new JSONObject();
      responseObject.put("status", "error");
      responseObject.put("message", "URL is already registered");
      return new ResponseEntity<>(responseObject, HttpStatus.CONFLICT);
    }
  }

  @CrossOrigin
  @PutMapping(value = "/api/clientDetails")
  public ResponseEntity<Object> updateClientDetails(
      @RequestBody ClientDetailsDTO clientDetailsDTO) {
    ClientDetails checkClientDetails =
        clientDetailsService.getClientDetailsByUrl(clientDetailsDTO.getFhirServerBaseURL());
    if (checkClientDetails == null
        || (checkClientDetails.getId().equals(clientDetailsDTO.getId()))) {

      if (clientDetailsDTO.getIsEmergentReportingEnabled() == null
          && clientDetailsDTO.getIsCovid() != null) {
        clientDetailsDTO.setIsEmergentReportingEnabled(clientDetailsDTO.getIsCovid());
      }
      logger.info("Saving the Client Details");
      ClientDetails updateClientDetails = new ClientDetails();

      if (clientDetailsDTO.getIsEmergentReportingEnabled() == null
          && clientDetailsDTO.getIsCovid() != null) {
        clientDetailsDTO.setIsEmergentReportingEnabled(clientDetailsDTO.getIsCovid());
      }

      BeanUtils.copyProperties(clientDetailsDTO, updateClientDetails);
      clientDetailsService.saveOrUpdate(updateClientDetails);
      BeanUtils.copyProperties(updateClientDetails, clientDetailsDTO);
      return new ResponseEntity<>(clientDetailsDTO, HttpStatus.OK);
    } else {
      logger.error("FHIR Server URL is already registered");
      JSONObject responseObject = new JSONObject();
      responseObject.put("status", "error");
      responseObject.put("message", "URL is already registered");
      return new ResponseEntity<>(responseObject, HttpStatus.CONFLICT);
    }
  }

  @CrossOrigin
  @GetMapping("/api/clientDetails")
  public ClientDetails getClientDetailsByUrl(@RequestParam(value = "url") String url) {
    return clientDetailsService.getClientDetailsByUrl(url);
  }

  @CrossOrigin
  @GetMapping("/api/clientDetails/")
  public List<ClientDetails> getAllClientDetails() {
    return clientDetailsService.getAllClientDetails();
  }

  @CrossOrigin
  @DeleteMapping(value = "/api/clientDetails")
  public ResponseEntity<String> deleteClientDetails(
      @RequestParam(value = "url") String url,
      @RequestHeader(name = "X-Request-ID") String xRequestIdHttpHeaderValue,
      @RequestHeader(name = "X-Correlation-ID", required = false)
          String xCorrelationIdHttpHeaderValue,
      HttpServletRequest request,
      HttpServletResponse response) {
    try {
      logger.info(
          "X-Request-ID: {} and X-Correlation-ID: {} received for deleting clientDetail",
          StringEscapeUtils.escapeJava(xRequestIdHttpHeaderValue),
          StringEscapeUtils.escapeJava(xCorrelationIdHttpHeaderValue));

      if (url == null || url.isEmpty()) {
        return new ResponseEntity<>(
            "Requested FHIR Url is missing or empty", HttpStatus.BAD_REQUEST);
      }
      ClientDetails checkClientDetails = clientDetailsService.getClientDetailsByUrl(url);
      if (checkClientDetails != null) {
        clientDetailsService.delete(checkClientDetails);
        return new ResponseEntity<>("ClientDetails deleted successfully", HttpStatus.OK);
      }
      return new ResponseEntity<>("Client Details Not found", HttpStatus.NOT_FOUND);
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      return new ResponseEntity<>(
          ERROR_IN_PROCESSING_THE_REQUEST, HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }
}
