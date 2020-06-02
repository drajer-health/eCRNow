package com.drajer.sof.utils;

import java.util.HashMap;
import java.util.Map;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.Response;
import com.drajer.sof.service.ClientDetailsService;

@Component
public class Authorization {

	@Autowired
	ClientDetailsService clientDetailService;
	
	private final Logger logger = LoggerFactory.getLogger(Authorization.class);

	public JSONObject getMetadata(String serverURL) {
		HttpHeaders headers = new HttpHeaders();
		headers.set("Accept", MediaType.APPLICATION_JSON_VALUE);
		HttpEntity<?> entity = new HttpEntity(headers);
		RestTemplate restTemplate = new RestTemplate();
		ResponseEntity<String> response = null;
		JSONObject metadata = null;
		try {
			logger.info("Getting Metadata information from URL:::::"+serverURL);
			response = restTemplate.exchange(serverURL, HttpMethod.GET, entity, String.class);	
			metadata = new JSONObject(response.getBody());
			logger.info("Received Metadata Information from URL:::::"+serverURL);
		}catch(Exception e) {
			logger.error("Error in getting Metadata information for URL:::::"+serverURL);
		}
		return metadata;
	}

	public String createAuthUrl(JSONObject authDetailsObject,ClientDetails clientDetails, Integer state) {
		// TODO Auto-generated method stub
		String authUrl = authDetailsObject.getString("authorizeUrl");
		Map<String, String> params = new HashMap<String, String>();
		params.put("response_type", "code");
		params.put("client_id", clientDetails.getClientId());
		params.put("redirect_uri", authDetailsObject.getString("redirectUrl"));
		params.put("launch", authDetailsObject.getString("launch"));
		params.put("state", Integer.toString(state));
		params.put("scope", clientDetails.getScopes());
		params.put("aud", authDetailsObject.getString("iss"));
		String queryParams = formatQueryParams(params);
		return authUrl + queryParams;
	}

	protected String formatQueryParams(Map<String, String> params) {
		return params.entrySet().stream().map(p -> p.getKey() + "=" + p.getValue()).reduce((p1, p2) -> p1 + "&" + p2)
				.map(s -> "?" + s).orElse("");
	}
	
	public JSONObject getAccessToken(JSONObject tokenDetails) {
		JSONObject tokenResponse = null;
		logger.info("Getting AccessToken for Client: " + tokenDetails.getString("client_id"));
		try {
			RestTemplate restTemplate = new RestTemplate();
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
			headers.add("Accept",MediaType.APPLICATION_JSON_VALUE);
			MultiValueMap<String, String> map = new LinkedMultiValueMap<>();
			map.add("grant_type", "authorization_code");
			map.add("code", tokenDetails.getString("code"));
			map.add("redirect_uri",tokenDetails.getString("redirectUrl"));
			map.add("client_id", tokenDetails.getString("client_id"));
			HttpEntity<MultiValueMap<String, String>> entity = new HttpEntity<>(map, headers);
			ResponseEntity<?> response = restTemplate.exchange(tokenDetails.getString("tokenUrl"), HttpMethod.POST, entity,
					Response.class);
			tokenResponse = new JSONObject(response.getBody());
			logger.info("Received AccessToken for Client: " + tokenDetails.getString("client_id"));
			logger.info("Received AccessToken: " + tokenResponse);

		} catch (Exception e) {
			logger.error("Error in Getting the AccessToken for the client: " + tokenDetails.getString("client_id"));
		}
		return tokenResponse;
	}

}
