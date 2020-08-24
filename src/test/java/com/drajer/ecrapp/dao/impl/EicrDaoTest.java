package com.drajer.ecrapp.dao.impl;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.io.IOException;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@RunWith(SpringRunner.class)
@ContextConfiguration(classes = SpringConfiguration.class)
@AutoConfigureTestDatabase
@SpringBootTest
@ActiveProfiles("test")
@Transactional
public class EicrDaoTest {
	
	@Autowired
	private EicrDaoImpl eicrDaoImpl;
	
	ObjectMapper mapper = new ObjectMapper();
	
	@Test
	public void saveOrUpdateEicr() throws JsonParseException, JsonMappingException, IOException
	{
		File jsonFile = new File(getClass().getClassLoader().getResource("eicr.json").getFile());
		Eicr eicr = mapper.readValue(jsonFile, Eicr.class);
		
		Eicr savedEicr = eicrDaoImpl.saveOrUpdate(eicr);
		
		assertEquals(eicr.getData(), savedEicr.getData());
	}
	
	@Test
	public void getEicrById() throws JsonParseException, JsonMappingException, IOException
	{
		File jsonFile = new File(getClass().getClassLoader().getResource("eicr.json").getFile());
		Eicr eicr = mapper.readValue(jsonFile, Eicr.class);
		
		Eicr savedEicr = eicrDaoImpl.saveOrUpdate(eicr);
		Eicr retrievedEicr = eicrDaoImpl.getEicrById(savedEicr.getId());
		
		assertNotNull(retrievedEicr);	
	}
	
	@Test
	public void saveOrUpdateReportabilityResponse() throws JsonParseException, JsonMappingException, IOException
	{
		File jsonFile = new File(getClass().getClassLoader().getResource("reportabilityResponse.json").getFile());
		ReportabilityResponse rr = mapper.readValue(jsonFile, ReportabilityResponse.class);
		
		ReportabilityResponse savedRR = eicrDaoImpl.saveOrUpdate(rr);
		
		assertEquals(rr.getData(), savedRR.getData());
	}
	
	@Test
	public void getReportabilityResponseById() throws JsonParseException, JsonMappingException, IOException
	{
		File jsonFile = new File(getClass().getClassLoader().getResource("reportabilityResponse.json").getFile());
		ReportabilityResponse rr = mapper.readValue(jsonFile, ReportabilityResponse.class);
		
		ReportabilityResponse savedRR = eicrDaoImpl.saveOrUpdate(rr);
		ReportabilityResponse retrievedRR = eicrDaoImpl.getRRById(savedRR.getId());
		
		assertNotNull(retrievedRR);	
	}

}
