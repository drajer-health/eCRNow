package com.drajer.ecrapp.dao.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.jdbc.Sql.ExecutionPhase;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;

@RunWith(SpringRunner.class)
@ContextConfiguration(classes = SpringConfiguration.class)
@AutoConfigureTestDatabase
@SpringBootTest
@ActiveProfiles("test")
@Transactional
public class PhMessageDaoTest {

	@Autowired
	PhMessageDaoImpl phMessageDaoImpl;

	private Map<String, String> searchParam;

	private List<PublicHealthMessage> exceptedPublicHealthMessageDetails;

	@BeforeEach
	public void setUp() throws IOException {

		exceptedPublicHealthMessageDetails = (List<PublicHealthMessage>) TestUtils.readFileContents(
				"ecrTestData/PhMessageOutput/PhMessage.json", new TypeReference<List<PublicHealthMessage>>() {
				});

		searchParam = (Map<String, String>) TestUtils.readFileContents("ecrTestData/phMessageInput/searchParam.json",
				new TypeReference<Map<String, String>>() {
				});

	}

	@Test
	@Sql(executionPhase = ExecutionPhase.BEFORE_TEST_METHOD, scripts = "classpath:ecrTestData/sql/Insert_PhMessager.sql")
	void testGetPhMessageData() {

		List<PublicHealthMessage> result = phMessageDaoImpl.getPhMessageData(searchParam);

		assertThat(result.size()).isEqualTo(exceptedPublicHealthMessageDetails.size());
		assertEquals(TestUtils.toJsonString(exceptedPublicHealthMessageDetails), TestUtils.toJsonString(result));

	}
}