package com.drajer.ecrapp.dao.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.util.List;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
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

@RunWith(SpringRunner.class)
@ContextConfiguration(classes = SpringConfiguration.class)
@AutoConfigureTestDatabase
@SpringBootTest
@ActiveProfiles("test")
@Transactional
public class PhMessageDaoTest {

  private static final String PH_MESSAGE_FILE = "ecrTestData/PhMessageOutput/PhMessage.json";
  private static final String SEARCH_PARAM_FILE = "ecrTestData/phMessageInput/searchParam.json";

  @Autowired private PhMessageDaoImpl phMessageDao;

  private Map<String, String> searchParams;

  private List<PublicHealthMessage> expectedPublicHealthMessages;

  @Before
  public void setUp() {

    expectedPublicHealthMessages =
        TestUtils.readFileContents(
            PH_MESSAGE_FILE, new TypeReference<List<PublicHealthMessage>>() {});
    searchParams =
        TestUtils.readFileContents(SEARCH_PARAM_FILE, new TypeReference<Map<String, String>>() {});
  }

  @Test
  @Sql(
      executionPhase = ExecutionPhase.BEFORE_TEST_METHOD,
      scripts = "classpath:ecrTestData/sql/Insert_PhMessage.sql")
  public void testGetPhMessageData() {

    List<PublicHealthMessage> actualPublicHealthMessages =
        phMessageDao.getPhMessageData(searchParams);

    assertThat(actualPublicHealthMessages).hasSize(expectedPublicHealthMessages.size());

    assertEquals(
        TestUtils.toJsonString(expectedPublicHealthMessages),
        TestUtils.toJsonString(actualPublicHealthMessages));
  }
}
