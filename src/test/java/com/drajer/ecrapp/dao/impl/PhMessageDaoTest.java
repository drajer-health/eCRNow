package com.drajer.ecrapp.dao.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertNotNull;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import io.jsonwebtoken.io.IOException;
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
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;

@RunWith(SpringRunner.class)
@ContextConfiguration(classes = SpringConfiguration.class)
@AutoConfigureTestDatabase
@SpringBootTest
@ActiveProfiles("test")
@Transactional
public class PhMessageDaoTest {

  @Autowired PhMessageDaoImpl phMessageDaoImpl;

  private Map<String, String> searchParam;

  private PublicHealthMessage expectedPublicHealthMessage;

  @Before
  public void setUp() throws IOException {

    expectedPublicHealthMessage =
        (PublicHealthMessage)
            TestUtils.readFileContents(
                "ecrTestData/phMessageInput/PublicHealthMessage.json",
                new TypeReference<PublicHealthMessage>() {});

    searchParam =
        (Map<String, String>)
            TestUtils.readFileContents(
                "ecrTestData/phMessageInput/searchParam.json",
                new TypeReference<Map<String, String>>() {});
  }

  @Test
  public void testGetPhMessageData() {

    PublicHealthMessage savedPublicHealthMessage =
        phMessageDaoImpl.saveOrUpdate(expectedPublicHealthMessage);
    assertNotNull(savedPublicHealthMessage);

    List<PublicHealthMessage> result = phMessageDaoImpl.getPhMessageData(searchParam);
    assertThat(result).isNotEmpty();
  }
}
