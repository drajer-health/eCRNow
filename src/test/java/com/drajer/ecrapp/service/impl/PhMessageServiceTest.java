package com.drajer.ecrapp.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.ecrapp.dao.PhMessageDao;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;

@ExtendWith(MockitoExtension.class)
public class PhMessageServiceTest {

	@InjectMocks
	PhMessageServiceImpl phMessageServiceImpl;

	@Mock
	PhMessageDao phMessageDao;

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
	public void testGetPhMessageData() throws IOException {

		Mockito.lenient().when(phMessageDao.getPhMessageData(searchParam))
				.thenReturn(exceptedPublicHealthMessageDetails);

		List<PublicHealthMessage> result = phMessageServiceImpl.getPhMessageData(searchParam);

		assertEquals(exceptedPublicHealthMessageDetails, result);

	}
}
