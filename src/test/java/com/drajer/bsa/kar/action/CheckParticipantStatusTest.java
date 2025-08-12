package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
public class CheckParticipantStatusTest {

  @InjectMocks CheckParticipantStatus checkParticipantStatus;

  @Test
  public void testSetAndGetParticipantMatchStatus() {

    checkParticipantStatus.setParticipantMatchStatus(true);

    assertTrue(checkParticipantStatus.getParticipantMatchStatus());

    checkParticipantStatus.setParticipantMatchStatus(false);
    assertFalse(checkParticipantStatus.getParticipantMatchStatus());
  }
}
