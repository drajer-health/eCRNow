package com.drajer.bsa.auth;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.model.HealthcareSetting;
import org.junit.Test;

public class EhrTokenStorageModeTest {

  @Test
  public void memory_andDatabase_areRecognized_ignoringCaseAndSpaces() {
    assertTrue(new EhrTokenStorageMode("memory").isInMemory());
    assertTrue(new EhrTokenStorageMode(" Memory ").isInMemory());
    assertTrue(new EhrTokenStorageMode("database").isDatabase());
    assertTrue(new EhrTokenStorageMode("DATABASE").isDatabase());
    assertFalse(new EhrTokenStorageMode("database").isInMemory());
  }

  @Test
  public void unknownValue_failsAtStartup() {
    assertThrows(IllegalStateException.class, () -> new EhrTokenStorageMode("redis"));
    assertThrows(IllegalStateException.class, () -> new EhrTokenStorageMode(""));
    assertThrows(IllegalStateException.class, () -> new EhrTokenStorageMode(null));
  }

  @Test
  public void factories() {
    assertEquals(EhrTokenStorageMode.Storage.MEMORY, EhrTokenStorageMode.memory().storage());
    assertEquals(EhrTokenStorageMode.Storage.DATABASE, EhrTokenStorageMode.database().storage());
  }

  @Test
  public void keepTokenChangesOutOfDatabase_withoutTransaction_doesNothing() {
    HealthcareSettingsDao dao = mock(HealthcareSettingsDao.class);

    EhrTokenStorageMode.keepTokenChangesOutOfDatabase(dao, new HealthcareSetting());
    EhrTokenStorageMode.keepTokenChangesOutOfDatabase(null, new HealthcareSetting());
    EhrTokenStorageMode.keepTokenChangesOutOfDatabase(dao, null);

    verify(dao, never()).markReadOnly(org.mockito.ArgumentMatchers.any());
  }

  @Test
  public void keepTokenChangesOutOfDatabase_inTransaction_marksTheSettingReadOnly() {
    HealthcareSettingsDao dao = mock(HealthcareSettingsDao.class);
    HealthcareSetting hs = new HealthcareSetting();
    org.springframework.transaction.support.TransactionSynchronizationManager
        .setActualTransactionActive(true);
    try {
      EhrTokenStorageMode.keepTokenChangesOutOfDatabase(dao, hs);
    } finally {
      org.springframework.transaction.support.TransactionSynchronizationManager
          .setActualTransactionActive(false);
    }
    verify(dao).markReadOnly(hs);
  }
}
