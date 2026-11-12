package com.drajer.bsa.auth;

import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.model.HealthcareSetting;
import java.util.Locale;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronizationManager;

/**
 * Selects how EHR access tokens are generated, stored and read, with {@value #PROPERTY}. The two
 * modes are independent; each token path in the application checks the mode and runs exactly one of
 * them.
 *
 * <ul>
 *   <li>{@code database} (default): the original implementation. The token is kept on the
 *       HealthcareSetting entity, reused while it is valid, and a new token is saved with {@code
 *       saveOrUpdate} in the job's transaction, committed with the job. {@link EhrAccessTokenCache}
 *       is not used.
 *   <li>{@code memory}: tokens are generated, stored and read only through {@link
 *       EhrAccessTokenCache}. No token is read from or written to the database. The
 *       HealthcareSetting is marked read-only in the current session before the cache is used, so a
 *       provider id taken from the token response can never be written to the database either.
 * </ul>
 *
 * <p>An unknown value stops the application at startup.
 */
@Component
public class EhrTokenStorageMode {

  private static final Logger logger = LoggerFactory.getLogger(EhrTokenStorageMode.class);

  public static final String PROPERTY = "ecr.ehr.token.store";
  public static final String MEMORY = "memory";
  public static final String DATABASE = "database";

  public enum Storage {
    MEMORY,
    DATABASE
  }

  private final Storage storage;

  @Autowired
  public EhrTokenStorageMode(@Value("${" + PROPERTY + ":" + DATABASE + "}") String value) {
    this.storage = parse(value);
    logger.info(
        "EHR access token storage: {}={}", PROPERTY, storage.name().toLowerCase(Locale.ROOT));
  }

  private EhrTokenStorageMode(Storage storage) {
    this.storage = storage;
  }

  /** In-memory mode, for code and tests created without Spring. */
  public static EhrTokenStorageMode memory() {
    return new EhrTokenStorageMode(Storage.MEMORY);
  }

  /** Database mode (the original implementation), for code and tests created without Spring. */
  public static EhrTokenStorageMode database() {
    return new EhrTokenStorageMode(Storage.DATABASE);
  }

  static Storage parse(String value) {
    String mode = value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
    switch (mode) {
      case MEMORY:
        return Storage.MEMORY;
      case DATABASE:
        return Storage.DATABASE;
      default:
        throw new IllegalStateException(
            "Unknown "
                + PROPERTY
                + "='"
                + value
                + "'. Use '"
                + MEMORY
                + "' or '"
                + DATABASE
                + "'.");
    }
  }

  public Storage storage() {
    return storage;
  }

  public boolean isInMemory() {
    return storage == Storage.MEMORY;
  }

  public boolean isDatabase() {
    return storage == Storage.DATABASE;
  }

  /**
   * In-memory mode only: marks the HealthcareSetting read-only in the current Hibernate session, so
   * that nothing the token cache sets on it (the provider id from a token response) is ever written
   * to the database. This sets a flag in the session and runs no SQL. Without an active transaction
   * there is no session that could write the entity, so nothing is done.
   */
  public static void keepTokenChangesOutOfDatabase(
      HealthcareSettingsDao healthcareSettingsDao, HealthcareSetting hs) {
    if (healthcareSettingsDao != null
        && hs != null
        && TransactionSynchronizationManager.isActualTransactionActive()) {
      healthcareSettingsDao.markReadOnly(hs);
    }
  }
}
