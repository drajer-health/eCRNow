package com.drajer.ecrapp.config;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Initializes all _v2 database sequences on application startup.
 *
 * <p>This ensures Hibernate's pooled-lo optimizer (allocationSize=50) fetches the current sequence
 * value from the database before any inserts, preventing duplicate key errors after a sequence fix
 * or database migration.
 *
 * <p>Runs once after the SessionFactory is fully built.
 */
@Component
@DependsOn("entityManagerFactory")
public class SequenceInitializer implements InitializingBean {

  private static final Logger logger = LoggerFactory.getLogger(SequenceInitializer.class);

  /** All _v2 sequences used by Hibernate entities (allocationSize=50). */
  private static final List<String> SEQUENCES =
      Arrays.asList(
          "client_details_v2_SEQ",
          "launch_details_v2_SEQ",
          "eicr_v2_SEQ",
          "healthcare_setting_v2_SEQ",
          "kar_repos_v2_SEQ",
          "kar_info_v2_SEQ",
          "hs_kar_status_v2_SEQ",
          "public_health_authority_v2_SEQ");

  private final SessionFactory sessionFactory;

  private enum DatabaseType {
    SQLSERVER,
    POSTGRESQL,
    UNKNOWN
  }

  public SequenceInitializer(SessionFactory sessionFactory) {
    this.sessionFactory = sessionFactory;
  }

  @Override
  public void afterPropertiesSet() {

    logger.info("SequenceInitializer: warming up {} sequences...", SEQUENCES.size());

    int successCount = 0;
    int failCount = 0;

    try (Session session = sessionFactory.openSession()) {
      DatabaseType databaseType = detectDatabaseType(session);
      logger.info("SequenceInitializer: detected database type={}", databaseType);

      if (databaseType == DatabaseType.UNKNOWN) {
        logger.warn("SequenceInitializer: unsupported database type. Skipping sequence warmup.");
        return;
      }

      for (String seqName : SEQUENCES) {
        try {
          String query = buildNextValueSql(databaseType, seqName);
          Object value = session.createNativeQuery(query, Object.class).getSingleResult();
          logger.debug("  [OK] {} = {}", seqName, value);
          successCount++;
        } catch (Exception e) {
          failCount++;
          logger.error("  [FAIL] {} - {}", seqName, e.getMessage());
        }
      }
    } catch (Exception e) {
      logger.error("SequenceInitializer: failed to open session - {}", e.getMessage(), e);
      return;
    }

    logger.info(
        "SequenceInitializer: complete. success={}, failed={}, total={}",
        successCount,
        failCount,
        SEQUENCES.size());

    if (failCount > 0) {
      logger.warn(
          "SequenceInitializer: {} sequence(s) failed. "
              + "Check that the sequences exist and INCREMENT BY is 50. ",
          failCount);
    }
  }

  private DatabaseType detectDatabaseType(Session session) {
    try {
      String productName =
          session.doReturningWork(
              connection ->
                  connection.getMetaData().getDatabaseProductName().toLowerCase(Locale.ROOT));

      if (productName.contains("postgresql")) {
        return DatabaseType.POSTGRESQL;
      }

      if (productName.contains("sql server") || productName.contains("microsoft")) {
        return DatabaseType.SQLSERVER;
      }
    } catch (Exception e) {
      logger.warn("SequenceInitializer: unable to detect database type - {}", e.getMessage());
    }

    return DatabaseType.UNKNOWN;
  }

  private String buildNextValueSql(DatabaseType databaseType, String sequenceName) {
    if (databaseType == DatabaseType.POSTGRESQL) {
      return "SELECT nextval('public." + sequenceName.toLowerCase(Locale.ROOT) + "')";
    }

    return "SELECT NEXT VALUE FOR dbo." + sequenceName;
  }
}
