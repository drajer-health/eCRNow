# Logback Archive Retention Configuration Guide

## Overview

The eCRNow application uses **Logback** (via Spring Boot's `logback-spring.xml`) to manage application logging. By default, rolling log files are archived daily. This guide walks through how to customize the number of days that archived log files are retained and how to cap total archive disk usage.

---

## Current Default Configuration

The active logback configuration file is located at:

```
src/main/resources/logback-spring.xml
```

Its current contents delegate log file behavior to Spring Boot's built-in defaults:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<configuration>
    <include resource="org/springframework/boot/logging/logback/defaults.xml" />
    <property name="LOG_FILE" value="${LOG_FILE:-${LOG_PATH:-${LOG_TEMP:-${java.io.tmpdir:-/tmp}}/}spring.log}"/>
    <include resource="org/springframework/boot/logging/logback/file-appender.xml" />
    <root level="INFO">
        <appender-ref ref="FILE" />
    </root>
</configuration>
```

Spring Boot's included `file-appender.xml` uses a `RollingFileAppender` with a `TimeBasedRollingPolicy`. The default retention is **7 days** (`maxHistory=7`).

---

## How to Customize Archive Retention

To override the default and control how many days of archived log files are kept, replace the `<include>` reference to `file-appender.xml` with an explicit `RollingFileAppender` definition directly in `logback-spring.xml`.

### Step 1 — Open the configuration file

```
src/main/resources/logback-spring.xml
```

### Step 2 — Replace the default file-appender include with a custom appender

Below is a full example that keeps **30 days** of archived logs and caps total archive size at **1 GB**. Adjust the values in the highlighted properties to match your environment's retention requirements.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<configuration>
    <include resource="org/springframework/boot/logging/logback/defaults.xml" />

    <!-- Define the active log file location -->
    <property name="LOG_FILE" value="${LOG_FILE:-${LOG_PATH:-${LOG_TEMP:-${java.io.tmpdir:-/tmp}}/}spring.log}"/>

    <appender name="FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">

        <!-- Path to the current (active) log file -->
        <file>${LOG_FILE}</file>

        <encoder>
            <pattern>${FILE_LOG_PATTERN}</pattern>            
        </encoder>

        <rollingPolicy class="ch.qos.logback.core.rolling.TimeBasedRollingPolicy">

            <!-- Archive file naming pattern — rolls over daily -->
            <fileNamePattern>${LOG_FILE}.%d{yyyy-MM-dd}.gz</fileNamePattern>

            <!--
                MAX HISTORY (days to retain)
                ════════════════════════════
                Controls the total number of archive files (one per day) to keep.
                Files older than this number of days are automatically deleted
                during the next rollover.

                Default (Spring Boot): 7
                Change this value to match your retention policy.
            -->
            <maxHistory>30</maxHistory>

            <!--
                TOTAL SIZE CAP (optional disk usage ceiling)
                ════════════════════════════════════════════
                Enforces an upper bound on the combined size of ALL archive files.
                If the cap is reached, the oldest archives are removed even if
                maxHistory has not been exceeded yet.

                Common values: 500MB, 1GB, 5GB
                Remove this element entirely to disable the cap.
            -->
            <!-- <totalSizeCap>1GB</totalSizeCap> -->

        </rollingPolicy>
    </appender>

    <root level="INFO">
        <appender-ref ref="FILE" />
    </root>
</configuration>
```

---

## Key Configuration Properties

| Property | XML Element | Description | Default |
|---|---|---|---|
| Days to retain archives | `<maxHistory>` | Number of daily archive files to keep before the oldest are purged | `7` |
| Total archive disk cap | `<totalSizeCap>` | Maximum combined size of all archive files on disk | Unlimited |
| Archive filename pattern | `<fileNamePattern>` | Naming convention and rollover schedule for archive files | Daily (`.%d{yyyy-MM-dd}`) |
| Active log file path | `<file>` | Path to the current log file being written | Resolved from `LOG_FILE` / `LOG_PATH` env vars |

### `<maxHistory>` Details

- Value is an **integer** representing **calendar days**.
- At each rollover (midnight by default), Logback checks the archive directory and deletes files older than `maxHistory` days.
- Example values:
  - `7` — one week (Spring Boot default)
  - `30` — one month
  - `90` — three months
  - `365` — one year

### `<totalSizeCap>` Details

- Accepted size suffixes: `KB`, `MB`, `GB`.
- Acts as a **secondary** pruning rule alongside `maxHistory`. If total archive size exceeds the cap, the oldest archived files are removed first regardless of age.
- This property requires Logback **1.1.8 or later** (included in Spring Boot 2.x and 3.x).

---

## Controlling Log File Path via Environment Variables

The log file location is driven by environment variables or JVM system properties, checked in the following order of precedence:

| Variable | Description |
|---|---|
| `LOG_FILE` | Full path (including filename) to the active log file |
| `LOG_PATH` | Directory path; the filename defaults to `spring.log` |
| `LOG_TEMP` | Fallback temp directory |
| `java.io.tmpdir` | JVM temp dir (last resort) |

**Example — set log path in `application.properties`:**
```properties
logging.file.path=/opt/ecrnow/logs
```

**Example — set explicit log file name:**
```properties
logging.file.name=/opt/ecrnow/logs/ecrnow-app.log
```

When `logging.file.name` is set, the archive pattern in `logback-spring.xml` will produce files such as:
```
/opt/ecrnow/logs/ecrnow-app.log.2025-06-28.gz
/opt/ecrnow/logs/ecrnow-app.log.2025-06-29.gz
```

---

## Applying the Change

1. Edit `src/main/resources/logback-spring.xml` with the custom appender shown above.
2. Set `<maxHistory>` to your desired number of days.
3. Optionally set `<totalSizeCap>` to prevent runaway disk usage.
4. Rebuild and redeploy the application JAR or Docker container. Logback reads the configuration at startup — **no additional restart or reload step** is needed beyond the normal deployment process.

> **Note:** If the application is deployed via Docker, ensure the log directory is mounted as a volume so that archive files persist across container restarts. Without a volume mount, all archived logs are lost when the container is recreated.

---

## Troubleshooting

| Symptom | Likely Cause | Resolution |
|---|---|---|
| Old archives not being deleted | `maxHistory` not set or misconfigured | Verify the `<maxHistory>` element is inside `<rollingPolicy>`, not inside `<appender>` |
| All logs going to `/tmp` | `LOG_FILE` / `LOG_PATH` not set | Set `logging.file.name` or `logging.file.path` in `application.properties` |
| `totalSizeCap` has no effect | Logback version too old | Confirm Spring Boot version is 2.x or 3.x (Logback ≥ 1.1.8) |
| Configuration changes not applied | Cached build artifact | Clean and rebuild: `mvn clean package` |

---

## References

- [Logback RollingFileAppender Documentation](https://logback.qos.ch/manual/appenders.html#RollingFileAppender)
- [Spring Boot Logging Reference](https://docs.spring.io/spring-boot/docs/current/reference/html/features.html#features.logging)
- eCRNow active logback config: `src/main/resources/logback-spring.xml`
