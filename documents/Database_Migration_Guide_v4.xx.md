# eCRNow Database Migration Guide — v3.x → v4.x (Production)

**Document Date:** May 1, 2026  
**Application:** eCRNow | **Target Version:** 4.x  
**Supported Databases:** PostgreSQL | Microsoft SQL Server

---

## Table of Contents

1. [Overview](#1-overview)
2. [Architecture: What Changes in 4.x](#2-architecture-what-changes-in-4x)
3. [Pre-Migration Checklist](#3-pre-migration-checklist)
4. [Migration Path Decision Table](#4-migration-path-decision-table)
5. [Phase 1 — Incremental 3.x Upgrade (If Needed)](#5-phase-1--incremental-3x-upgrade-if-needed)
6. [Phase 2 — 4.x Core Migration (All Environments)](#6-phase-2--4x-core-migration-all-environments)
7. [Phase 3 — Sequence Synchronization (PostgreSQL Only)](#7-phase-3--sequence-synchronization-postgresql-only)
8. [Execution Instructions by Database Tool](#8-execution-instructions-by-database-tool)
9. [Post-Migration Validation Checklist](#9-post-migration-validation-checklist)
10. [Failure Handling & Rollback Plan](#10-failure-handling--rollback-plan)
11. [Best Practices for Production](#11-best-practices-for-production)
12. [Quick Reference: Full Script Execution Order](#12-quick-reference-full-script-execution-order)

---

## 1. Overview

The 4.x release of eCRNow introduces a **renamed table architecture**. All core tables are migrated to `_v2` equivalents (e.g., `healthcare_setting` → `healthcare_setting_v2`). The migration scripts are **fully idempotent** — they are safe to run multiple times and will skip steps already completed.

**Original tables are NOT dropped automatically** — the application must be configured to use the new `_v2` tables before cutover.

---

## 2. Architecture: What Changes in 4.x

| Old Table (3.x) | New Table (4.x) | New Sequence |
|---|---|---|
| `healthcare_setting` | `healthcare_setting_v2` | `healthcare_setting_v2_seq` |
| `eicr` | `eicr_v2` | `eicr_v2_seq` |
| `launch_details` | `launch_details_v2` | `launch_details_v2_seq` |
| `kar_info` | `kar_info_v2` | `kar_info_v2_seq` |
| `kar_repos` | `kar_repos_v2` | `kar_repos_v2_seq` |
| `notification_context` | `notification_context_v2` | `notification_context_v2_seq` |
| `ph_messages` | `ph_messages_v2` | `ph_messages_v2_seq` |
| `public_health_authority` | `public_health_authority_v2` | `public_health_authority_v2_seq` |
| `client_details` | `client_details_v2` | `client_details_v2_seq` |
| `hs_kar_status` | `hs_kar_status_v2` | *(composite PK — no seq)* |
| `kar_execution_state` | `kar_execution_state_v2` | *(composite PK — no seq)* |


---

## 3. Pre-Migration Checklist

Complete **every item** before starting. Do not proceed if any item is unresolved.

```
[ ] 1. BACKUP — Take a full verified database backup
[ ] 2. STOP SERVICES — Stop all eCRNow application instances
[ ] 3. VALIDATE IN QA — Run all scripts in a lower environment first
[ ] 4. CHECK DB USER PRIVILEGES — User must have:
         CREATE, ALTER, INSERT, UPDATE, DELETE, EXECUTE (procedures/functions)
[ ] 5. NOTE CURRENT APP VERSION — Determine if version is < 3.1.9 or >= 3.1.9
[ ] 6. RECORD BASELINE — Count rows in key tables before migration:
         SELECT COUNT(*) FROM healthcare_setting;
         SELECT COUNT(*) FROM eicr;
         SELECT COUNT(*) FROM launch_details;
         SELECT COUNT(*) FROM kar_info;
         SELECT COUNT(*) FROM notification_context;
         SELECT COUNT(*) FROM ph_messages;
[ ] 7. CONFIRM MAINTENANCE WINDOW — Allocate adequate downtime
```

---

## 4. Migration Path Decision Table

| Current App Version | Action Required |
|---|---|
| **< 3.1.9** | Run **Phase 1** (incremental 3.x scripts) → then **Phase 2 + Phase 3** |
| **= 3.1.9** | Skip Phase 1 → Run **Phase 2 + Phase 3** |
| **> 3.1.9 (3.x)** | Skip Phase 1 → Run **Phase 2 + Phase 3** |
| **Fresh / New Install** | Skip Phase 1 → Run **Phase 2 + Phase 3** (scripts handle missing old tables) |

> **Note:** All 4.x scripts detect whether old tables exist. If an old table is absent (fresh install), the script creates the new `_v2` table directly and exits — no error is raised.

---

## 5. Phase 1 — Incremental 3.x Upgrade (If Needed)

> ⚠️ **Only required if current version is < 3.1.9**

Execute the incremental scripts in **strict order**. Do not skip versions.

### 5.1 PostgreSQL — Incremental Path

```
Path: src/main/resources/db-scripts/3.xx_db_migration_scripts/psql_db_scripts/
```

| Step | Script Folder | Execute |
|---|---|---|
| 1 | `v3.1.1_to_v3.1.7_db_changes/` | All `.sql` files in this folder |
| 2 | `v3.1.2_to_v3.1.3_db_changes/` | All `.sql` files in this folder |
| 3 | `v3.1.3_to_v3.1.4_db_changes/` | All `.sql` files in this folder |
| 4 | `v3.1.4_to_v3.1.5_db_changes/` | All `.sql` files in this folder |
| 5 | `v3.1.6_to_v3.1.7_db_changes/` | All `.sql` files in this folder |
| 6 | `v3.1.7_to_v3.1.8_db_changes/` | `no_changes.sql` (no-op, safe) |
| 7 | `v3.1.8_to_v3.1.9_db_changes/` | All `.sql` files in this folder |

### 5.2 SQL Server — Incremental Path

```
Path: src/main/resources/db-scripts/3.xx_db_migration_scripts/msql_db_scripts/
```

Execute the scripts in the same logical order as PostgreSQL above.

---

## 6. Phase 2 — 4.x Core Migration (All Environments)

This phase creates all `_v2` tables and migrates data from old tables. Scripts are **idempotent** — each script:
1. Checks if the old table exists
2. If yes: creates the `_v2` table and copies data
3. If no: creates the `_v2` table directly (fresh install scenario)

### 6.1 Step 1 — Initialize Schema & Data

#### PostgreSQL

```
Path: src/main/resources/db-scripts/4.xx_db_migration_scripts/psql_db_scripts/init_schema_and_data/
```

Execute the following scripts **in this exact order**:

| # | Script | Creates / Migrates |
|---|---|---|
| 1 | `client_details.sql` | `client_details_v2` |
| 2 | `healthcareSettings.sql` | `healthcare_setting_v2` |
| 3 | `launch_details.sql` | `launch_details_v2` |
| 4 | `eicr.sql` | `eicr_v2` |
| 5 | `kar_repos.sql` | `kar_repos_v2` |
| 6 | `kar_info.sql` | `kar_info_v2` |
| 7 | `kar_execution_state.sql` | `kar_execution_state_v2` |
| 8 | `hs_kar_status.sql` | `hs_kar_status_v2` |
| 9 | `notification_context.sql` | `notification_context_v2` |
| 10 | `ph_messages.sql` | `ph_messages_v2` |
| 11 | `public_health_authority.sql` | `public_health_authority_v2` |
| 12 | `scheduled_tasks.sql` | `scheduled_tasks_v2` |

> **Key behavior of each script:**
> - Uses `DO $$ ... $$` anonymous blocks for transactional safety
> - Performs column compatibility checks before inserting — raises `EXCEPTION` on mismatch
> - Reports row counts via `RAISE NOTICE` for verification
> - `eicr.sql` deduplicates on `eicr_doc_id` during migration

#### SQL Server

```
Path: src/main/resources/db-scripts/4.xx_db_migration_scripts/msql_db_scripts/init_schema_and_data/
```

Execute the same set of scripts in the same order as listed above.

---

### 6.2 Step 2 — Apply Constraints & Indexes

#### PostgreSQL

```
Path: src/main/resources/db-scripts/4.xx_db_migration_scripts/psql_db_scripts/schema_alterations_and_constraints/
```

Execute the following scripts **in this exact order**:

| # | Script | Applies |
|---|---|---|
| 1 | `healthcare_setting.sql` | Constraints on `healthcare_setting_v2` |
| 2 | `launch_details.sql` | Unique constraint `uc_launch_details_v2` on `(ehr_server_url, launch_patient_id, encounter_id)` |
| 3 | `notification_context.sql` | Constraints on `notification_context_v2` |
| 4 | `ph_messages.sql` | Indexes/constraints on `ph_messages_v2` |
| 5 | `public_health_authority.sql` | Constraints on `public_health_authority_v2` |
| 6 | `client_details.sql` | Constraints on `client_details_v2` |
| 7 | `kar_info.sql` | Constraints on `kar_info_v2` |
| 8 | `kar_repo.sql` | Constraints on `kar_repos_v2` |
| 9 | `kar_execution_state.sql` | Constraints on `kar_execution_state_v2` |
| 10 | `hs_kar_status.sql` | Constraints on `hs_kar_status_v2` |

> All constraint scripts use `IF NOT EXISTS` guards — safe to re-run.

#### SQL Server

```
Path: src/main/resources/db-scripts/4.xx_db_migration_scripts/msql_db_scripts/schema_alterations_and_constraints/
```

Execute the same set of files as listed above.

---

## 7. Phase 3 — Sequence Synchronization (PostgreSQL Only)

> ⚠️ **Critical step.** Skipping this causes `duplicate key` errors when the application starts.

After data is copied into all `_v2` tables, sequences must be re-aligned to `MAX(id) + 1` for each table.

```
Path: src/main/resources/db-scripts/4.xx_db_migration_scripts/psql_db_scripts/schema_alterations_and_constraints/fix_all_v2_sequences.sql
```

**What this script does:**
- Scans all `_v2` tables in the `public` schema
- For each table with an `id` column: finds its sequence (via `pg_get_serial_sequence` or naming convention `<table>_seq`)
- Calls `setval(seq_name, MAX(id), true)` to align the sequence
- Skips tables without an `id` column (e.g., composite PK tables like `hs_kar_status_v2`)
- Fully idempotent — safe to run multiple times

**Expected output (example):**

```
--- Processing table: healthcare_setting_v2 ---
  [INFO] Sequence: public.healthcare_setting_v2_seq
  [INFO] MAX(id) = 42
  [OK]   Sequence reset to 42
=== Sequence sync complete for all _v2 tables ===
```

---

## 8. Execution Instructions by Database Tool

### PostgreSQL — pgAdmin

1. Open **pgAdmin** → connect to the target database
2. Right-click the database → **Query Tool**
3. Click **Open File** → load the `.sql` script
4. Click **Execute / Run (F5)**
5. Review the **Messages** tab for `RAISE NOTICE` output
6. Confirm no `ERROR` messages before proceeding to the next script

### PostgreSQL — psql (Command Line)

```bash
psql -U <db_user> -d <db_name> -h <host> -p <port> -f path/to/script.sql
```

To run all init scripts in order (Linux/macOS):

```bash
SCRIPT_DIR="src/main/resources/db-scripts/4.xx_db_migration_scripts/psql_db_scripts/init_schema_and_data"

for script in client_details healthcareSettings launch_details eicr kar_repos kar_info kar_execution_state hs_kar_status notification_context ph_messages public_health_authority scheduled_tasks; do
  echo "Running: ${script}.sql"
  psql -U <db_user> -d <db_name> -f "${SCRIPT_DIR}/${script}.sql"
done
```

### SQL Server — SSMS

1. Open **SQL Server Management Studio**
2. Connect to the target instance and select the correct database
3. **File → Open → File** → select the `.sql` script
4. Ensure the correct database is selected in the toolbar dropdown
5. Click **Execute (F5)**
6. Review the **Messages** tab for output
7. Validate schema changes in **Object Explorer**

---

## 9. Post-Migration Validation Checklist

Run these checks after completing all phases.

### 9.1 Verify Tables Created

```sql
-- PostgreSQL
SELECT table_name
FROM information_schema.tables
WHERE table_schema = 'public'
  AND table_name LIKE '%_v2'
ORDER BY table_name;
```

**Expected tables:**

| Table |
|---|
| `client_details_v2` |
| `eicr_v2` |
| `healthcare_setting_v2` |
| `hs_kar_status_v2` |
| `kar_execution_state_v2` |
| `kar_info_v2` |
| `kar_repos_v2` |
| `launch_details_v2` |
| `notification_context_v2` |
| `ph_messages_v2` |
| `public_health_authority_v2` |
| `scheduled_tasks_v2` |

### 9.2 Verify Row Counts Match

```sql
-- PostgreSQL
SELECT
  (SELECT COUNT(*) FROM healthcare_setting)    AS old_count,
  (SELECT COUNT(*) FROM healthcare_setting_v2) AS new_count;

SELECT
  (SELECT COUNT(*) FROM eicr)    AS old_count,
  (SELECT COUNT(*) FROM eicr_v2) AS new_count;

SELECT
  (SELECT COUNT(*) FROM launch_details)    AS old_count,
  (SELECT COUNT(*) FROM launch_details_v2) AS new_count;

SELECT
  (SELECT COUNT(*) FROM kar_info)    AS old_count,
  (SELECT COUNT(*) FROM kar_info_v2) AS new_count;

SELECT
  (SELECT COUNT(*) FROM notification_context)    AS old_count,
  (SELECT COUNT(*) FROM notification_context_v2) AS new_count;

SELECT
  (SELECT COUNT(*) FROM ph_messages)    AS old_count,
  (SELECT COUNT(*) FROM ph_messages_v2) AS new_count;
```

> **Note:** `eicr_v2` may have fewer rows than `eicr` if records with `NULL` `eicr_doc_id` existed — this is by design (duplicates are skipped).

### 9.3 Verify Constraints Applied

```sql
-- PostgreSQL: Check unique constraints
SELECT constraint_name, table_name
FROM information_schema.table_constraints
WHERE table_name IN ('launch_details_v2', 'notification_context_v2')
  AND constraint_type = 'UNIQUE';
```

**Expected constraints:**
- `uc_launch_details_v2` on `launch_details_v2`

### 9.4 Verify Sequences Are Synced

```sql
-- PostgreSQL
SELECT sequencename, last_value
FROM pg_sequences
WHERE schemaname = 'public'
  AND sequencename LIKE '%_v2_seq'
ORDER BY sequencename;
```

Compare `last_value` against `MAX(id)` for each table — they should match.

### 9.5 Verify Application Connectivity

```
[ ] Start the eCRNow application
[ ] Check application logs — no DB connection errors
[ ] Confirm the UI loads and existing configurations are visible
[ ] Perform a test transaction (e.g., update a healthcare setting)
[ ] Confirm no duplicate key errors in logs
```

---

## 10. Failure Handling & Rollback Plan

### If a Script Fails Mid-Execution

1. **Stop immediately** — do not run subsequent scripts
2. Read the `ERROR` message carefully — note the failing table/constraint
3. Do **not** attempt to manually fix data in production
4. Restore from the pre-migration backup
5. Reproduce and fix the issue in a lower environment
6. Re-run the full migration in the lower environment
7. Only re-attempt production after successful lower-environment validation

### Rollback Procedure

```sql
-- PostgreSQL: Remove all _v2 tables if needed
DROP TABLE IF EXISTS healthcare_setting_v2 CASCADE;
DROP TABLE IF EXISTS eicr_v2 CASCADE;
DROP TABLE IF EXISTS launch_details_v2 CASCADE;
DROP TABLE IF EXISTS kar_info_v2 CASCADE;
DROP TABLE IF EXISTS kar_repos_v2 CASCADE;
DROP TABLE IF EXISTS notification_context_v2 CASCADE;
DROP TABLE IF EXISTS ph_messages_v2 CASCADE;
DROP TABLE IF EXISTS public_health_authority_v2 CASCADE;
DROP TABLE IF EXISTS client_details_v2 CASCADE;
DROP TABLE IF EXISTS hs_kar_status_v2 CASCADE;
DROP TABLE IF EXISTS kar_execution_state_v2 CASCADE;
DROP TABLE IF EXISTS scheduled_tasks_v2 CASCADE;

-- Remove sequences
DROP SEQUENCE IF EXISTS healthcare_setting_v2_seq;
DROP SEQUENCE IF EXISTS eicr_v2_seq;
DROP SEQUENCE IF EXISTS launch_details_v2_seq;
DROP SEQUENCE IF EXISTS kar_info_v2_seq;
DROP SEQUENCE IF EXISTS kar_repos_v2_seq;
DROP SEQUENCE IF EXISTS notification_context_v2_seq;
DROP SEQUENCE IF EXISTS ph_messages_v2_seq;
DROP SEQUENCE IF EXISTS public_health_authority_v2_seq;
DROP SEQUENCE IF EXISTS client_details_v2_seq;
```

> Old tables (`healthcare_setting`, `eicr`, etc.) are **never dropped** by the migration scripts, so the original 3.x schema is always preserved for rollback.

---

## 11. Best Practices for Production

| Practice | Details |
|---|---|
| **Always backup first** | Take a full backup immediately before migration starts |
| **Validate in QA first** | Never run untested scripts directly in production |
| **Run during low-traffic windows** | Schedule during off-hours or planned maintenance windows |
| **Run one script at a time** | Do not batch all scripts into a single execution on first run |
| **Log all executions** | Record: who ran it, when, on which database, and the outcome |
| **Use transactions where possible** | PostgreSQL `DO $$ ... $$` blocks provide implicit transaction scope |
| **Verify row counts at every step** | Compare old vs new table counts after each init script |
| **Run `fix_all_v2_sequences.sql` last** | Always run sequence sync after all data migration is complete |
| **Do not drop old tables immediately** | Keep 3.x tables for at least one release cycle as a safety net |
| **Test rollback in QA** | Validate the rollback procedure works before attempting production migration |

---

## 12. Quick Reference: Full Script Execution Order

```
PHASE 1 (Only if current version < v3.1.9):
  └── 3.xx_db_migration_scripts/psql_db_scripts/<version>/  (incremental per version)

PHASE 2A — Init Schema & Data (PostgreSQL):
  └── 4.xx_db_migration_scripts/psql_db_scripts/init_schema_and_data/
      1.  client_details.sql
      2.  healthcareSettings.sql
      3.  launch_details.sql
      4.  eicr.sql
      5.  kar_repos.sql
      6.  kar_info.sql
      7.  kar_execution_state.sql
      8.  hs_kar_status.sql
      9.  notification_context.sql
      10. ph_messages.sql
      11. public_health_authority.sql
      12. scheduled_tasks.sql

PHASE 2B — Constraints & Indexes (PostgreSQL):
  └── 4.xx_db_migration_scripts/psql_db_scripts/schema_alterations_and_constraints/
      1.  healthcare_setting.sql
      2.  launch_details.sql
      3.  notification_context.sql
      4.  ph_messages.sql
      5.  public_health_authority.sql
      6.  client_details.sql
      7.  kar_info.sql
      8.  kar_repo.sql
      9.  kar_execution_state.sql
      10. hs_kar_status.sql

PHASE 3 — Sequence Sync (PostgreSQL ONLY):
  └── schema_alterations_and_constraints/fix_all_v2_sequences.sql

(Repeat PHASE 2A, 2B using msql_db_scripts/ path for SQL Server)
```

---

> **Support Reference:** For script-level questions, refer to `documents/Database Script Execution Guide.docx` in the repository, or the **[eCRNow README](../README.md)** for general migration guidance.
> For application configuration after migration, see [Section 4.3 of the README](../README.md#43-ecrnow-app-configuration-steps).  
> For full application setup, build instructions, and deployment details, refer to the **[eCRNow README](../README.md)**.


