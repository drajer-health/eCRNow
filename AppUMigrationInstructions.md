The following set of instructions provide guidance for EHR vendors to upgrade the eCRNow App from 2.0 to 3.0. 
**NOTE:** All the instructions provided are generic and these may vary based on vendor deployments and customizations. 

Compatibility across versions: 
eCRNow 3.x.x version of the app is compatible with version 2.x.x of the app. The current master branch in fact allows vendors to use either 2.x.x or 3.x.x or both at the same time. 

eCRNow 2.x.x uses ERSD v1 instances and the API /api/systemLaunch

eCRNow 3.x.x uses ERSD v2 instances and the API /api/launchPatient

These APIs have deliberately been designed to be different to be allowed for the upgrades and the database tables backing the 2.x.x and 3.x.x are different from each other.
2.X.X Tables:
  - eICR
  - cllient_details
  - launch_details
  - scheduled_tasks

3.X.X Tables:
  - ph_messages
  - healthcare_setting
  - notification_context
  - kar_repos
  - kar_info
  - hs_kar_status
  - kar_execution_state
  - public_health_authority
  - scheduled_tasks
  
The scheduled_tasks table is the only common table that is used across both versions of the app. 
The scheduled_tasks table contains information regarding the timers that are going to expire for either version of the app. 
The callback function registered with the timers during creation will route the timer to the appropriate logic in the app.

**New Healthcare site onboarding with 3.x.x involves the following steps**

1. Upgrade from whichever baseline you are running to the latest master baseline. 
2. For all new healthcare sites create a new healthcare setting instance.
3. Download the new ERSD v2 instance from the ERSD website.
4. Enable the ERSD v2 so that it can be activated for the healthcare setting being integrated. These instructions are present in the Release 3.0 Configuration guide.
5. Integrate with the healthcare setting to invoke the /api/launchPatient for all patients and encounters that get started. 
6. This will start using the version 3.X.X of the app for the healthcare site. 

**Upgrading existing healthcare sites which are already running 2.x.x of the app**

1. Stop invoking /api/systemLaunch when the upgrade is going to start.
2. Keep track of all messages that would have been queued up during the upgrade.
3. Perform the  activities as described previously for **New Healthcare site onboarding with 3.x.x** for the site using 2.x.x
4. Ensure /api/systemLaunch is not called for this site anymore.
5. All new transactions will migrate over to 3.X.X of the app.
6. The existing timers will fire and eventually bleed out of the table. This may take many days or weeks depending on the type of encounters.
7. Once the existing timers bleed out, all transactions going forward will be on 3.X.X of the app.

**Migrating from existing infrastructure to a new infrastructure** 

Sometimes vendors would like to migrate from existing infrastructure (say from on-prem) to a new infrastructure (say cloud), then there is a series 
of steps that need to be followed to ensure smooth transition of the ecrNow App data. 
These steps apply when migrating from one cloud provider (say AWS or Azure) to another cloud provider( say Azure or AWS) or when migrating to new 
instances within the same cloud provider. 

1. Plan the migration such that there is minimal downtime. 
2. For the duration of the migration, stop invoking /api/systemLaunch or /api/launchPatient or /api/relaunchPatient APIs.
3. 





