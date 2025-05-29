**Upgrading eCRNow App releases between any 3.X release (for e.g, 3.1.1 to 3.1.12)**

The following steps should be followed for updating the eCRNow App releases.

1. Download the release that you desire to use and test the release in your test environment.
2. After verification of the release, stop the notification process to eCRNow. This stops invoking the launchPatient or reLaunchPatient or reProcessPatient API calls.
3. Stop the eCRNow App
4. Apply the DB changes using the DB Scripts provided in psql_db_scripts or msql_db_scripts folder as applicable to your environment. For example, if you are migrating from 3.1.1 to 3.1.11 apply the scripts in the folder v3.1.1_to_v3.1.11_db_changes.
5. Once the DB changes are completed, deploy the latest eCRNow App.
6. Restart the eCRNow App.
7. After maybe 5 to 10 minutes, restart your notification feed that was stopped in Step 2. The reason for the 5 to 10 minutes to ensure any old timers that exist are executed.

**Upgrading from 2.X of the App to 3.X of the eCRNow App Releases**

The following set of instructions provide guidance for EHR vendors to upgrade the eCRNow App from 2.0 to 3.0. NOTE: All the instructions provided are generic and these may vary based on vendor deployments and customizations.

* Compatibility across versions: eCRNow 3.x.x version of the app is compatible with version 2.x.x of the app. The current master branch in fact allows vendors to use either 2.x.x or 3.x.x or both at the same time.
* eCRNow 2.x.x uses ERSD v1 instances and the API /api/systemLaunch API
* eCRNow 3.x.x uses ERSD v2 instances and the API /api/launchPatient

These APIs have deliberately been designed to be different to be allowed for the upgrades and the database tables backing the 2.x.x and 3.x.x are different from each other. 

**2.X.X Tables:**
eICR
cllient_details
launch_details
scheduled_tasks

**3.X.X Tables:**
ph_messages (equivalent to eICR)
healthcare_setting (equivalent to client_details)
notification_context (equivalent to launch_details)
kar_repos
kar_info
hs_kar_status
kar_execution_state
public_health_authority
scheduled_tasks (same table present in both versions of the app)

The scheduled_tasks table is the only common table that is used across both versions of the app. The scheduled_tasks table contains information regarding the timers that are going to expire for either version of the app. The callback function registered with the timers during creation will route the timer to the appropriate logic in the app.

**New Healthcare site onboarding with 3.x.x involves the following steps**

* Upgrade from whichever baseline you are running to the latest master baseline.
* For all new healthcare sites create a new healthcare setting instance.
* Download the new ERSD v2 or ERSD v3 instance from the ERSD website.
* Enable the ERSD v2 or ERSD v3 so that it can be activated for the healthcare setting being integrated. These instructions are present in the Release 3.0 Configuration guide.
* Integrate with the healthcare setting to invoke the /api/launchPatient for all patients and encounters that get started.

This will start using the version 3.X.X of the app for the healthcare site.

**Upgrading existing healthcare sites which are already running 2.x.x of the app**

* Stop invoking /api/systemLaunch when the upgrade is going to start.
* Keep track of all messages that would have been queued up during the upgrade.
* Perform the activities as described previously for New Healthcare site onboarding with 3.x.x for the site using 2.x.x
* Ensure /api/systemLaunch is not called for this site anymore.
* All new transactions will migrate over to 3.X.X of the app.
* The existing timers will fire and eventually bleed out of the table. This may take many days or weeks depending on the type of encounters.
* Once the existing timers bleed out, all transactions going forward will be on 3.X.X of the app.

**Migrating from existing infrastructure to a new infrastructure**

Sometimes vendors would like to migrate from existing infrastructure (say from on-prem) to a new infrastructure (say cloud), then there is a series of steps that need to be followed to ensure smooth transition of the ecrNow App data. These steps apply when migrating from one cloud provider (say AWS or Azure) to another cloud provider( say Azure or AWS) or when migrating to new instances within the same cloud provider.

Plan the migration such that there is minimal downtime.
1. Start the eCRNow App in the new infrastructure.
2. Configure all the healthcare settings following the configuration guide.
3. The app is now ready to process notifications in the new infrastructure.
4. Stop invoking /api/systemLaunch or /api/launchPatient or /api/relaunchPatient or /api/reProcessPatient APIs on the old eCRNow App running on the old infrastructure.
5. Let all the existing timers bleed out, keep the existing eCRNow App running until the scheduled_tasks table becomes empty, this may take a week or longer.
6. After stopping the notifications in the old infrastructure, Redirect the notifications to the new infrastructure (i.e, start invoking systemLaunch, launchPatient, reLaunchPatient, reProcessPatient on the new eCRNow App infrastructure that has been stood up.
7. Eventually the old timers in the old infrastructure will finish and the old infrastructure can be shutdown at that point of time as the new infrastruture has already started processing the activities. 
