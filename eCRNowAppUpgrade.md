** Upgrading eCRNow App releases between any 3.X release (for e.g, 3.1.1 to 3.1.12)**

The following steps should be followed for updating the eCRNow App releases.

1. Download the release that you desire to use and test the release in your test environment.
2. After verification of the release, stop the notification process to eCRNow. This stops invoking the launchPatient or reLaunchPatient or reProcessPatient API calls.
3. Stop the eCRNow App
4. Apply the DB changes using the DB Scripts provided in psql_db_scripts or msql_db_scripts folder as applicable to your environment. For example, if you are migrating from 3.1.1 to 3.1.11 apply the scripts in the folder v3.1.1_to_v3.1.11_db_changes.
5. Once the DB changes are completed, deploy the latest eCRNow App.
6. Restart the eCRNow App.
7. After maybe 5 to 10 minutes, restart your notification feed that was stopped in Step 2. The reason for the 5 to 10 minutes to ensure any old timers that exist are executed.