# 1. Running Multiple ERSD Versions concurrently #

The eCRNow App is capable of executing multiple ERSD versions concurrently. There are many scenarios where this is required.
Vendors are encouraged to use ERSDv3 and produce CDA_R31 version of the eICRs.

## Scenario 1: Organizations want to migrate to new ERSD file.

In this scenario, the same organization (HealthcareSetting) with the same FHIR Server URL will use multiple ERSD files.

The steps to enable such a configuration is 

1. Disable the Old ERSD file
2. Place the new ERSD file in the folder where the ERSD files are located.
3. Restart eCRNow App.
3. Enable the new ERSD file.
4. The Old ERSD file needs to be present until all the timers that were created using the Old ERSD file get removed.
5. Finally purge the Old ERSD file when there are no more timers with the old ERSD file by using the script
"CheckTimersForERSD".

## Scenario 2: Organizations wants to use different ERSDs for Test and Production with different Healthcare Settings.

The steps to enable such a configuration for ERSDv2 is as follows:  

1. Keep the Old ERSD file as it is currently.
2. Copy the Old ERSD file into a new ERSD file. (Let us call it New ERSD).
3. In the new ERSD file, change the following: 
	- Bundle.meta.versionId to a different version.
4. Place the New ERSD file into the folder where the ERSD files are located.
5. Restart the eCRNow App.
6. Enable the New ERSD for the Healthcare Setting that is needed.  

The steps to enable such a configuration for ERSDv3 is as follows:  

1. Keep the Old ERSD file as it is currently.
2. Copy the Old ERSD file into a new ERSD file. (Let us call it New ERSD).
3. In the new ERSD file, change the following: 
	- Library.meta.versionId to a different version.
	- Library resource to be updated should have the profile set to http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-specification-library"
4. Place the New ERSD file into the folder where the ERSD files are located.
5. Restart the eCRNow App.
6. Enable the New ERSD for the Healthcare Setting that is needed. 
