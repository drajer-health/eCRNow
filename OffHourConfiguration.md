# 1. Off Hours Introduction #

EHRs are used by Providers as part of the care delivery operations and there may be large amount of load on the EHRs during the day time and there may be a desire
to reduce the load during the working hours or peak usage hours. 
In these cases, it is beneficial if the eCRNow App can be configured to only process patients whose data has to be reported immediately and delay the processing of the other patients to 
off hours (non peak hours). This kind of configuration can be achieved by the OffHour Configuration.

OffHours are non-peak hours where the expected load on the EHR is significantly lesser than during the working day.

For e.g Peak Business Hours may be : 6am to 8pm.

Then OffHours will be 8pm to 6am.

# 2. Configuring Off Hours # 

OffHours can be configured for each Healthcare Setting in the database directly. They will be added to the UI eventually.

The fields to be configured are: 

off_hours_enabled -- Should be set to true / 1
off_hours_start --- Start of off hours in 24 hour clock format. For the above example, off_hours_start = 20
off_hours_end -- End of the off hours in 24 hour clock format. For the above example, off_hours_end = 6

If you wanted to specify minutes offset for the hours, for e.g if you need to specify 8:30 and 6:45, 
then you would set the following

off_hours_enabled = 1
off_hours_start = 20
off_hours_start_min = 30
off_hours_end = 6
off_hours_end_min = 45

In case the minutes offset is zero, for e.g if you want to set 8pm to 6am the following would be set.

off_hours_enabled = 1
off_hours_start = 20
off_hours_start_min = 00
off_hours_end = 6
off_hours_end_min = 00

The off_hours_timezone is currently assumed to be UTC/GMT only. So setting the timezone to something else has no effect.
So the start and end times have to be translated to UTC times when setting up these attributes.

The total off hours cannot be greater than 24 hours.

#3. Tuning the OffHours and the number of parallel threads #

The result of configuring OffHours is reduction in the load on the EHR due to electronic case reporting, however an increase in load in the OffHours is expected.
All the pending processing will be queued up for the OffHours, so the EHRs FHIR Server and the # of parallel threads required to complete the 
pending load should be tuned for optimal performance.   

