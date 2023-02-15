# 1. Introduction to Throttling #

EHR's FHIR server which services the FHIR APIs invoked by the eCRNow App have to be serviced by the EHR for the eCRNOw 
transactions to successfully complete. Sometimes the EHR's FHIR Server may not be available or may be busy and cannot service
the requests originating from the eCRNow App. In these cases the transactions in eCRNow will fail and then get retried.
However after a configurable number of retries and failures the timers will be removed and the processing for the 
specific patient will be terminated. This could be problematic as the patient's data is not evaluated for the eICR generation. 
In order to avoid invoking the EHR's FHIR APIs when it is not able to service, a throttling mechanism is built into the eCRNow App.

To enable throttling set the following property to true. 
enable.throttling=true

If the above property is true and if the infrastructure is busy, then the following property will determine the recheck interval.
The recheck interval is in minutes and the eCRNow App will recheck after 5 minutes for the below configuration if the timer that has
expired can be executed.

throttle.recheck.interval=5

#2. Informing the eCRNow App if the request can be processed or needs to be throttled #

For the EHR system to tell the eCRNow App that it cannot process the FHIR Request, the following interface has to be implemented by the vendor.

InfrastructureLoadManagerInterface.java (canExecuteJob method).

If the method returns true, the eCRNow will continue to perform normal processing, if it is false, it will recheck after the throttle.recheck.interval 
parameter value in minutes.

Sometimes the EHR systems may require a context parameter to evaluate the infrastructure. For example, in a multi-tenant configuration, you may want to 
know which tenant's FHIR APIs may be accessed for you to make a decision. 

This information will be exchanged as follows:

In the launchPatient API, a parameter called "throttleContext" can be sent to the eCRNow App.
This throttleContext will be sent back to you on the canExecuteJob method.

An example is : If you have 3 different containers of FHIR Servers running and container 1 services tenant 1 to 5 and container 2 services tenants 6 to 10 and container 3 services tenants 11 to 15.
You can pass the container # or the tenant Id (1 to 16) in the above parameter. So when you get the same parameter back in the canExecuteJob
you can see if the CPU and Memory usages are below a threshold allow the eCRNow app to call the APIs or hold back by throttling.

