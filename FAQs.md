This page contains the FAQs associated with using eCRNow App and providing services to the Providers. 

**1. What should be the size of the server for installing and running eCRNow App ?**

For production, it really depends on the work load. It is a horizontally scalable application which can be deployed on docker / kubernetes or VMs.
we recommend using Linux machines, but if your environment has only windows you can still deploy the application on windows. There are vendors who have deployed on windows.
Typically vendors are starting off with a 4 CPU, 16GB RAM, 500GB or 1TB hard disk as a starting point and then adjust from there.

**2. How do I get notification of changes and releases ?**

eCRNow App releases are scheduled for the last Monday of every month typically. You can subscribe to github notifications to get notified of new releases.

**3. How do I get the latest eRSD and be notified of changes in eRSD versions ?**

All eRSD related documentation including downloading eRSD's and subscribing for eRSD changes are documented at https://ersd.aimsplatform.org/#/home 

**4. How do I update my production releases of eCRNow ?**

Please refer to the eCRNowAppUpgrade.md instructions.



**5. How do I update my eRSD files to latest version ?** 

Please refer to the MultipleERSDConfiguration.md instructions. 

**6. How do I configure custom-queries ?**

Please refer to the CustomQueryConfiguration.md instructions.

**7. How do I protect the eCRNow App APIs ?**

Please refer to the Security Configuration.md instructions.



