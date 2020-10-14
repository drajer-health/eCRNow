# 1. eCR Now Testing
Fully automated tests have been created to provide comprehensive testing coverage to eCRNow application. 

# 2. Test Design and Architecture # 
The tests in eCR Now can be broadly divided into two categories- Unit Tests and Integration Tests.  Unit tests utilize Surefire and Integration Tests utilize Failsafe maven plugin for reporting purpose. Jacoco provides test coverage information for both unit tests and integration tests.

Details on these plugins can be found below : 

* [ Maven SureFire Plugin](https://maven.apache.org/surefire-archives/surefire-2.19/maven-surefire-plugin/index.html)

* [ Maven Failsafe Plugin](https://maven.apache.org/surefire/maven-failsafe-plugin/index.html)
 
* [ Jacoco Plugin](https://www.eclemma.org/jacoco/trunk/doc/maven.html)

### 2.1 Integration Tests

The Integration tests are created as SpringBootTest using the Spring Boot Module . In memory H2 db is used to persist and fetch data for and during test runs. The tests are parameterized and thus multiple test cases can be run using the same test class. These test cases have been defined in Test(name).yaml file. For external FHIR http calls, WireMock framework is used to mock http responses. The input and expected data and the expected responses for wiremock are defined in this yaml through filepaths. Any new tests can utilize this setup and add additional test cases as per requirement. 

For more information on wiremock framework please refer to below links:
* [ WireMock](http://wiremock.org/docs/)


### 2.2 Unit Tests

Unit test Description to be added here...



# 3. Steps to create new Test Case
Steps to create new tests for Unit and Integration Tests are summarized as below:

### 3.1 Steps to create Integration Test
New test cases can be added to existing tests easily by adding entry into specific yaml files.
For example for System Launch Tests we have TestSystemLaunch.yaml
The yaml file contains list of all test cases. Each testcase contains the data required to run and validate the test.
##### Testcase section in yaml consists of following section:
##### 1. fileData: 
Consists of all the file paths that are required to set up and launch test as well as for validation. The data is saved as key value pair for the file name and filepath. It also contains the names of all the sections of the EICR xml that needs to be validated a spart of this test case. THe key to be used for this is "ValidationSections" and should not be changed.
```FileData example:```
```
    ClientDataToBeSaved: "R4/Misc/ClientDetails/ClientDataEntry1.json"
    SystemLaunchPayload: "R4/Misc/SystemLaunchPayload/systemLaunchRequest3.json"
    ValidationSections: "ENCOUNTERS|VISITS"
```      
##### 2. resourceMappingData: 
Consists of all the mappings for FHIR resources that are to be stubbed using the wiremock utility. Later, these resources would be called through the application when test is invoked. The data is saved as key value pair where key is the resource name and value is a list of mappings. This list in turn consists of a param and the response json file path. Param has value as resource id if the the FHIR call is to happen through path param. If the call happens through query param, then the param has a map of all query params and values as map. 
```Path param example:```
```
Patient:
      - params: "12742536"
        responseFilePath: "R4/Patient/Patient_12742536.json"
```

```Query param example:```
```
Observation:
      - params:
          patient: "12742536"
          category: "laboratory"
        responseFilePath: "R4/Observation/ObservationBundle_4.json"
```

##### 3. otherMappingData
This section consists of other mappings which are not resources but are required for stubbing.
```otherMappingData example:```
```
    otherMappingData:
      metadata: "R4/Misc/MetaData_r4.json"
      token: "R4/Misc/AccessToken.json"
      default: "R4/Misc/NoDataFound_Default.json" 
```     
### 3.2 Steps to create Unit Test
Add steps here...






