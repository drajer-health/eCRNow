# eICR 3.1 Schematron & XSD Configuration

This guide explains how to configure Schematron and XSD files to validate eICRs (electronic Case Reports) for different CDA releases.

---

## 1️ Schematron File Location

The Schematron file is used to validate eICRs. Make sure the configuration points to the correct file for the release you are using.

| eICR Version | Schematron File | Repository URL |
|--------------|----------------|----------------|
| **R3.1 (D3)** | `CDAR2_IG_PHCASERPT_R2_D3_SCHEMATRON.sch` | [HL7 CDA-phcaserpt-1.3.0 Validation](https://github.com/HL7/CDA-phcaserpt-1.3.0/tree/main/validation) |

 **Important:**  
Also include the vocabulary files (`CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml`) in the same directory as the Schematron for proper validation.

**Example configuration in `application.properties`:**

```properties
# Schematron file locations
eicr.R31.schematron.file.location=//Users//nbashyam//ecrnow//schematrons//CDAR2_IG_PHCASERPT_R2_D3_SCHEMATRON.sch
```
## 2️ CDA XSD File Location

The **CDA XSD schema** with **SDTC extensions** is required for structural validation of the eICR.
**CDA Schema with SDTC Extensions**:
- Download the official CDA schema package from the **HL7 website**: [https://www.hl7.org/cda](https://www.hl7.org/cda)
- Locate the `CDA_SDTC.xsd` file within the downloaded package.
- Set the attribute `xsd.schemas.location` in your configuration to point to the CDA XSD with SDTC extensions, for example:


### Attribute and Example Path
| Attribute             | Example Path                                                        |
|------------------------|---------------------------------------------------------------------|
| `xsd.schemas.location` | `//Users//nbashyam//Downloads//schemas//CDA_SDTC.xsd`               |

### Example Configuration
```properties
xsd.schemas.location=//Users//nbashyam//Downloads//schemas//CDA_SDTC.xsd
```
No


### 3️ Validation Flags

The application supports multiple validation types.  
You can enable or disable them using the following configuration flags:

```properties
validate.eicr.cdar11=false    # Allows the validation of a eICR based on CDA Release 1.1 for ECR when set to true.
validate.eicr.cdar31=true   # # Allows the validation of a eICR based on CDA Release 3.1 for ECR when set to true
validate.eicr.fhir=false     # Allows the validation of a eICR based on ECR FHIR IG when set to true..
```

### Enabling eICR 3.1 Validation

To validate against CDA R3.1, update your configuration as follows:
```properties
validate.eicr.cdar31=true
```

###  Notes

- Always ensure that the configured file paths are accessible and have the correct read permissions.
- Use the **latest Schematron files** provided for **eICR 3.1.13** to ensure compliance with updated business rules.
- The **CDA_SDTC.xsd** file is critical for schema-level validation, including structural and SDTC extension checks.  
