package com.drajer.bsa.utils;

import ca.uhn.fhir.context.FhirContext;
import java.util.List;
import org.hl7.fhir.instance.model.api.IBase;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.*;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

public class TestFhirPathEvaluator {

  private static final FhirContext fhirContext = FhirContext.forR4();

  @Test
  public void testEvaluateFhirPathUsingPatient() {
    Patient patient = createPatient();
    String patientFamilyExpression = "Patient.name.where(use='usual').given.first()";
    List<IBase> results = FhirPathEvaluator.evaluateFhirPath(patient, patientFamilyExpression);
    Assertions.assertEquals("Jim", results.get(0).toString());
  }

  @Test
  public void testEvaluateFhirPathUsingBundleWhereAndSelect() {
    String bundleExpression =
        "Bundle.entry.select(resource as Patient).where(gender='female').select(name.family)";
    Bundle bundle = createBundle();
    List<IBase> results = FhirPathEvaluator.evaluateFhirPath(bundle, bundleExpression);
    String family = results.get(0).toString();
    Assertions.assertEquals("Smith", family);
  }

  @Test
  public void testEvaluateFhirPathUsingBundleGender() {
    String bundleExpression = "Bundle.entry.resource.ofType(Patient).select(gender)";
    Bundle bundle = createBundle();
    List<IBase> results = FhirPathEvaluator.evaluateFhirPath(bundle, bundleExpression);
    Assertions.assertEquals("female", ((Enumeration) results.get(0)).primitiveValue());
  }

  @Test
  public void testEvaluateFhirPathUsingBundleSelectAndWhere() {
    String bundleExpression =
        "Bundle.entry.resource.ofType(Patient).where(gender='female').select(name.family)";
    Bundle bundle = createBundle();
    List<IBase> results = FhirPathEvaluator.evaluateFhirPath(bundle, bundleExpression);
    String family = results.get(0).toString();
    Assertions.assertEquals("Smith", family);
  }

  @Test
  public void testEvaluateFhirPathUsingBundleName() {
    String bundleExpression = "Bundle.entry.select(resource as Patient).name.where(given='Jane')";
    Bundle bundle = createBundle();
    List<IBase> results = FhirPathEvaluator.evaluateFhirPath(bundle, bundleExpression);
    HumanName name = (HumanName) results.get(0);
    String family = name.getFamily();
    Assertions.assertEquals("Smith", family);
  }

  @Test
  public void testEvaluateFhirPathUsingBundleComplex() {
    String bundleExpression =
        "Bundle.entry.select(resource as Patient).first().where(gender='female').exists()";
    Bundle bundle = createBundle();
    List<IBase> results = FhirPathEvaluator.evaluateFhirPath(bundle, bundleExpression);
    BooleanType isExists = (BooleanType) results.get(0);
    String boolVal = isExists.asStringValue();
    Assertions.assertEquals("true", boolVal);
  }

  @Test
  public void testEvaluateFhirPathUsingBundleComplex1() {
    String bundleExpression =
        "(Bundle.entry.resource.first() is Patient and Bundle.entry.resource[1].name.exists())"
            + "or (Bundle.entry.resource[0].where(gender = 'female'))";
    Bundle bundle = createBundle();
    List<IBase> results = FhirPathEvaluator.evaluateFhirPath(bundle, bundleExpression);
    BooleanType isExists = (BooleanType) results.get(0);
    String boolVal = isExists.asStringValue();
    Assertions.assertEquals("true", boolVal);
  }

  private static Patient createPatient() {
    String patientJson =
        """
        {
          "resourceType": "Patient",
          "id": "example",
          "address": [
            {
              "use": "home",
              "city": "PleasantVille",
              "type": "both",
              "state": "Vic",
              "line": [
                "534 Erewhon St"
              ],
              "postalCode": "3999",
              "period": {
                "start": "1974-12-25"
              },
              "district": "Rainbow",
              "text": "534 Erewhon St PeasantVille, Rainbow, Vic  3999"
            }
          ],
          "managingOrganization": {
            "reference": "Organization/1"
          },
          "name": [
            {
              "use": "official",
              "given": [
                "Peter",
                "James"
              ],
              "family": "Chalmers"
            },
            {
              "use": "usual",
              "given": [
                "Jim"
              ]
            },
            {
              "use": "maiden",
              "given": [
                "Peter",
                "James"
              ],
              "family": "Windsor",
              "period": {
                "end": "2002"
              }
            }
          ],
          "birthDate": "1974-12-25",
          "deceased": {
            "boolean": false
          },
          "active": true,
          "identifier": [
            {
              "use": "usual",
              "type": {
                "coding": [
                  {
                    "code": "MR",
                    "system": "http://hl7.org/fhir/v2/0203"
                  }
                ]
              },
              "value": "12345",
              "period": {
                "start": "2001-05-06"
              },
              "system": "urn:oid:1.2.36.146.595.217.0.1",
              "assigner": {
                "display": "Acme Healthcare"
              }
            }
          ],
          "telecom": [
            {
              "use": "home"
            },
            {
              "use": "work",
              "rank": 1,
              "value": "(03) 5555 6473",
              "system": "phone"
            },
            {
              "use": "mobile",
              "rank": 2,
              "value": "(03) 3410 5613",
              "system": "phone"
            },
            {
              "use": "old",
              "value": "(03) 5555 8834",
              "period": {
                "end": "2014"
              },
              "system": "phone"
            }
          ],
          "gender": "male",
          "contact": [
            {
              "name": {
                "given": [
                  "Bénédicte"
                ],
                "family": "du Marché",
                "_family": {
                  "extension": [
                    {
                      "url": "http://hl7.org/fhir/StructureDefinition/humanname-own-prefix",
                      "valueString": "VV"
                    }
                  ]
                }
              },
              "gender": "female",
              "period": {
                "start": "2012"
              },
              "address": {
                "use": "home",
                "city": "PleasantVille",
                "line": [
                  "534 Erewhon St"
                ],
                "type": "both",
                "state": "Vic",
                "period": {
                  "start": "1974-12-25"
                },
                "district": "Rainbow",
                "postalCode": "3999"
              },
              "telecom": [
                {
                  "value": "+33 (237) 998327",
                  "system": "phone"
                }
              ],
              "relationship": [
                {
                  "coding": [
                    {
                      "code": "N",
                      "system": "http://hl7.org/fhir/v2/0131"
                    }
                  ]
                }
              ]
            }
          ]
        }
        """;
    IBaseResource resource = fhirContext.newJsonParser().parseResource(patientJson);
    return (Patient) resource;
  }

  private static Bundle createBundle() {
    String json =
        """
        {
          "resourceType": "Bundle",
          "type": "collection",
          "entry": [
            {
              "resource": {
                "resourceType": "Patient",
                "id": "patient-1",
                "name": [
                  {
                    "family": "Smith",
                    "given": ["Jane"]
                  }
                ],
                "gender": "female"
              }
            },
            {
              "resource": {
                "resourceType": "Patient",
                "id": "patient-2",
                "name": [
                  {
                    "family": "Doe",
                    "given": ["John"]
                  }
                ],
                "gender": "male"
              }
            },
            {
              "resource": {
                "resourceType": "Observation",
                "id": "obs-1",
                "status": "final"
              }
            }
          ]
        }
        """;
    IBaseResource resource = fhirContext.newJsonParser().parseResource(json);
    return (Bundle) resource;
  }
}
