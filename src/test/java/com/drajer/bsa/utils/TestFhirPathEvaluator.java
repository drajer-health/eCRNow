package com.drajer.bsa.utils;

import ca.uhn.fhir.context.FhirContext;
import java.util.List;
import org.hl7.fhir.instance.model.api.IBase;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

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
        "{\n"
            + "  \"resourceType\": \"Patient\",\n"
            + "  \"id\": \"example\",\n"
            + "  \"address\": [\n"
            + "    {\n"
            + "      \"use\": \"home\",\n"
            + "      \"city\": \"PleasantVille\",\n"
            + "      \"type\": \"both\",\n"
            + "      \"state\": \"Vic\",\n"
            + "      \"line\": [\n"
            + "        \"534 Erewhon St\"\n"
            + "      ],\n"
            + "      \"postalCode\": \"3999\",\n"
            + "      \"period\": {\n"
            + "        \"start\": \"1974-12-25\"\n"
            + "      },\n"
            + "      \"district\": \"Rainbow\",\n"
            + "      \"text\": \"534 Erewhon St PeasantVille, Rainbow, Vic  3999\"\n"
            + "    }\n"
            + "  ],\n"
            + "  \"managingOrganization\": {\n"
            + "    \"reference\": \"Organization/1\"\n"
            + "  },\n"
            + "  \"name\": [\n"
            + "    {\n"
            + "      \"use\": \"official\",\n"
            + "      \"given\": [\n"
            + "        \"Peter\",\n"
            + "        \"James\"\n"
            + "      ],\n"
            + "      \"family\": \"Chalmers\"\n"
            + "    },\n"
            + "    {\n"
            + "      \"use\": \"usual\",\n"
            + "      \"given\": [\n"
            + "        \"Jim\"\n"
            + "      ]\n"
            + "    },\n"
            + "    {\n"
            + "      \"use\": \"maiden\",\n"
            + "      \"given\": [\n"
            + "        \"Peter\",\n"
            + "        \"James\"\n"
            + "      ],\n"
            + "      \"family\": \"Windsor\",\n"
            + "      \"period\": {\n"
            + "        \"end\": \"2002\"\n"
            + "      }\n"
            + "    }\n"
            + "  ],\n"
            + "  \"birthDate\": \"1974-12-25\",\n"
            + "  \"deceased\": {\n"
            + "    \"boolean\": false\n"
            + "  },\n"
            + "  \"active\": true,\n"
            + "  \"identifier\": [\n"
            + "    {\n"
            + "      \"use\": \"usual\",\n"
            + "      \"type\": {\n"
            + "        \"coding\": [\n"
            + "          {\n"
            + "            \"code\": \"MR\",\n"
            + "            \"system\": \"http://hl7.org/fhir/v2/0203\"\n"
            + "          }\n"
            + "        ]\n"
            + "      },\n"
            + "      \"value\": \"12345\",\n"
            + "      \"period\": {\n"
            + "        \"start\": \"2001-05-06\"\n"
            + "      },\n"
            + "      \"system\": \"urn:oid:1.2.36.146.595.217.0.1\",\n"
            + "      \"assigner\": {\n"
            + "        \"display\": \"Acme Healthcare\"\n"
            + "      }\n"
            + "    }\n"
            + "  ],\n"
            + "  \"telecom\": [\n"
            + "    {\n"
            + "      \"use\": \"home\"\n"
            + "    },\n"
            + "    {\n"
            + "      \"use\": \"work\",\n"
            + "      \"rank\": 1,\n"
            + "      \"value\": \"(03) 5555 6473\",\n"
            + "      \"system\": \"phone\"\n"
            + "    },\n"
            + "    {\n"
            + "      \"use\": \"mobile\",\n"
            + "      \"rank\": 2,\n"
            + "      \"value\": \"(03) 3410 5613\",\n"
            + "      \"system\": \"phone\"\n"
            + "    },\n"
            + "    {\n"
            + "      \"use\": \"old\",\n"
            + "      \"value\": \"(03) 5555 8834\",\n"
            + "      \"period\": {\n"
            + "        \"end\": \"2014\"\n"
            + "      },\n"
            + "      \"system\": \"phone\"\n"
            + "    }\n"
            + "  ],\n"
            + "  \"gender\": \"male\",\n"
            + "  \"contact\": [\n"
            + "    {\n"
            + "      \"name\": {\n"
            + "        \"given\": [\n"
            + "          \"Bénédicte\"\n"
            + "        ],\n"
            + "        \"family\": \"du Marché\",\n"
            + "        \"_family\": {\n"
            + "          \"extension\": [\n"
            + "            {\n"
            + "              \"url\": \"http://hl7.org/fhir/StructureDefinition/humanname-own-prefix\",\n"
            + "              \"valueString\": \"VV\"\n"
            + "            }\n"
            + "          ]\n"
            + "        }\n"
            + "      },\n"
            + "      \"gender\": \"female\",\n"
            + "      \"period\": {\n"
            + "        \"start\": \"2012\"\n"
            + "      },\n"
            + "      \"address\": {\n"
            + "        \"use\": \"home\",\n"
            + "        \"city\": \"PleasantVille\",\n"
            + "        \"line\": [\n"
            + "          \"534 Erewhon St\"\n"
            + "        ],\n"
            + "        \"type\": \"both\",\n"
            + "        \"state\": \"Vic\",\n"
            + "        \"period\": {\n"
            + "          \"start\": \"1974-12-25\"\n"
            + "        },\n"
            + "        \"district\": \"Rainbow\",\n"
            + "        \"postalCode\": \"3999\"\n"
            + "      },\n"
            + "      \"telecom\": [\n"
            + "        {\n"
            + "          \"value\": \"+33 (237) 998327\",\n"
            + "          \"system\": \"phone\"\n"
            + "        }\n"
            + "      ],\n"
            + "      \"relationship\": [\n"
            + "        {\n"
            + "          \"coding\": [\n"
            + "            {\n"
            + "              \"code\": \"N\",\n"
            + "              \"system\": \"http://hl7.org/fhir/v2/0131\"\n"
            + "            }\n"
            + "          ]\n"
            + "        }\n"
            + "      ]\n"
            + "    }\n"
            + "  ]\n"
            + "}";
    IBaseResource resource = fhirContext.newJsonParser().parseResource(patientJson);
    return (Patient) resource;
  }

  private static Bundle createBundle() {
    String json =
        "{\n"
            + "          \"resourceType\": \"Bundle\",\n"
            + "          \"type\": \"collection\",\n"
            + "          \"entry\": [\n"
            + "            {\n"
            + "              \"resource\": {\n"
            + "                \"resourceType\": \"Patient\",\n"
            + "                \"id\": \"patient-1\",\n"
            + "                \"name\": [\n"
            + "                  {\n"
            + "                    \"family\": \"Smith\",\n"
            + "                    \"given\": [\"Jane\"]\n"
            + "                  }\n"
            + "                ],\n"
            + "                \"gender\": \"female\"\n"
            + "              }\n"
            + "            },\n"
            + "            {\n"
            + "              \"resource\": {\n"
            + "                \"resourceType\": \"Patient\",\n"
            + "                \"id\": \"patient-2\",\n"
            + "                \"name\": [\n"
            + "                  {\n"
            + "                    \"family\": \"Doe\",\n"
            + "                    \"given\": [\"John\"]\n"
            + "                  }\n"
            + "                ],\n"
            + "                \"gender\": \"male\"\n"
            + "              }\n"
            + "            },\n"
            + "            {\n"
            + "              \"resource\": {\n"
            + "                \"resourceType\": \"Observation\",\n"
            + "                \"id\": \"obs-1\",\n"
            + "                \"status\": \"final\"\n"
            + "              }\n"
            + "            }\n"
            + "          ]\n"
            + "        }";
    IBaseResource resource = fhirContext.newJsonParser().parseResource(json);
    return (Bundle) resource;
  }
}
