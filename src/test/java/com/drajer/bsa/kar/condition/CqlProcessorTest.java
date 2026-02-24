package com.drajer.bsa.kar.condition;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.times;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.BsaCondition;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.test.util.TestUtils;
import org.hl7.fhir.r4.model.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.opencds.cqf.fhir.cr.cpg.r4.R4LibraryEvaluationService;

@RunWith(MockitoJUnitRunner.class)
public class CqlProcessorTest {
  @Mock private EhrQueryService ehrQueryService;
  @Mock private BsaCondition bsaCondition;
  @Mock private BsaCqlCondition bsaCqlCondition;
  @Mock private BsaAction bsaAction;
  @Mock private KarProcessingData karProcessingData;

  @Mock private R4LibraryEvaluationService libraryExecutionService;

  @InjectMocks private CqlProcessor cqlProcessor;

  @Mock private Expression expression;

  static final String ENCOUNTER_BUNDLE_JSON = "R4/Encounter/EncounterBundle_97953900.json";

  @Before
  public void setUp() {
    Mockito.lenient().when(bsaCqlCondition.getLogicExpression()).thenReturn(expression);
    Mockito.lenient().when(expression.getExpression()).thenReturn("mockExpression");
    Mockito.lenient().when(bsaCqlCondition.getPatientId()).thenReturn("patient123");
  }

  @Test
  public void testEvaluateExpression_notImplemented() {
    Parameters parameters = new Parameters();
    Boolean result = cqlProcessor.evaluateExpression(bsaCqlCondition, parameters, ehrQueryService);
    assertFalse(result);
  }

  @Test
  public void testEvaluateExpression_success() {
    Bundle bundle = TestUtils.loadBundleFromFile(ENCOUNTER_BUNDLE_JSON);
    Mockito.when(karProcessingData.getNotificationBundle()).thenReturn(bundle);
    Mockito.when(karProcessingData.getInputResourcesAsBundle()).thenReturn(bundle);

    Endpoint endpoint = new Endpoint();
    endpoint.setAddress("http://mock-endpoint");

    Mockito.when(bsaCqlCondition.getLibraryEndpoint()).thenReturn(endpoint);
    Mockito.when(bsaCqlCondition.getTerminologyEndpoint()).thenReturn(endpoint);
    Mockito.when(bsaCqlCondition.getDataEndpoint()).thenReturn(endpoint);

    Parameters resultParams = new Parameters();
    resultParams.addParameter().setName("mockExpression").setValue(new BooleanType(true));

    Mockito.when(
            libraryExecutionService.evaluate(
                Mockito.any(IdType.class),
                Mockito.anyString(),
                Mockito.anyList(),
                Mockito.any(Parameters.class),
                Mockito.any(Bundle.class),
                Mockito.isNull(),
                Mockito.any(),
                Mockito.any(),
                Mockito.any()))
        .thenReturn(resultParams);

    Boolean result =
        cqlProcessor.evaluateExpression(
            bsaCqlCondition, bsaAction, karProcessingData, ehrQueryService);

    assertTrue(result);

    Mockito.verify(bsaCqlCondition).getLibraryEndpoint();
    Mockito.verify(bsaCqlCondition).getTerminologyEndpoint();
    Mockito.verify(bsaCqlCondition).getDataEndpoint();
    Mockito.verify(bsaCqlCondition, times(2)).getLogicExpression();
    Mockito.verify(expression, times(2)).getExpression();
  }
}
