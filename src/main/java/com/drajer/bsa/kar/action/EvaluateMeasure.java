package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.KarProcessingData;

import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.r4.model.Endpoint;
import org.hl7.fhir.r4.model.MeasureReport;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EvaluateMeasure extends BsaAction {

    private String expression;

    private MeasureProcessor measureProcessor;

    private Endpoint dataEndpoint;
    private Endpoint terminologyEndpoint;
    private Endpoint libraryEndpoint;

    private String periodStart;
    private String periodEnd;

    private String patientId;
    private String url;

    private IBaseBundle bundle;

    private final Logger logger = LoggerFactory.getLogger(EvaluateMeasure.class);

    public IBaseBundle getBundle() {
        return this.bundle;
    }

    public void setBundle(IBaseBundle bundle) {
        this.bundle = bundle;
    }

    public String getExpression() {
        return expression;
    }

    public void setExpression(String expression) {
        this.expression = expression;
    }

    public Endpoint getDataEndpoint() {
        return this.dataEndpoint;
    }

    public void setDataEndpoint(Endpoint dataEndpoint) {
        this.dataEndpoint = dataEndpoint;
    }

    public Endpoint getTerminologyEndpoint() {
        return this.terminologyEndpoint;
    }

    public void setTerminologyEndpoint(Endpoint terminologyEndpoint) {
        this.terminologyEndpoint = terminologyEndpoint;
    }

    public Endpoint getLibraryEndpoint() {
        return libraryEndpoint;
    }

    public void setLibraryEndpoint(Endpoint libraryEndpoint) {
        this.libraryEndpoint = libraryEndpoint;
    }

    public String getUrl() {
        return this.url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getPatientId() {
        return this.patientId;
    }

    public void setPatientId(String patientId) {
        this.patientId = patientId;
    }

    public MeasureProcessor getMeasureProcessor() {
        return this.measureProcessor;
    }

    public void setMeasureProcessor(MeasureProcessor measureProcessor) {
        this.measureProcessor = measureProcessor;
    }

    public String getPeriodStart() {
        return this.periodStart;
    }

    public void setPeriodStart(String date) {
        this.periodStart = date;
    }

    public String getPeriodEnd() {
        return this.periodEnd;
    }

    public String setPeriodEnd(String date) {
        this.periodEnd = date;
    }

    @Override
    public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrservice) {
          MeasureReport result =
                  this.measureProcessor.evaluateMeasure(
                      this.url,
                      this.periodStart,
                      this.periodEnd,
                      this.patientId,
                      "subject",
                      null, // practitioner
                      null, // received on
                      this.libraryEndpoint,
                      this.terminologyEndpoint,
                      this.dataEndpoint,
                      this.bundle);
    
      
          // what to do with the generated report?
        }
    }
}