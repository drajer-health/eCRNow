package com.drajer.sampleehr.controller;

public class Eicr {
    private String fhirServerUrl;
    private String patientId;
    private String encounterId;
    private String submittedMessageVersionId;
    private String payload;

    public String getFhirServerUrl() {
        return fhirServerUrl;
    }

    public void setFhirServerUrl(String fhirServerUrl) {
        this.fhirServerUrl = fhirServerUrl;
    }

    public String getPatientId() {
        return patientId;
    }

    public void setPatientId(String patientId) {
        this.patientId = patientId;
    }

    public String getEncounterId() {
        return encounterId;
    }

    public void setEncounterId(String encounterId) {
        this.encounterId = encounterId;
    }

    public String getSubmittedMessageVersionId() {
        return submittedMessageVersionId;
    }

    public void setSubmittedMessageVersionId(String submittedMessageVersionId) {
        this.submittedMessageVersionId = submittedMessageVersionId;
    }

    public String getPayload() {
        return payload;
    }

    public void setPayload(String payload) {
        this.payload = payload;
    }
}