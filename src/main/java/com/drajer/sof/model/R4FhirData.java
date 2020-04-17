package com.drajer.sof.model;

import org.hl7.fhir.r4.model.Bundle;

public class R4FhirData extends FhirData {

	private Bundle data;

	public Bundle getData() {
		return data;
	}

	public void setData(Bundle data) {
		this.data = data;
	}
}
