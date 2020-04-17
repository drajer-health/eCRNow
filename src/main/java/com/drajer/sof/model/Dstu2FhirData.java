package com.drajer.sof.model;

import ca.uhn.fhir.model.dstu2.resource.Bundle;

public class Dstu2FhirData extends FhirData {

	private Bundle  data;

	public Bundle getData() {
		return data;
	}

	public void setData(Bundle data) {
		this.data = data;
	}
	
	
}
