package com.drajer.plandefinition.resource;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Optional;

import javax.annotation.PostConstruct;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.UsageContext;
import org.hl7.fhir.r4.model.ValueSet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.drajer.plandefinition.service.ValueSetService;

import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;

@Service
public class PlandefinitionProcessor {

	@Autowired
	@Qualifier("esrdGenericClient")
	private IGenericClient esrdClient;

	@Autowired
	@Qualifier("jsonParser")
	IParser jsonParser;

	@Autowired
	@Qualifier("valueSetServiceImpl")
	ValueSetService valueSetService;

	@Value("${esrd.file.location}")
	String esrdFileLocation;

	@PostConstruct
	public void initializeClientMethods() {
		processResourceBundle();
	}

	public void processResourceBundle() {

		// Reading Bundle with Id 506 from ersd server.
		// Bundle esrdBundle =
		// esrdClient.read().resource(Bundle.class).withId("506").execute();

		Bundle esrdBundle = readEsrdBundleFromFile();

		if (esrdBundle != null) {
			List<BundleEntryComponent> bundleEntries = esrdBundle.getEntry();
			ValueSet valueSet = null;
			List<UsageContext> usageContextList;
			for (BundleEntryComponent bundleEntry : bundleEntries) {
				if (Optional.ofNullable(bundleEntry).isPresent()) {
					if (bundleEntry.getResource().getResourceType().equals(ResourceType.ValueSet)) {
						valueSet = (ValueSet) bundleEntry.getResource();
						usageContextList = valueSet.getUseContext();

						for (UsageContext usageContext : usageContextList) {
							if (Optional.ofNullable(usageContext).isPresent()) {
								if (usageContext.getValueCodeableConcept() != null) {
									if (usageContext.getValueCodeableConcept().getText().equalsIgnoreCase("COVID-19")) {
										System.out.println("Processing value set with id : " + valueSet.getId());
										valueSetService.createValueSet(valueSet);
									}
								}
							}
						}
					}
				}
			}
		}
	}

	private Bundle readEsrdBundleFromFile() {
		InputStream in = null;
		Bundle bundle = null;
		try {
			in = new FileInputStream(new File(esrdFileLocation));
			if (in != null) {
				bundle = jsonParser.parseResource(Bundle.class, in);
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (in != null) {
				try {
					in.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		return bundle;
	}
}
