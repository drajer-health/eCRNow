package com.drajer.eca.model;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import com.drajer.cda.CdaEicrGenerator;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.service.TriggerQueryService;

import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;
import ca.uhn.fhir.model.dstu2.resource.Patient;

@Service
public class CreateEicrAction extends AbstractAction {

	private final Logger logger = LoggerFactory.getLogger(CreateEicrAction.class);
	
	@Autowired
	TriggerQueryService triggerQueryService;
	
	@Autowired
	LoadingQueryService loadingQueryService;
	
	@Override
	public void print() {
		
		logger.info(" **** Printing CreateEicrAction **** ");
		printBase();
		logger.info(" **** End Printing CreateEicrAction **** ");
	}
	
	@Override
	public void execute(Object obj) {

		if(obj instanceof LaunchDetails) {
			
			logger.info(" Obtained Launch Details ");
			LaunchDetails details = (LaunchDetails)obj;
			
			DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
			Date start = null;
			Date end = null;
			
			try {
				start = formatter.parse("2019-02-13");
				end = formatter.parse("2019-02-14");
				
			} catch (ParseException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			
			// Call the Loading Queries.
			if(loadingQueryService != null ) { 
				
			logger.info(" Getting necessary data from Loading Queries ");
			FhirData data = loadingQueryService.getData(details, start, end);
			
			String eICR = null;
			
			if(data != null && data instanceof Dstu2FhirData) {
				
				Dstu2FhirData dstu2Data = (Dstu2FhirData)data;
				eICR = CdaEicrGenerator.convertDstu2FhirBundletoCdaEicr(dstu2Data, details);
				
				
			}
				
			logger.info(" ************ START EiCR ************ " + "\n");
				
			logger.info(eICR);
				
			logger.info(" ************ END EiCR ************ " + "\n");
				
		}
			
			
		}
	}

}
