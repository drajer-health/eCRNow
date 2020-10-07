package com.drajer.test.util;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import org.apache.commons.io.IOUtils;
import org.hl7.v3.POCDMT000040ClinicalDocument;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.ecrapp.model.Eicr;

public class ValidationUtils {

	private static final Logger logger = LoggerFactory.getLogger(ValidationUtils.class);

	public static POCDMT000040ClinicalDocument getClinicalDocXml(Eicr eicr) {

		JAXBContext jaxbContext;
		Unmarshaller jaxbUnmarshaller;
		POCDMT000040ClinicalDocument clinicalDoc = null;
		try {
			jaxbContext = JAXBContext.newInstance("org.hl7.v3:org.hl7.sdtc");
			jaxbUnmarshaller = jaxbContext.createUnmarshaller();

			Source source = new StreamSource(IOUtils.toInputStream(eicr.getData()));

			JAXBElement<POCDMT000040ClinicalDocument> root = jaxbUnmarshaller.unmarshal(source,
					POCDMT000040ClinicalDocument.class);

			clinicalDoc = root.getValue();
		} catch (JAXBException e) {
			logger.error("Error in unmarshalling EIRC. " + e.getMessage());
		}

		return clinicalDoc;

	}

	public static void validateAddress() {

	}

	public static void validateCode() {

	}

	public static void validateIdentifier() {

	}
	public static void validateTelecom() {

	}

}
