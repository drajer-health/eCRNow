package com.drajer.bsa.ehr.customizations;

import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 *
 * <h1>SampleCustomization</h1>
 *
 * This is an example class that shows that it can be used for using Spring Annotations for any
 * customizations required by Implementers (EHR vendors). This package is already scanned by the
 * base configuration of the application for spring annotations. This avoids vendors creating new
 * packages and modifying the AppConfiguration to scan the new pacakges.
 *
 * @author nbashyam
 */
@Component
@Service
@Transactional
public class SampleCustomization {}
