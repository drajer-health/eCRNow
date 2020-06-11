package com.drajer.ecrapp.listener;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.stereotype.Component;

import com.drajer.sof.service.ClientDetailsService;
import com.drajer.sof.service.LaunchService;

@Component
public class EcrAppStartupListener implements ApplicationListener<ContextRefreshedEvent> {

	@Autowired
	ClientDetailsService clientDetailsService;

	@Autowired
	LaunchService launchService;

	public static final String DECRYPT_ERROR = "DECRYPT_ERROR";

	private final Logger logger = LoggerFactory.getLogger(EcrAppStartupListener.class);

	@Override
	public void onApplicationEvent(ContextRefreshedEvent event) {

		clientDetailsService.getAllClientDetails().forEach(clientDetail -> {
			if (StringUtils.startsWithAny(clientDetail.getDirectPwd(), DECRYPT_ERROR)) {
				clientDetail.setDirectPwd(clientDetail.getDirectPwd().replace(DECRYPT_ERROR, ""));
				logger.info(" Password encrypted for Client Id " + clientDetail.getClientId());
				clientDetailsService.saveOrUpdate(clientDetail);
			}
		});

		launchService.getAllLaunchDetails().forEach(launchDetail -> {
			if (StringUtils.startsWithAny(launchDetail.getDirectPwd(), DECRYPT_ERROR)) {
				launchDetail.setDirectPwd(launchDetail.getDirectPwd().replace(DECRYPT_ERROR, ""));
				logger.info(" Password encrypted for Launch Client Id " + launchDetail.getClientId());
				launchService.saveOrUpdate(launchDetail);
			}
		});

	}

}
