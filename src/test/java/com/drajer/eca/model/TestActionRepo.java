package com.drajer.eca.model;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.junit.MockitoJUnitRunner;


@RunWith(MockitoJUnitRunner.class)
public class TestActionRepo {

	@InjectMocks
	private ActionRepo actionRepo;
	
	@Test
	public void testSetupTriggerBasedActions() throws Exception {
		actionRepo.setupTriggerBasedActions();
	}

}
