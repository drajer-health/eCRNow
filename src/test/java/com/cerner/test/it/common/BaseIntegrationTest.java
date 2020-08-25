package com.cerner.test.it.common;

import java.io.IOException;
import java.lang.reflect.Method;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.junit.After;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.HttpHeaders;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.transaction.annotation.Transactional;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.fasterxml.jackson.databind.ObjectMapper;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@ContextConfiguration(classes = SpringConfiguration.class)
@AutoConfigureMockMvc
@Transactional
@ActiveProfiles("test")
public abstract class BaseIntegrationTest {
	
	@LocalServerPort
	protected int port;

	@Autowired
	protected MockMvc mockMvc;

	@Autowired
	protected SessionFactory sessionFactory;
	protected Session session = null;
	protected Transaction tx = null;

	protected TestRestTemplate restTemplate = new TestRestTemplate();
	protected HttpHeaders headers = new HttpHeaders();

	protected static ObjectMapper mapper = new ObjectMapper();
	
	protected static final String URL = "http://localhost:";
	
	@Before
	public void setUp() throws IOException {
		
		session = sessionFactory.openSession();
	}
	
	@After
	public void tearDown() {
		session.close();
	}
	

	protected Object invokePrivateMethod(String className, String methodName,
			Class<?> parameterType, Object singleParameterValue) {
		Object invokeClazz = null;
		Class<?>[] paramString = new Class[1];
		paramString[0] = parameterType;
		invokeClazz = invokePrivateMethod(className, methodName, paramString,
				singleParameterValue);
		return invokeClazz;
	}

	
	protected Object invokePrivateMethod(String className, String methodName,
			Class<?>[] parameterType, Object... parameterValues) {

		Object invoke = null;
		try {
			Class<?> clazz = Class.forName(className);
			Method declaredMethod = null;
			// stop when we got method or reached top of class hierarchy 
			while (declaredMethod == null && clazz != null) {
				try {
					declaredMethod = clazz.getDeclaredMethod(methodName,
							parameterType);
				} catch (NoSuchMethodException e) {
					
					/* only get super-class when we couldn't find method in the
					 current class*/
					 
					clazz = clazz.getSuperclass();
				}
			}

			
			 /*Check if the method is found once we are done walking to the top
			 of class hierarchy*/
			 
			if (declaredMethod == null) {
				// Did not find the method 
				System.out
						.println("No such method found in the entire class hierarchy");
			} else {
				// Found the method 
				declaredMethod.setAccessible(true);
				Object objInstance = Class.forName(className).newInstance();
				invoke = declaredMethod.invoke(objInstance, parameterValues);
			}
		} catch (Exception e) {
			e.printStackTrace();

			Throwable t = e.getCause();
			/*String ErrorKey = ((SystemException) t).getErrorKeysList().get(0);
			String ErrorMessage = e.getCause().getLocalizedMessage();
			throw new SystemException(ErrorKey, ErrorMessage);*/
		}
		return invoke;
	}

}