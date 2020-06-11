package com.drajer.ecrapp.encryptor;

import java.security.InvalidKeyException;
import java.util.Base64;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.spec.SecretKeySpec;
import javax.persistence.AttributeConverter;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

@Component
public class ColumnEncryptor implements AttributeConverter<String, String>, InitializingBean {

	private static final String AES = "AES";

	@Autowired
	private Environment environment;

	private String SECRET;
	
	public static final String DECRYPT_ERROR = "DECRYPT_ERROR";

	private final Cipher cipher;

	public ColumnEncryptor() throws Exception {
		cipher = Cipher.getInstance(AES);
	}

	@Override
	public String convertToDatabaseColumn(String attribute) {
		try {
			if (StringUtils.isNotBlank(attribute)) {
				cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(SECRET.getBytes(), AES));
				return Base64.getEncoder().encodeToString(cipher.doFinal(attribute.getBytes()));
			} else {
				return attribute;
			}
		} catch (IllegalBlockSizeException | BadPaddingException | InvalidKeyException e) {
			throw new IllegalStateException(e);
		}
	}

	@Override
	public String convertToEntityAttribute(String dbData) {
		try {
			if (StringUtils.isNotBlank(dbData)) {
				cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(SECRET.getBytes(), AES));
				return new String(cipher.doFinal(Base64.getDecoder().decode(dbData)));
			} else {
				return dbData;
			}
		} catch (InvalidKeyException | BadPaddingException | IllegalBlockSizeException e) {
			return DECRYPT_ERROR + dbData;
			// throw new IllegalStateException(e);
		}
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		SECRET = environment.getRequiredProperty("secret-key");
		if (StringUtils.isBlank(SECRET)) {
			throw new RuntimeException("secret-key in application.properties can't be blank ");
		}
		if (StringUtils.isNotBlank(SECRET) && StringUtils.length(SECRET) != 16) {
			throw new RuntimeException("secret-key in application.properties should be 16 characters");
		}
	}
}