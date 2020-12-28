package com.drajer.ecrapp.security;

import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import javax.annotation.PostConstruct;
import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import org.apache.commons.codec.binary.Base64;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

@Service
public class AESEncryption {

  private static final Logger logger = LoggerFactory.getLogger(AESEncryption.class);

  @Autowired private Environment environment;

  private static String secretKey;

  private static IvParameterSpec iv;

  private static SecretKeySpec skeySpec;

  public static String encrypt(String value) {
    try {

      Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
      cipher.init(Cipher.ENCRYPT_MODE, skeySpec, iv);

      byte[] encrypted = cipher.doFinal(value.getBytes());

      return Base64.encodeBase64String(encrypted);
    } catch (Exception ex) {
      logger.error("Error while encrypting:", ex);
    }

    return null;
  }

  public static String decrypt(String encrypted) {
    try {

      Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
      cipher.init(Cipher.DECRYPT_MODE, skeySpec, iv);

      byte[] original = cipher.doFinal(Base64.decodeBase64(encrypted));

      return new String(original);
    } catch (Exception ex) {
      logger.error("Error while decrypting:", ex);
    }

    return null;
  }

  @PostConstruct
  public void getSecretKey() {
    try {
      secretKey = environment.getRequiredProperty("security.key");
      byte[] keyBytes = secretKey.getBytes("UTF-16");
      iv = new IvParameterSpec(Arrays.copyOf(keyBytes, 16));
      skeySpec = new SecretKeySpec(Arrays.copyOf(keyBytes, 16), "AES");
    } catch (UnsupportedEncodingException e) {
      logger.error("Error while converting String to Bytes:", e);
    }
  }
}
