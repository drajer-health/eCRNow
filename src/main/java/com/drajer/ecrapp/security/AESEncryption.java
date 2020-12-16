package com.drajer.ecrapp.security;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.Base64;
import javax.annotation.PostConstruct;
import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

@Component
public class AESEncryption {

  private static final Logger logger = LoggerFactory.getLogger(AESEncryption.class);
  private static SecretKeySpec secretKey;
  private static byte[] key;

  @Autowired private Environment environment;

  private static String secret;

  public static void setKey(String myKey) {
    MessageDigest sha = null;
    try {
      key = myKey.getBytes("UTF-8");
      sha = MessageDigest.getInstance("SHA-1");
      key = sha.digest(key);
      key = Arrays.copyOf(key, 16);
      secretKey = new SecretKeySpec(key, "AES");
    } catch (NoSuchAlgorithmException e) {
      logger.error("Error while setting secret key: {}", e.toString());
    } catch (UnsupportedEncodingException e) {
      logger.error("Error while setting secret key: {}", e.toString());
    }
  }

  public static String encrypt(String strToEncrypt) {
    try {
      setKey(secret);
      Cipher cipher = Cipher.getInstance("AES/ECB/PKCS5Padding");
      cipher.init(Cipher.ENCRYPT_MODE, secretKey);
      return Base64.getEncoder().encodeToString(cipher.doFinal(strToEncrypt.getBytes("UTF-8")));
    } catch (Exception e) {
      logger.error("Error while encrypting: {}", e.toString());
    }
    return null;
  }

  public static String decrypt(String strToDecrypt) {
    try {
      setKey(secret);
      Cipher cipher = Cipher.getInstance("AES/ECB/PKCS5PADDING");
      cipher.init(Cipher.DECRYPT_MODE, secretKey);
      return new String(cipher.doFinal(Base64.getDecoder().decode(strToDecrypt)));
    } catch (Exception e) {
      logger.error("Error while decrypting: " + e.toString());
    }
    return null;
  }

  @PostConstruct
  public void getSecretKey() {
    secret = environment.getRequiredProperty("security.key");
  }
}
