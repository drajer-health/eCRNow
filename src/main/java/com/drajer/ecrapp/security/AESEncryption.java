package com.drajer.ecrapp.security;

import com.drajer.ecrapp.util.CryptoUtils;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Base64;
import javax.annotation.PostConstruct;
import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;
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

  private static final String ENCRYPT_ALGO = "AES/GCM/NoPadding";

  private static final int TAG_LENGTH_BIT = 128;
  private static final int IV_LENGTH_BYTE = 12;
  private static final int SALT_LENGTH_BYTE = 16;
  private static final Charset UTF_8 = StandardCharsets.UTF_8;

  public static String encrypt(String pText) {

    try {
      byte[] salt = CryptoUtils.getRandomNonce(SALT_LENGTH_BYTE);

      byte[] iv = CryptoUtils.getRandomNonce(IV_LENGTH_BYTE);

      byte[] keyBytes = secretKey.getBytes("UTF-16");

      SecretKeySpec skeySpec = new SecretKeySpec(Arrays.copyOf(keyBytes, 16), "AES");

      Cipher cipher = Cipher.getInstance(ENCRYPT_ALGO);

      cipher.init(Cipher.ENCRYPT_MODE, skeySpec, new GCMParameterSpec(TAG_LENGTH_BIT, iv));

      byte[] cipherText = cipher.doFinal(pText.getBytes());

      byte[] cipherTextWithIvSalt =
          ByteBuffer.allocate(iv.length + salt.length + cipherText.length)
              .put(iv)
              .put(salt)
              .put(cipherText)
              .array();
      return Base64.getEncoder().encodeToString(cipherTextWithIvSalt);
    } catch (Exception ex) {
      logger.error("Error while encrypting:", ex);
    }

    return null;
  }

  public static String decrypt(String cText) {

    try {
      byte[] decode = Base64.getDecoder().decode(cText.getBytes(UTF_8));

      ByteBuffer bb = ByteBuffer.wrap(decode);

      byte[] iv = new byte[IV_LENGTH_BYTE];
      bb.get(iv);

      byte[] salt = new byte[SALT_LENGTH_BYTE];
      bb.get(salt);

      byte[] cipherText = new byte[bb.remaining()];
      bb.get(cipherText);

      byte[] keyBytes = secretKey.getBytes("UTF-16");

      SecretKeySpec skeySpec = new SecretKeySpec(Arrays.copyOf(keyBytes, 16), "AES");

      Cipher cipher = Cipher.getInstance(ENCRYPT_ALGO);

      cipher.init(Cipher.DECRYPT_MODE, skeySpec, new GCMParameterSpec(TAG_LENGTH_BIT, iv));

      byte[] plainText = cipher.doFinal(cipherText);

      return new String(plainText, UTF_8);
    } catch (Exception ex) {
      logger.error("Error while decrypting:", ex);
    }
    return null;
  }

  @PostConstruct
  public void getSecretKey() {
    secretKey = environment.getRequiredProperty("security.key");
  }
}
