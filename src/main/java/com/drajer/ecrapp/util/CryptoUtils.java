package com.drajer.ecrapp.util;

import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

public class CryptoUtils {

  public static byte[] getRandomNonce(int numBytes) {
    byte[] nonce = new byte[numBytes];
    new SecureRandom().nextBytes(nonce);
    return nonce;
  }

  // AES secret key
  public static SecretKey getAESKey(int keysize) throws NoSuchAlgorithmException {
    KeyGenerator keyGen = KeyGenerator.getInstance("AES");
    keyGen.init(keysize, SecureRandom.getInstanceStrong());
    return keyGen.generateKey();
  }

  // Password derived AES 256 bits secret key
  public static SecretKey getAESKeyFromPassword(char[] password, byte[] salt)
      throws NoSuchAlgorithmException, InvalidKeySpecException {

    SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
    // iterationCount = 65536
    // keyLength = 256
    KeySpec spec = new PBEKeySpec(password, salt, 65536, 256);
    SecretKey secret = new SecretKeySpec(factory.generateSecret(spec).getEncoded(), "AES");
    return secret;
  }
}
