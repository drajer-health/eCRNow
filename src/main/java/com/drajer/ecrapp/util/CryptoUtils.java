package com.drajer.ecrapp.util;

import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;

public class CryptoUtils {

  // java:S2119: SecureRandom is thread-safe and expensive to seed, so one shared instance is
  // created once and reused for every nonce instead of a new instance per call.
  private static final SecureRandom SECURE_RANDOM = new SecureRandom();

  public static byte[] getRandomNonce(int numBytes) {
    byte[] nonce = new byte[numBytes];
    SECURE_RANDOM.nextBytes(nonce);
    return nonce;
  }

  // AES secret key
  public static SecretKey getAESKey(int keysize) throws NoSuchAlgorithmException {
    KeyGenerator keyGen = KeyGenerator.getInstance("AES");
    keyGen.init(keysize, SecureRandom.getInstanceStrong());
    return keyGen.generateKey();
  }
}
