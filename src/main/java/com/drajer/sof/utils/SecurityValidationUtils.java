package com.drajer.sof.utils;

import java.net.URI;

public final class SecurityValidationUtils {

  private SecurityValidationUtils() {}

  public static boolean validateUrl(String url) {

    if (url == null || url.isBlank()) {
      return false;
    }

    try {
      URI uri = new URI(url);

      if (!uri.isAbsolute()) {
        return false;
      }

      String scheme = uri.getScheme();

      return "http".equalsIgnoreCase(scheme) || "https".equalsIgnoreCase(scheme);

    } catch (Exception ex) {
      return false;
    }
  }
}
