package com.drajer.ecrapp.config;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

@Converter(autoApply = true) // autoApply ensures that this converter is applied globally
public class SetOfStringsConverter implements AttributeConverter<Set<String>, String> {

  @Override
  public String convertToDatabaseColumn(Set<String> attribute) {
    if (attribute == null) {
      return null;
    }
    return String.join("||", attribute); // Use '||' as separator
  }

  @Override
  public Set<String> convertToEntityAttribute(String dbData) {
    if (dbData == null) {
      return null;
    }
    return new HashSet<>(Arrays.asList(dbData.split("||")));
  }
}
