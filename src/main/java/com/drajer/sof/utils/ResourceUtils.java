package com.drajer.sof.utils;

import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ResourceUtils {
  private static final Logger logger = LoggerFactory.getLogger(ResourceUtils.class);

  public static <T extends IBaseResource> List<T> deduplicate(Collection<T> resources) {
    Map<String, List<T>> groupedById =
        resources.stream()
            .collect(
                Collectors.groupingBy(
                    x -> x.getIdElement().getResourceType() + "/" + x.getIdElement().getIdPart(),
                    Collectors.toList()));

    List<T> sorted =
        groupedById.entrySet().stream()
            .map(
                x ->
                    x.getValue().stream()
                        .max(
                            Comparator.comparingInt(
                                y ->
                                    y.getMeta() != null && y.getMeta().getVersionId() != null
                                        ? Integer.parseInt(
                                            y.getMeta().getVersionId().substring(0, 1))
                                        : 0))
                        .get())
            .collect(Collectors.toList());

    logger.debug("Sorted Resource List :{}", sorted);
    return sorted;
  }
}
