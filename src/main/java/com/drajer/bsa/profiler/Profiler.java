package com.drajer.bsa.profiler;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Lightweight nested profiler utility. Use Profiler.get().step("Name") in a try-with-resources
 * block to measure nested operations. The profiler can be enabled/disabled using the system
 * property "profiler.enabled" (true/false). It aggregates per-root runs for batch reporting.
 */
public class Profiler {

  private static final Logger logger = LoggerFactory.getLogger(Profiler.class);

  private static final Profiler INSTANCE = new Profiler();

  // Thread local current node for nested steps
  private final ThreadLocal<TimingNode> current = new ThreadLocal<>();

  // Collect root runs across patients for aggregate stats
  private final List<TimingNode> roots = Collections.synchronizedList(new LinkedList<>());

  // Aggregate durations per operation name
  private final Map<String, List<Long>> aggregates = new ConcurrentHashMap<>();

  private final boolean enabled;

  private Profiler() {
    String prop = System.getProperty("profiler.enabled", "true");
    this.enabled = Boolean.parseBoolean(prop);
    logger.info("Profiler enabled: {} (system property profiler.enabled)", this.enabled);
  }

  public static Profiler get() {
    return INSTANCE;
  }

  public boolean isEnabled() {
    return enabled;
  }

  public Step step(String name) {
    if (!enabled) return Step.NOOP;

    TimingNode parent = current.get();
    TimingNode node = new TimingNode(name, parent);
    if (parent != null) parent.children.add(node);
    current.set(node);
    return new Step(node);
  }

  void finishRoot(TimingNode root) {
    if (root == null) return;
    roots.add(root);

    // update aggregates
    List<TimingNode> all = flatten(root);
    for (TimingNode n : all) {
      aggregates
          .computeIfAbsent(n.name, k -> Collections.synchronizedList(new ArrayList<>()))
          .add(n.elapsedMillis());
    }
  }

  private List<TimingNode> flatten(TimingNode root) {
    List<TimingNode> list = new ArrayList<>();
    LinkedList<TimingNode> q = new LinkedList<>();
    q.add(root);
    while (!q.isEmpty()) {
      TimingNode n = q.removeFirst();
      list.add(n);
      if (n.children != null) q.addAll(n.children);
    }
    return list;
  }

  public String reportString(TimingNode root) {
    StringBuilder sb = new StringBuilder();
    sb.append("===============================\n");
    sb.append("Performance Report\n");
    sb.append("===============================\n");

    if (root == null) return sb.toString();

    long total = root.elapsedMillis();
    formatNode(sb, root, 0, total);

    sb.append('\n');
    // Top 10 slowest operations
    List<TimingNode> all = flatten(root);
    all.sort(Comparator.comparingLong(TimingNode::elapsedMillis).reversed());
    sb.append("Top 10 slowest operations:\n");
    int limit = Math.min(10, all.size());
    for (int i = 0; i < limit; i++) {
      TimingNode n = all.get(i);
      sb.append(String.format("%d) %s - %s\n", i + 1, n.name, formatMillis(n.elapsedMillis())));
    }

    sb.append('\n');
    sb.append(String.format("Total execution time: %s\n", formatMillis(total)));

    sb.append('\n');
    sb.append("Operations taking more than 500 ms:\n");
    for (TimingNode n : all) {
      if (n.elapsedMillis() > 500)
        sb.append(String.format("%s - %s\n", n.name, formatMillis(n.elapsedMillis())));
    }

    sb.append('\n');
    sb.append("Operations taking more than 1 second:\n");
    for (TimingNode n : all) {
      if (n.elapsedMillis() > 1000)
        sb.append(String.format("%s - %s\n", n.name, formatMillis(n.elapsedMillis())));
    }

    return sb.toString();
  }

  private void formatNode(StringBuilder sb, TimingNode n, int depth, long total) {
    String indent = "";
    for (int i = 0; i < depth; i++) indent += "   ";

    sb.append(indent).append(n.name).append('\n');
    sb.append(indent).append(formatMillis(n.elapsedMillis())).append('\n');

    for (TimingNode c : n.children) {
      formatNode(sb, c, depth + 1, total);
    }
  }

  private String formatMillis(long ms) {
    if (ms >= 1000) return String.format("%.3f sec", ms / 1000.0);
    else return String.format("%d ms", ms);
  }

  public void logReport(TimingNode root) {
    if (!enabled) return;
    String s = reportString(root);
    logger.info(s);
    finishRoot(root);
  }

  public void logAggregateReport() {
    if (!enabled) return;
    StringBuilder sb = new StringBuilder();
    sb.append("===============================\n");
    sb.append("Aggregate Performance Report\n");
    sb.append("===============================\n");

    sb.append(String.format("Total runs: %d\n\n", roots.size()));

    // compute averages
    List<Map.Entry<String, List<Long>>> entries = new ArrayList<>(aggregates.entrySet());
    entries.sort((a, b) -> Long.compare(avg(b.getValue()), avg(a.getValue())));

    sb.append("Top operations by average duration:\n");
    int i = 1;
    for (Map.Entry<String, List<Long>> e : entries) {
      sb.append(
          String.format(
              "%d) %s - avg: %s (runs: %d)\n",
              i++, e.getKey(), formatMillis(avg(e.getValue())), e.getValue().size()));
      if (i > 20) break;
    }

    logger.info(sb.toString());
  }

  private static long avg(List<Long> l) {
    if (l == null || l.isEmpty()) return 0;
    long s = 0;
    for (Long v : l) s += v;
    return s / l.size();
  }

  // AutoCloseable step wrapper
  public static class Step implements AutoCloseable {

    private final TimingNode node;

    private static final Step NOOP = new Step();

    private Step() {
      this.node = null;
    }

    Step(TimingNode node) {
      this.node = node;
    }

    @Override
    public void close() {
      if (node == null) return;
      node.close();
    }
  }

  // Timing node
  static class TimingNode {
    final String name;
    final Instant start;
    Instant end;
    final TimingNode parent;
    final List<TimingNode> children = new ArrayList<>();

    TimingNode(String name, TimingNode parent) {
      this.name = name;
      this.parent = parent;
      this.start = Instant.now();
    }

    void close() {
      this.end = Instant.now();
      // if this is root (parent == null), clear thread current
      if (parent == null) {
        Profiler.get().current.remove();
        Profiler.get().finishRoot(this);
        // Automatically log per-run report when a root closes
        try {
          logger.info(Profiler.get().reportString(this));
        } catch (Exception e) {
          logger.error("Error printing profiler report", e);
        }
      } else {
        Profiler.get().current.set(parent);
      }
    }

    long elapsedMillis() {
      Instant e = (end != null) ? end : Instant.now();
      return java.time.Duration.between(start, e).toMillis();
    }
  }
}
