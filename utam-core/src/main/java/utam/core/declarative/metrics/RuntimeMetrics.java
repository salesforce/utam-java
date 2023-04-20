package utam.core.declarative.metrics;

public interface RuntimeMetrics {
  void report(Object ...objects);

  void auth();
}
