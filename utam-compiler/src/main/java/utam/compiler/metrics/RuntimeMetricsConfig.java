package utam.compiler.metrics;

import utam.core.declarative.metrics.RuntimeMetrics;

public class RuntimeMetricsConfig implements RuntimeMetrics {

  private final String metricsUser;

  public RuntimeMetricsConfig(String metricsUser) {
    this.metricsUser = metricsUser;
  }

  @Override
  public void report(Object... objects) {
    // todo
    String metricsStr = objects!= null? "Metrics" : "Metrics: nothing to report";
    System.out.println(metricsStr);
  }

  @Override
  public void auth() {
    // todo
  }
}
