package utam.core.framework.consumer;

import java.time.Duration;

/**
 * Implementation for all UI timeouts
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class UtamTimeoutsImpl implements UtamTimeouts {

  private final static Duration DEFAULT_TIMEOUT = Duration.ofSeconds(20);
  private final static Duration DEFAULT_POLLING_INTERVAL = Duration.ofMillis(500);

  private Duration findTimeout;
  private Duration waitForTimeout;
  private Duration fluentTimeout;
  private Duration pollingInterval;

  UtamTimeoutsImpl(Duration findTimeout, Duration waitForTimeout, Duration pollingInterval) {
    this.findTimeout = findTimeout;
    this.waitForTimeout = waitForTimeout;
    this.fluentTimeout = waitForTimeout;
    this.pollingInterval = pollingInterval;
  }

  public UtamTimeoutsImpl() {
    this(DEFAULT_TIMEOUT, DEFAULT_TIMEOUT, DEFAULT_POLLING_INTERVAL);
  }

  @Override
  public Duration getFindTimeout() {
    return findTimeout;
  }

  @Override
  public Duration getWaitForTimeout() {
    return waitForTimeout;
  }

  @Override
  public Duration getPollingInterval() {
    return pollingInterval;
  }

  @Override
  @Deprecated
  public Duration getFluentWaitTimeout() {
    return fluentTimeout;
  }

  @Override
  public void setFindTimeout(Duration findTimeout) {
    this.findTimeout = findTimeout;
  }

  @Override
  public void setWaitForTimeout(Duration waitForTimeout) {
    this.waitForTimeout = waitForTimeout;
    // todo - remove this when parity achieved
    this.findTimeout = waitForTimeout;
  }

  @Override
  public void setWaitForPollingInterval(Duration pollingInterval) {
    this.pollingInterval = pollingInterval;
  }
}
