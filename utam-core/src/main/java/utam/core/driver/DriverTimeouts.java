/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.driver;

import java.time.Duration;

/**
 * supported timeouts for UI interactions
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class DriverTimeouts {

  // short timeout for testing with mocks
  public static final DriverTimeouts TEST = new DriverTimeouts(Duration.ofSeconds(1),
      Duration.ofSeconds(1),
      Duration.ofMillis(200));
  static final Duration DEFAULT_TIMEOUT = Duration.ofSeconds(20);
  static final Duration DEFAULT_POLLING_INTERVAL = Duration.ofMillis(500);
  public static final DriverTimeouts DEFAULT = new DriverTimeouts(DEFAULT_TIMEOUT, DEFAULT_TIMEOUT,
      DEFAULT_POLLING_INTERVAL);
  private final Duration findTimeout;
  private final Duration waitForTimeout;
  private final Duration fluentTimeout;
  private final Duration pollingInterval;

  public DriverTimeouts(Duration findTimeout, Duration fluentTimeout, Duration waitForTimeout, Duration pollingInterval) {
    this.findTimeout = findTimeout;
    this.waitForTimeout = waitForTimeout;
    this.fluentTimeout = fluentTimeout;
    this.pollingInterval = pollingInterval;
  }

  public DriverTimeouts(Duration findTimeout, Duration waitForTimeout, Duration pollingInterval) {
    this(findTimeout, Duration.ZERO, waitForTimeout, pollingInterval);
  }

  /**
   * this type of timeout is used to find an element
   *
   * @return duration units
   */
  public Duration getFindTimeout() {
    return findTimeout;
  }

  /**
   * this type of timeout is used for all wait operations, it is NOT applied to interactions like
   * click or getAttribute or setText
   *
   * @return duration units
   */
  public Duration getWaitForTimeout() {
    return waitForTimeout;
  }

  /**
   * polling interval for fluent wait
   *
   * @return duration units
   */
  public Duration getPollingInterval() {
    return pollingInterval;
  }

  /**
   * applied for all interactions that do not wait, like click
   *
   * @return duration units
   * @deprecated should be removed when behavior transition with UTAM-JS is complete
   */
  @Deprecated
  public Duration getFluentWaitTimeout() {
    return fluentTimeout;
  }

  @Override //for tests to compare timeouts
  public boolean equals(Object obj) {
    if (obj instanceof DriverTimeouts) {
      DriverTimeouts tmp = (DriverTimeouts) obj;
      return tmp.getFindTimeout().equals(getFindTimeout())
          && tmp.getPollingInterval().equals(getPollingInterval())
          && tmp.getWaitForTimeout().equals(getWaitForTimeout())
          && tmp.getFluentWaitTimeout().equals(getFluentWaitTimeout());
    }
    return false;
  }
}
