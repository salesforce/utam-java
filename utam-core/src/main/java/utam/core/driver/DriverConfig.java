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
 * driver configuration including timeouts
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class DriverConfig {

  public static final Duration DEFAULT_IMPLICIT_TIMEOUT = Duration.ZERO;
  public static final Duration DEFAULT_EXPLICIT_TIMEOUT = Duration.ofSeconds(20);
  public static final Duration DEFAULT_EXPLICIT_TIMEOUT_MOCK = Duration.ofSeconds(1);
  public static final Duration DEFAULT_POLLING_INTERVAL = Duration.ofMillis(200);

  private final Duration implicitTimeout;
  private final Duration explicitTimeout;
  private final Duration pollingInterval;

  public DriverConfig(
      Duration implicitTimeout,
      Duration explicitTimeout,
      Duration pollingInterval) {
    this.implicitTimeout = implicitTimeout;
    this.explicitTimeout = explicitTimeout;
    this.pollingInterval = pollingInterval;
  }

  public DriverConfig() {
    this(DEFAULT_IMPLICIT_TIMEOUT, DEFAULT_EXPLICIT_TIMEOUT, DEFAULT_POLLING_INTERVAL);
  }

  public Duration getImplicitTimeout() {
    return implicitTimeout;
  }

  public Duration getExplicitTimeout() {
    return explicitTimeout;
  }

  public Duration getPollingInterval() {
    return pollingInterval;
  }
}
