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

  /**
   * The default implicit wait timeout of zero
   */
  public static final Duration DEFAULT_IMPLICIT_TIMEOUT = Duration.ZERO;

  /**
   * The default explicit wait timeout of 20 seconds
   */
  public static final Duration DEFAULT_EXPLICIT_TIMEOUT = Duration.ofSeconds(20);

  /**
   * The default explicit wait timeout for mock objects of 1 second
   */
  public static final Duration DEFAULT_EXPLICIT_TIMEOUT_MOCK = Duration.ofSeconds(1);

  /**
   * The default polling interval of 200 milliseconds
   */
  public static final Duration DEFAULT_POLLING_INTERVAL = Duration.ofMillis(200);

  private final Duration implicitTimeout;
  private final Duration explicitTimeout;
  private final Duration pollingInterval;

  /**
   * Initializes a new instance of the DriverConfig class
   *
   * @param implicitTimeout the implicit wait timeout for finding elements
   * @param explicitTimeout the explicit wait timeout for actions
   * @param pollingInterval the polling interval used to poll for conditions
   */
  public DriverConfig(
      Duration implicitTimeout,
      Duration explicitTimeout,
      Duration pollingInterval) {
    this.implicitTimeout = implicitTimeout;
    this.explicitTimeout = explicitTimeout;
    this.pollingInterval = pollingInterval;
  }

  /**
   * Initializes a new instance of the DriverConfig class
   */
  public DriverConfig() {
    this(DEFAULT_IMPLICIT_TIMEOUT, DEFAULT_EXPLICIT_TIMEOUT, DEFAULT_POLLING_INTERVAL);
  }

  /**
   * Gets the implicit wait timeout
   *
   * @return the implicit wait timeout
   */
  public Duration getImplicitTimeout() {
    return implicitTimeout;
  }

  /**
   * Gets the explicit wait timeout
   *
   * @return the explicit wait timeout
   */
  public Duration getExplicitTimeout() {
    return explicitTimeout;
  }

  /**
   * Gets the polling interval
   *
   * @return the polling interval
   */
  public Duration getPollingInterval() {
    return pollingInterval;
  }
}
