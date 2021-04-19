/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.driver;


/**
 * driver related settings such as timeouts and bridge app title
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class DriverContext {

  /**
   * setting used in unit tests with mocks
   */
  public static final DriverContext TEST = new DriverContext(DriverTimeouts.TEST, "");
  /**
   * context with default timeouts
   */
  public static final DriverContext DEFAULT = new DriverContext(DriverTimeouts.DEFAULT, "");

  private final DriverTimeouts timeouts;
  private final String bridgeAppTitle;

  public DriverContext(DriverTimeouts timeouts, String bridgeAppTitle) {
    this.timeouts = timeouts;
    this.bridgeAppTitle = bridgeAppTitle;
  }

  /**
   * get configured timeouts
   *
   * @return timeouts
   */
  public DriverTimeouts getTimeouts() {
    return timeouts;
  }

  /**
   * get WebView page with given title
   *
   * @return the title of the WebView page
   */
  public String getBridgeAppTitle() {
    return bridgeAppTitle;
  }
}
