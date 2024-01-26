/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.driver;

/**
 * Supported driver types used in Driver factory utilities to create instance of the driver
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public enum DriverType {
  /** driver is a web browser */
  web,

  /** driver is for iOS applications */
  ios,

  /** driver is for Android applications */
  android,

  /** driver is for the Chrome browser */
  chrome,

  /** driver is for the Firefox browser */
  firefox
}
