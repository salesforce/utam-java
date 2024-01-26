/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.factory;

import io.appium.java_client.remote.MobileCapabilityType;

/**
 * The extended Appium Capabilities
 *
 * @author qren
 * @since 230
 */
@SuppressWarnings("WeakerAccess")
public interface AppiumCustomCapabilityType extends MobileCapabilityType {
  /** Enable "real", non-javascript-based web taps in Safari */
  String NATIVE_WEB_TAP = "nativeWebTap";

  /** Java package of the Android application that want to run. */
  String APP_PACKAGE = "appPackage";

  /** Activity name for the Android activity that want to launch from test package */
  String APP_ACTIVITY = "appActivity";
}
