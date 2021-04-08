/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.factory;

import io.appium.java_client.service.local.AppiumDriverLocalService;
import io.appium.java_client.service.local.AppiumServiceBuilder;

import java.io.File;

/**
 * The factory for AppiumDriverLocalService
 *
 * @author qren
 * @since 230
 */
@SuppressWarnings({"WeakerAccess", "unused"})
public class AppiumServerFactory {

  public static AppiumDriverLocalService getAppiumServer() {
    AppiumServiceBuilder serviceBuilder = new AppiumServiceBuilder();
    serviceBuilder.usingAnyFreePort();
    serviceBuilder.usingDriverExecutable(new File(SystemProperties.getNodeJSPath()));
    serviceBuilder.withAppiumJS(new File(SystemProperties.getAppiumPath()));
    return serviceBuilder.build();
  }

}
