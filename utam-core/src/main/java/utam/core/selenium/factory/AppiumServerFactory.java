/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.factory;

import static utam.core.selenium.factory.SystemProperties.getAppiumPath;
import static utam.core.selenium.factory.SystemProperties.getNodeJSPath;

import io.appium.java_client.service.local.AppiumDriverLocalService;
import io.appium.java_client.service.local.AppiumServiceBuilder;
import io.appium.java_client.service.local.flags.GeneralServerFlag;
import java.io.File;

/**
 * The factory for AppiumDriverLocalService
 *
 * @author qren
 * @since 230
 */
public class AppiumServerFactory {

  /**
   * Gets an instance of the local Appium server.
   *
   * @return An AppiumDriverLocalService instance
   */
  public static AppiumDriverLocalService getAppiumServer() {
    SystemProperties.setNodeJSPath();
    SystemProperties.setAppiumPath();

    AppiumServiceBuilder serviceBuilder = new AppiumServiceBuilder();
    serviceBuilder.usingDriverExecutable(new File(getNodeJSPath()));
    serviceBuilder.withAppiumJS(new File(getAppiumPath()));
    serviceBuilder.withArgument(GeneralServerFlag.SESSION_OVERRIDE);
    serviceBuilder.usingAnyFreePort();
    AppiumDriverLocalService service = AppiumDriverLocalService.buildService(serviceBuilder);
    service.clearOutPutStreams();
    service.enableDefaultSlf4jLoggingOfOutputData();
    return service;
  }
}
