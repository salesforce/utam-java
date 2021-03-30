package utam.core.selenium.factory;

import static utam.core.selenium.factory.SystemProperties.getAppiumPath;
import static utam.core.selenium.factory.SystemProperties.getNodeJSPath;

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
    serviceBuilder.usingDriverExecutable(new File(getNodeJSPath()));
    serviceBuilder.withAppiumJS(new File(getAppiumPath()));
    return serviceBuilder.build();
  }

}
