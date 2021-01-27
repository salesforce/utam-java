package utam.core.framework.context;

import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.WebDriver;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public enum Driver {
  web,
  ios,
  android,
  chrome,
  firefox;

  public static boolean isMobileDriver(WebDriver driver) {
    return driver instanceof AppiumDriver;
  }
}
