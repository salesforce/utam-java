package framework.context;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;

import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;

public enum PlatformType {
  NONE(""),
  WEB("web"),
  NATIVE("native");
  
  private final String name;
  
  PlatformType(String name) {
      this.name = name;
  }

  public static PlatformType fromString(String string) {
    if (string == null || string.isEmpty()) {
      return NONE;
    }
    for (PlatformType type : PlatformType.values()) {
      if (type.name().toLowerCase().equals(string)) {
        return type;
      }
    }
    throw new IllegalArgumentException(String.format("Unknown platform type '%s'", string));
  }

  public String getAnnotation() {
    return String.format("%s.%s", getClass().getSimpleName(), name());
  }

  public String getName() {
      return name;
  }

  public static final String PLATFORM_PROFILE_KEY = "platform";
  public static final Profile PLATFORM_WEB = new StringValueProfile(PLATFORM_PROFILE_KEY, "web");
  public static final Profile PLATFORM_IOS = new StringValueProfile(PLATFORM_PROFILE_KEY, "ios");
  public static final Profile PLATFORM_ANDROID = new StringValueProfile(PLATFORM_PROFILE_KEY, "android");

  /**
   * detect platform profile based on driver type
   * @param driver driver passed to Page Objects Provider
   * @return profile
   */
  public static Profile getActivePlatformProfile(WebDriver driver) {
    if(driver instanceof AndroidDriver) {
      return PlatformType.PLATFORM_ANDROID;
    }
    if(driver instanceof IOSDriver) {
      return PlatformType.PLATFORM_IOS;
    }
    if (driver instanceof AppiumDriver) {
      Platform platform = ((AppiumDriver) driver).getCapabilities().getPlatform();
      if (platform == Platform.LINUX) {
        return PlatformType.PLATFORM_ANDROID;
      }
      if (platform == Platform.MAC) {
        return PlatformType.PLATFORM_IOS;
      }
    }
    return PlatformType.PLATFORM_WEB;
  }
}
