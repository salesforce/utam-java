package utam.core.selenium.appium;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.core.selenium.appium.MobileDriverUtils.getActivePlatformProfile;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.DesiredCapabilities;
import org.testng.annotations.Test;
import utam.core.framework.context.PlatformType;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class MobileDriverUtilsTests {

  @Test
  public void testGetActivePlatformProfile() {
    assertThat(getActivePlatformProfile(mock(WebDriver.class)), is(PlatformType.PLATFORM_WEB));
    assertThat(getActivePlatformProfile(mock(AndroidDriver.class)),
        is(PlatformType.PLATFORM_ANDROID));
    assertThat(getActivePlatformProfile(mock(IOSDriver.class)), is(PlatformType.PLATFORM_IOS));
    AppiumDriver driver = mock(AppiumDriver.class);
    DesiredCapabilities capabilities = new DesiredCapabilities();
    capabilities.setPlatform(Platform.MAC);
    when(driver.getCapabilities()).thenReturn(capabilities);
    assertThat(getActivePlatformProfile(driver), is(PlatformType.PLATFORM_IOS));
    capabilities.setPlatform(Platform.LINUX);
    when(driver.getCapabilities()).thenReturn(capabilities);
    assertThat(getActivePlatformProfile(driver), is(PlatformType.PLATFORM_ANDROID));
    capabilities.setPlatform(Platform.WINDOWS);
    when(driver.getCapabilities()).thenReturn(capabilities);
    assertThat(getActivePlatformProfile(driver), is(PlatformType.PLATFORM_WEB));
  }
}
