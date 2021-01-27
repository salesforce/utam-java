package utam.core.framework.context;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.*;
import static org.testng.Assert.expectThrows;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;

import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.DesiredCapabilities;
import org.testng.annotations.Test;
import utam.core.framework.context.PlatformType;

/**
 * Provides tests for the PlatformType enumerated type
 * @author james.evans
 *
 */
public class PlatformTypeTests {
  
  /**
   * The fromString method should return valid values for known types
   */
  @Test
  public void testFromString() {
    assertThat(PlatformType.fromString("native"), is(equalTo(PlatformType.NATIVE)));
    assertThat(PlatformType.fromString("web"), is(equalTo(PlatformType.WEB)));
    assertThat(PlatformType.fromString(null), is(equalTo(PlatformType.NONE)));
    assertThat(PlatformType.fromString(""), is(equalTo(PlatformType.NONE)));
  }

  /**
   * The fromString method should throw the appropriate exception
   * for unknown types
   */
  @Test
  public void testFromStringWithUnknownTypeThrows() {
    IllegalArgumentException e = expectThrows(
        IllegalArgumentException.class,
        () -> PlatformType.fromString("illegal"));
    assertThat(e.getMessage(), containsString("Unknown platform type 'illegal'"));
  }

  /**
   * The getName method should return valid values for known types
   */
  @Test
  public void testGetName() {
    assertThat(PlatformType.NATIVE.getName(), is(equalTo("native")));
    assertThat(PlatformType.WEB.getName(), is(equalTo("web")));
    assertThat(PlatformType.NONE.getName(), is(equalTo("")));
  }

  @Test
  public void testGetActivePlatformProfile() {
    assertThat(PlatformType.getActivePlatformProfile(mock(WebDriver.class)), is(PlatformType.PLATFORM_WEB));
    assertThat(PlatformType.getActivePlatformProfile(mock(AndroidDriver.class)), is(PlatformType.PLATFORM_ANDROID));
    assertThat(PlatformType.getActivePlatformProfile(mock(IOSDriver.class)), is(PlatformType.PLATFORM_IOS));
    AppiumDriver driver = mock(AppiumDriver.class);
    DesiredCapabilities capabilities = new DesiredCapabilities();
    capabilities.setPlatform(Platform.MAC);
    when(driver.getCapabilities()).thenReturn(capabilities);
    assertThat(PlatformType.getActivePlatformProfile(driver), is(PlatformType.PLATFORM_IOS));
    capabilities.setPlatform(Platform.LINUX);
    when(driver.getCapabilities()).thenReturn(capabilities);
    assertThat(PlatformType.getActivePlatformProfile(driver), is(PlatformType.PLATFORM_ANDROID));
    capabilities.setPlatform(Platform.WINDOWS);
    when(driver.getCapabilities()).thenReturn(capabilities);
    assertThat(PlatformType.getActivePlatformProfile(driver), is(PlatformType.PLATFORM_WEB));
  }
}
