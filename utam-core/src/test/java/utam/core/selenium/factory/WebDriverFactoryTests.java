package utam.core.selenium.factory;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.WebDriver;
import utam.core.driver.DriverContext;
import utam.core.driver.DriverType;
import org.testng.annotations.Test;
import utam.core.selenium.appium.MobileDriverAdapter;
import utam.core.selenium.element.DriverAdapter;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.factory.WebDriverFactory.ERR_UNKNOWN_DRIVER_TYPE;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class WebDriverFactoryTests {

    private static final DriverContext driverContextMock = mock(DriverContext.class);

    @Test
    void testChromeOptions() {
        WebDriverFactory.defaultChromeOptions(false);
        WebDriverFactory.defaultChromeOptions(true);
        assertThrows(() -> WebDriverFactory.getWebDriver(mock(DriverType.class)));
    }

    @Test
    void testGetDriverError() {
        IllegalArgumentException e = expectThrows(IllegalArgumentException.class, () -> WebDriverFactory.getWebDriver(null));
        assertThat(e.getMessage(), is(equalTo(String.format(ERR_UNKNOWN_DRIVER_TYPE, "null"))));
    }

    @Test
    public void testIsMobileDriver() {
        IOSDriver iosDriver = mock(IOSDriver.class);
        assertThat(WebDriverFactory.getAdapter(iosDriver, driverContextMock), instanceOf(
            MobileDriverAdapter.class));

        AndroidDriver androidDriver = mock(AndroidDriver.class);
        assertThat(WebDriverFactory.getAdapter(androidDriver, driverContextMock), instanceOf(
            MobileDriverAdapter.class));

        AppiumDriver appiumDriver = mock(AppiumDriver.class);
        assertThat(WebDriverFactory.getAdapter(appiumDriver, driverContextMock), instanceOf(
            MobileDriverAdapter.class));

        WebDriver driver = mock(WebDriver.class);
        assertThat(WebDriverFactory.getAdapter(driver, driverContextMock), instanceOf(
            DriverAdapter.class));
    }
}
