package selenium.factory;

import framework.context.Driver;
import org.testng.annotations.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static selenium.factory.WebDriverFactory.ERR_UNKNOWN_DRIVER_TYPE;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class WebDriverFactoryTests {

    @Test
    void testChromeOptions() {
        WebDriverFactory.defaultChromeOptions(false);
        WebDriverFactory.defaultChromeOptions(true);
        assertThrows(() -> WebDriverFactory.getWebDriver(mock(Driver.class)));
    }

    @Test
    void testGetDriverError() {
        IllegalArgumentException e = expectThrows(IllegalArgumentException.class, () -> WebDriverFactory.getWebDriver(null));
        assertThat(e.getMessage(), is(equalTo(String.format(ERR_UNKNOWN_DRIVER_TYPE, "null"))));
    }
}
