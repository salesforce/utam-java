package utam.core.framework.consumer;

import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;
import utam.core.framework.consumer.LocationPolicyType;
import utam.core.framework.consumer.PageObjectsProvider;
import utam.core.framework.consumer.PageObjectsProviderImpl;

import java.time.Duration;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class PageObjectsProviderImplTests {

    @Test
    public void testLocationPolicyDefaultValue() {
        assertThat(LocationPolicyType.getDefault(), is(equalTo(LocationPolicyType.CHAIN)));
    }

    @Test
    public void testSetLocationPolicyFromProvider() {
        PageObjectsProvider provider = new PageObjectsProviderImpl(mock(WebDriver.class));
        assertThat(provider.getSeleniumContext().getLocationPolicy(), is(equalTo(LocationPolicyType.CHAIN)));
        provider.setLocationPolicy(LocationPolicyType.JAVASCRIPT);
        assertThat(provider.getSeleniumContext().getLocationPolicy(), is(equalTo(LocationPolicyType.JAVASCRIPT)));
    }

    @Test
    public void testSetTimeout() {
        PageObjectsProvider provider = new PageObjectsProviderImpl(mock(WebDriver.class));
        provider.setTimeout(Duration.ofSeconds(1024));
        assertThat(provider.getSeleniumContext().getPollingTimeout(), is(equalTo(Duration.ofSeconds(1024))));
    }
}
