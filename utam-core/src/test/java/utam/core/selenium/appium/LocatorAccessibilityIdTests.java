package utam.core.selenium.appium;

import io.appium.java_client.MobileBy;
import org.testng.annotations.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

/**
 * tests for mobile locators
 *
 * @author Qingchun Ren
 * @since 228
 */
public class LocatorAccessibilityIdTests {

  @Test
  public void testCreation() {
    String selector = "selectorString";
    LocatorAccessibilityId locator = new LocatorAccessibilityId(selector);
    assertThat(locator.getValue(), is(equalTo(MobileBy.AccessibilityId(selector))));
    assertThat(locator.getStringValue(), is(equalTo(selector)));
    assertThat(locator.getCopy(selector), is(equalTo(locator)));
    assertThat(locator.setParameters(0,"parameters").getValue(), is(sameInstance(locator)));
  }
}
