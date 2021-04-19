package utam.core.selenium.appium;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.appium.LocatorUIAutomator.ERR_SELECTOR_UIAUTOMATOR_UNSUPPORTED_METHOD;
import static utam.core.selenium.appium.LocatorUIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX;

import io.appium.java_client.MobileBy;
import org.openqa.selenium.By;
import org.testng.annotations.Test;
import utam.core.element.Locator;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.appium.LocatorUIAutomator.Method;
import utam.core.selenium.element.LocatorBy;

/**
 * Android UIAutomator selector based locator
 *
 * @author Qingchun Ren
 * @since 228
 */
public class LocatorUiAutomatorTests {

  @Test
  public void testCreation() {
    String selector = "description('test')";
    Locator<By> locator = new LocatorUIAutomator(selector);
    By by = MobileBy.AndroidUIAutomator("new UiSelector()." + selector);
    assertThat(locator.getValue(), is(equalTo(by)));
    assertThat(locator.getStringValue(), is(equalTo("new UiSelector()." + selector)));
    assertThat(locator.setParameters(0).getValue().getValue(), is(equalTo(by)));
  }

  @Test
  public void testCreationThrows() {
    String selector = "unsupported";
    expectThrows(UtamError.class, () -> new LocatorUIAutomator(selector));
  }

  @Test
  public void testValidateUIAutomatorSelectorIncludingPrefix() {
    expectThrows(UtamError.class, () -> LocatorBy.byUiAutomator(
        "new UiSelector().\"**/XCUIElementTypeStaticText[`label == 'something'`]\""));
  }

  @Test
  public void testUnsupportedMethod() {
    UtamError e = expectThrows(UtamError.class, () -> LocatorBy.byUiAutomator("unsupported()"));
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_SELECTOR_UIAUTOMATOR_UNSUPPORTED_METHOD, "unsupported"))));
  }

  @Test
  public void testSupportedMethods() {
    for (Method method : Method.values()) {
      Locator locator = new LocatorUIAutomator(method.value + "()");
      assertThat(locator.getStringValue(),
          is(equalTo(UI_AUTOMATOR_SELECTOR_PREFIX + method.value + "()")));
    }
  }
}
