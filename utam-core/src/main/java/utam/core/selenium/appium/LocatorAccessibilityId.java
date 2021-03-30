package utam.core.selenium.appium;

import io.appium.java_client.MobileBy;
import org.openqa.selenium.By;
import utam.core.selenium.element.LocatorBy;

/**
 * @since 230
 */
public class LocatorAccessibilityId extends LocatorBy {

  public LocatorAccessibilityId(String selectorString) {
    super(selectorString);
  }

  @Override
  public By getValue() {
    return MobileBy.AccessibilityId(stringValue);
  }

  @Override
  public LocatorBy getCopy(String valueWithParameters) {
    return new LocatorAccessibilityId(valueWithParameters);
  }
}
