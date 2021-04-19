package utam.core.selenium.element;

import org.openqa.selenium.By;

/**
 * CSS locator
 *
 * @author elizaveta.ivanova
 * @since 230
 */
class LocatorByCss extends LocatorBy {

  LocatorByCss(String stringValue) {
    super(stringValue);
  }

  @Override
  public By getValue() {
    return By.cssSelector(stringValue);
  }

  @Override
  public LocatorBy getCopy(String valueWithParameters) {
    return new LocatorByCss(valueWithParameters);
  }
}
