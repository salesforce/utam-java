package utam.core.selenium.appium;

import org.openqa.selenium.WebElement;
import utam.core.driver.Driver;
import utam.core.selenium.element.ElementAdapter;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class MobileElementAdapter extends ElementAdapter {

  public MobileElementAdapter(WebElement element) {
    super(element);
  }

  @Override
  public void flick(Driver driver, int xOffset, int yOffset) {
    MobileDriverUtils.flickElement(driver, getWebElement(), xOffset, yOffset);
  }
}
