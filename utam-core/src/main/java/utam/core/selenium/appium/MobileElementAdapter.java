package utam.core.selenium.appium;

import java.time.Duration;
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
  public void flick(Driver driver, Duration timeout, Duration pollingInterval, int xOffset,
      int yOffset) {
    MobileDriverUtils.flickElement(driver, timeout, pollingInterval, getWebElement(), xOffset, yOffset);
  }
}
