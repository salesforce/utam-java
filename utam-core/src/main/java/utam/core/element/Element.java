package utam.core.element;

import java.time.Duration;
import java.util.List;
import utam.core.driver.Driver;

/**
 * wrapper around element instance from the integrated framework, ex. WebElement from Selenium
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface Element {

  boolean isNull();

  Element findElement(Locator by, FindContext finderContext);

  List<Element> findElements(Locator by, FindContext finderContext);

  int containsElements(Locator by, boolean isExpandShadowRoot);

  boolean isDisplayed();

  boolean isEnabled();

  boolean isExisting();

  void clear();

  void click();

  /**
   * provided as temporary workaround when regular click does not work
   *
   * @deprecated when all browsers work as expected, will be removed
   */
  @Deprecated
  void deprecatedClick(Driver driver);

  String getAttribute(String attrName);

  String getText();

  void setText(String text);

  void scrollIntoView(Driver driver, ScrollOptions options);

  void moveTo(Driver driver);

  boolean hasFocus(Driver driver);

  void blur(Driver driver);

  void focus(Driver driver);

  void flick(Driver driver, Duration timeout, Duration pollingInterval, int xOffset, int yOffset);

  boolean flickItems(GestureDirection direction);

  enum ScrollOptions {
    TOP,
    CENTER
  }

  /**
   * Enumeration of gesture directions.
   */
  enum GestureDirection {
    DOWN,
    UP,
    LEFT,
    RIGHT
  }
}
