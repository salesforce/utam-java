package utam.core.driver;

import java.time.Duration;
import java.util.List;
import utam.core.element.Element;
import utam.core.element.FindContext;
import utam.core.element.Locator;

/**
 * Driver interface allows to plugin any driver type, default is Selenium
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface Driver {

  Object executeScript(String script, Object... parameters);

  Element findElement(Locator by, FindContext finderContext);

  List<Element> findElements(Locator by, FindContext finderContext);

  <T, R> R waitFor(Duration timeout, Duration pollingInterval, Expectations<T, R> expectations, T parameter);

  <T, R> R waitFor(Expectations<T, R> expectations, T parameter);

  /**
   * set active page context to NATIVE_APP
   */
  void setPageContextToNative();

  /**
   * set active page context to the target WebView page
   *
   * @return if view is different from current, new driver is created
   */
  Driver setPageContextToWebView();

  /**
   * set active page context to the target WebView page
   *
   * @param title title to switch to
   * @return if view is different from current, new driver is created
   */
  Driver setPageContextToWebView(String title);

  /**
   * check if current context is native
   *
   * @return boolean true if current context is native
   */
  boolean isNative();

  /**
   * check if current context is mobile
   *
   * @return boolean true if current context is native
   */
  boolean isMobile();

  String getUrl();
}
