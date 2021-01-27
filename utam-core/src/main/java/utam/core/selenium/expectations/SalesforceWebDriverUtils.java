/*
 * Copyright 2020 salesforce.com, inc.
 * All Rights Reserved
 * Company Confidential
 */

package utam.core.selenium.expectations;

import org.openqa.selenium.*;
import utam.core.selenium.context.WebDriverUtilities;

/**
 * this is adapted from core <br\>
 * core/test-shared/test/func/java/src/test/util/webdriver/BaseWebDriverUtil.java
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public abstract class SalesforceWebDriverUtils {

  static final String SCROLL_INTO_VIEW_MSG = "scroll element into view";
  static final String SCROLL_INTO_VIEW_ERR =
      "Element is still not visible or clickable after scroll into view";
  private static final String SCROLL_TO_DOCUMENT_ORIGIN_JS = 
      "window.scrollTo(0,0);";
  static final String SCROLL_INTO_VIEW_JS = 
      "if (document.documentElement"
      + " && document.documentElement.style"
      + " && 'scrollBehavior' in document.documentElement.style) {"
      + "arguments[0].scrollIntoView({behavior: 'instant', block: 'end', inline: 'nearest'});"
      + "} else {"
      + "arguments[0].scrollIntoView(false);"
      + "}";
  public static final String SCROLL_INTO_VIEW_ALIGN_TO_TOP_JS = 
      "return arguments[0].scrollIntoView(true);";

  private static void scrollWithCompliance(WebDriverUtilities utilities, WebElement element) {
    // History lesson: The original WebDriver JSON Wire Protocol, now known
    // as the OSS dialect of the protocol, had a command for getting the
    // location of an element after scrolling it into view. This was exposed
    // in Selenium by using ((Locatable)element).getCoordinates().inViewPort().
    // Drivers compliant with the W3C WebDriver Specification do not support
    // that command. In modern browsers and modern versions of Selenium,
    // all driver instances are compliant with the specification, and no
    // longer need special cases. For scrolling into view, the Selenium Java
    // language bindings require using JavaScript. Note carefully that we
    // should only attempt to scroll if either the element is not currently
    // in the view port (which should be handled by isDisplayed). The below
    // JavaScript code is designed to work across all browsers, including
    // Internet Explorer, and works around a bug in Firefox 57 and higher
    // regarding scrolling elements into view when frames are present on the
    // page.
    utilities.executeJavaScript(SCROLL_INTO_VIEW_JS, element);
  }

  /**
   * scroll into view from Salesforce utilities
   * @return expectations
   */
  static ElementExpectations<SearchContext> scrollIntoView() {
    return new AbstractElementExpectation.ConsumesUtilsElement(
        SCROLL_INTO_VIEW_MSG,
        (utilities, element) -> {
          if (element.isDisplayed()) {
            return;
          }
          scrollWithCompliance(utilities, element);
          if (element.isDisplayed()) {
            return;
          }
          scrollIntoViewJavascript(utilities, element);
          if (!element.isDisplayed()) {
            throw new ElementNotVisibleException(SCROLL_INTO_VIEW_ERR);
          }
        });
  }

  static void scrollIntoViewJavascript(WebDriverUtilities utilities, WebElement element) {
    utilities.executeJavaScript(SCROLL_INTO_VIEW_ALIGN_TO_TOP_JS, element);
    if (!element.isDisplayed()) {
      utilities.executeJavaScript(SCROLL_TO_DOCUMENT_ORIGIN_JS);
      utilities.executeJavaScript(SCROLL_INTO_VIEW_ALIGN_TO_TOP_JS, element);
    }
  }
}
