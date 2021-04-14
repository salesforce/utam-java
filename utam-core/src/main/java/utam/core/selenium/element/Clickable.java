/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

/**
 * interaction methods for clickable element
 * @author elizaveta.ivanova
 * @since 228
 */
public interface Clickable extends Actionable {

  /**
   * clicks on the element using Selenium WebElement.click.
   * Method is wrapped in fluent wait to find the element. <br>
   * Throws exception if nothing found within timeout. <br>
   */
  void click();

  /**
   * executes javascript "arguments[0].click();" to trick Selenium into clicking on the element that
   * is not considered clickable by the Web Driver due to bug in driver or in LWC.
   * @deprecated starting from chromedriver 87 and utam version 232.0.10, use click(): <br>
   * because driver now throws exception, we are falling back in click() into same
   */
  @Deprecated
  void javascriptClick();
}
