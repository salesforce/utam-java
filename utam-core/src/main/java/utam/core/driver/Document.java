/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.driver;

import utam.core.element.Locator;

/**
 * document object to interact with a browser
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface Document {

  /**
   * get current URL
   *
   * @return string with URL
   */
  String getUrl();

  /**
   * wait until DOM ready state is complete
   */
  void waitForDocumentReady();

  /**
   * check if there is an element with the given locator in the DOM
   *
   * @param locator locator to find an element
   * @return true if element found
   */
  boolean containsElement(Locator locator);
}
