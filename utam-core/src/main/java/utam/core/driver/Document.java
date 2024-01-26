/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.driver;

import java.util.function.Supplier;
import utam.core.element.FrameElement;
import utam.core.element.Locator;
import utam.core.framework.base.RootPageObject;

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

  /** wait until DOM ready state is complete */
  void waitForDocumentReady();

  /**
   * check if there is an element with the given locator in the DOM
   *
   * @param locator locator to find an element
   * @return true if element found
   */
  boolean containsElement(Locator locator);

  /**
   * check if there is a root page object present in the DOM
   *
   * @param pageObjectType type of root Page Object to search for
   * @return true if Page Object's root element is found
   */
  boolean containsObject(Class<? extends RootPageObject> pageObjectType);

  /**
   * enters a frame or iframe element
   *
   * @param frame the frame element to enter
   */
  void enterFrame(FrameElement frame);

  /**
   * exits focus from a frame or iframe to the immediate parent frame, or a no-op if already on the
   * top-level frame
   */
  void exitToParentFrame();

  /**
   * exits focus from a frame or iframe to the top-level frame in the document, or a no-op if
   * already on the top-level frame
   */
  void exitFrame();

  /**
   * enters a frame or iframe element and loads the specified Page Object as loaded in the frame
   *
   * @param frame the frame to enter
   * @param type type of the object
   * @param <T> type of Root Page Object to return
   * @return instance of the Page Object, loaded in the frame
   */
  <T extends RootPageObject> T enterFrameAndLoad(FrameElement frame, Class<T> type);

  /**
   * polling wait that repeatedly applies expectations until truthy value is return (not null or
   * boolean true)
   *
   * @param condition condition to wait for
   * @param <T> return type
   * @return result of the applied expectations
   */
  <T> T waitFor(Supplier<T> condition);
}
