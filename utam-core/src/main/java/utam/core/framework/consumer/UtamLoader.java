/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import utam.core.driver.Document;
import utam.core.element.FrameElement;
import utam.core.element.Locator;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.RootPageObject;

/**
 * loader to instantiate and load UTAM POs based on consumer configuration
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public interface UtamLoader {

  /**
   * creates instance of the Root Page Object inside the browser
   *
   * @param type type of the object
   * @param <T> type of Root Page Object to return
   * @return instance of the Page Object, not loaded
   */
  <T extends RootPageObject> T create(Class<T> type);

  /**
   * same as create method, but also checks if PO is actually present, otherwise throws exception
   *
   * @param type type of the object
   * @param <T> type of Root Page Object to return
   * @return instance of the Page Object after we checked that it's loaded
   */
  <T extends RootPageObject> T load(Class<T> type);

  /**
   * creates instance of the UTAM Page Object inside external parent
   *
   * @param parent  external parent
   * @param type    type of the object
   * @param locator UTAM PO root locator
   * @param <T> type of Root Page Object to return
   * @return instance of the Page Object, not loaded
   */
  <T extends PageObject> T create(
      Container parent,
      Class<T> type,
      Locator locator);

  /**
   * enters a frame or iframe element
   * @param frame the frame element to enter
   */
  void enterFrame(FrameElement frame);

  /**
   * enters a frame or iframe element and loads the specified Page Object as loaded in the frame
   * @param frame the frame to enter
   * @param type type of the object
   * @param <T> type of Root Page Object to return
   * @return instance of the Page Object, loaded in the frame
   */
  <T extends RootPageObject> T enterFrameAndLoad(FrameElement frame, Class<T> type);

  /**
   * exits focus from a frame or iframe to the immediate parent frame, or a no-op
   * if already on the top-level frame
   */
  void exitToParentFrame();

  /**
   * exits focus from a frame or iframe to the top-level frame in the document, or a no-op
   * if already on the top-level frame
   */
  void exitFrame();

  /**
   * provides access to configurable parameters
   *
   * @return instance of the config
   */
  UtamLoaderConfig getConfig();

  /**
   * should be called if config parameters were updated in runtime,
   * recreates instance of the page objects context and factory with new settings
   */
  void resetContext();

  /**
   * get instance of the Document Object to call its public methods from test, ex.
   * loader.getDocument().containsObject(MyModal.class);
   *
   * @return new instance every time method is called
   */
  Document getDocument();
}
