/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import java.time.Duration;
import java.util.function.Supplier;
import utam.core.driver.Document;
import utam.core.driver.Navigation;
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
   * @param externalScopeProvider external parent
   * @param utamPageObjectType type of the object
   * @param utamPageObjectRoot UTAM PO root locator
   * @param <T> type of Root Page Object to return
   * @return instance of the Page Object, not loaded
   * @deprecated compatibility mode not supported
   */
  @Deprecated
  <T extends PageObject> T create(
      Container externalScopeProvider, Class<T> utamPageObjectType, Locator utamPageObjectRoot);

  /**
   * enters a frame or iframe element
   *
   * @param frame the frame element to enter
   */
  void enterFrame(FrameElement frame);

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
   * provides access to configurable parameters
   *
   * @return instance of the config
   */
  UtamLoaderConfig getConfig();

  /**
   * should be called if config parameters were updated in runtime, recreates instance of the page
   * objects context and factory with new settings
   */
  void resetContext();

  /**
   * Get instance of the Document Object to call its public methods from test, ex.
   * loader.getDocument().containsObject(MyModal.class);
   *
   * @return document instance
   */
  Document getDocument();

  /**
   * Get instance of the Navigation Object to call its public methods from test, ex.
   * loader.getNavigation().back();
   *
   * @return navigation instance
   */
  Navigation getNavigation();

  /**
   * Utility method that does same as Driver.waitFor. Polling wait repeatedly applies expectations
   * until truthy value is return (not null or boolean true).
   *
   * @param timeout timeout after which exception is thrown if condition is not met. If passed as
   *     null, timeout from config is used
   * @param isTrue condition to apply
   * @param message error message to throw if timeout is reached, can be null
   * @param <T> return type
   * @return result of the applied expectations
   */
  <T> T waitFor(Supplier<T> isTrue, String message, Duration timeout);

  /**
   * Utility method that does same as Driver.waitFor and uses default explicit wait timeout
   *
   * @param isTrue condition to apply
   * @param message error message to throw if timeout is reached, can be null
   * @param <T> return type
   * @return result of the applied expectations
   */
  <T> T waitFor(Supplier<T> isTrue, String message);
}
