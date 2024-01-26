/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.driver;

import utam.core.framework.base.RootPageObject;

/**
 * Navigation interface exposing commands to navigate browsing web and mobile contexts
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public interface Navigation {

  /** Command to navigate one step backward in the browsing context */
  void back();

  /** Command to navigate one step forward in the browsing context */
  void forward();

  /**
   * Command to navigate to an open window with the given url
   *
   * @param url the url of the window to navigate to
   */
  Window switchToWindow(String url);

  /**
   * Closes the window navigated to the specified URL
   *
   * @param url the url of the window to be closed
   */
  void closeWindow(String url);

  /** Closes the current window */
  void closeWindow();

  /**
   * Gets the number of currently opened managed windows
   *
   * @return the number of currently opened managed windows
   */
  int getWindowCount();

  /**
   * Gets the window with the current command context
   *
   * @return the window with the current command context
   */
  Window currentWindow();

  /** Sets up a wait for a new window to appear after taking a subsequent action */
  void setupWaitForNewWindow();

  /**
   * Waits for a new window to be opened and switches the command context to the new window; if
   * called without a prior call to setupWaitForNewWindow, an exception is thrown
   *
   * @return the new window
   */
  Window waitForNewWindow();

  /**
   * Waits for a new window to be opened, switches to it, and loads a Page Object that is a new root
   *
   * @param type the type of page object to load
   * @return a Page Object that is a new root
   */
  <T extends RootPageObject> T waitForNewWindowAndLoad(Class<T> type);

  /**
   * Switches to the window navigated to the specified URL and loads a Page Object that is a new
   * root
   *
   * @param url the url of the window to navigate to
   * @param type the type of page object to load
   * @return a Page Object that is a new root
   */
  <T extends RootPageObject> T switchToNewWindowAndLoad(String url, Class<T> type);
}
