/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import org.openqa.selenium.NoSuchWindowException;
import utam.core.driver.Driver;
import utam.core.driver.Navigation;
import utam.core.driver.Window;
import utam.core.framework.UtamLogger;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.consumer.UtamError;

import java.util.Set;

/**
 * Navigation implementation
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public class NavigationImpl implements Navigation {

  static final String ERR_NO_WINDOW_WITH_URL = "can't find window with url %s";
  static final String ERR_NO_INITIAL_WINDOW =
          "window with url %s not found, and the previous window was closed";

  private final Driver driverAdapter;
  private final PageObjectsFactory factory;

  public NavigationImpl(PageObjectsFactory factory) {
    this.factory = factory;
    this.driverAdapter = factory.getDriver();
  }

  @Override
  public void back() {
    this.driverAdapter.back();
  }

  @Override
  public void forward() {
    this.driverAdapter.forward();
  }

  @Override
  public Window switchToWindow(String url) {
    // save the current url in case we need to revert to it
    String initialHandle = null;
    try {
      initialHandle = this.driverAdapter.getWindowHandle();
    } catch (NoSuchWindowException e) {
      UtamLogger.info("No currently focused window");
    }

    Set<String> windowHandles = this.driverAdapter.getWindowHandles();
    boolean isFound = false;
    for (String handle : windowHandles) {
      this.driverAdapter.switchTo(handle);
      if (driverAdapter.getUrl().equals(url)) {
        isFound = true;
        break;
      }
    }

    if (!isFound) {
      if (initialHandle == null) {
        throw new UtamError(String.format(ERR_NO_INITIAL_WINDOW, url));
      }

      // revert to the original url
      this.driverAdapter.switchTo(initialHandle);
      throw new UtamError(String.format(ERR_NO_WINDOW_WITH_URL, url));
    }

    return new WindowImpl(this.factory);
  }

  @Override
  public void closeWindow(String targetUrl) {
    String currentUrl = this.driverAdapter.getUrl();

    if (targetUrl.equals(currentUrl)) {
      this.driverAdapter.close();
    } else {
      this.switchToWindow(targetUrl);
      this.driverAdapter.close();
      this.switchToWindow(currentUrl);
    }

  }

  @Override
  public void closeWindow() {
    this.driverAdapter.close();
  }

  @Override
  public int getWindowCount() {
    return this.driverAdapter.getWindowHandle().length();
  }

  @Override
  public Window currentWindow() {
    return new WindowImpl(this.factory);
  }

  @Override
  public void setupWaitForNewWindow() {

  }

  @Override
  public Window waitForNewWindow() {
    return null;
  }

  @Override
  public PageObject waitForNewWindowAndLoad() {
    return null;
  }

  @Override
  public PageObject switchToWindowAndLoad(String url) {
    return null;
  }
}
