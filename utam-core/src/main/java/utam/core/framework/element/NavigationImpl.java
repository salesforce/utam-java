/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import java.util.HashSet;
import java.util.Set;
import org.openqa.selenium.NoSuchWindowException;
import utam.core.driver.Driver;
import utam.core.driver.Navigation;
import utam.core.driver.Window;
import utam.core.framework.UtamLogger;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.RootPageObject;
import utam.core.framework.consumer.UtamError;

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
  static final String ERR_SETUP_NOT_RUN =
      "setupWaitForNewWindow must be called before waitForNewWindow";

  private final Driver driverAdapter;
  private final PageObjectsFactory factory;
  Set<String> trackedWindowHandles;

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
    return this.driverAdapter.getWindowHandles().size();
  }

  @Override
  public Window currentWindow() {
    return new WindowImpl(this.factory);
  }

  @Override
  public void setupWaitForNewWindow() {
    trackedWindowHandles = new HashSet<>(this.driverAdapter.getWindowHandles());
  }

  @Override
  public Window waitForNewWindow() {
    if (this.trackedWindowHandles == null) {
      throw new UtamError(ERR_SETUP_NOT_RUN);
    }

    // Wait for the window count to increase indicating a new window
    this.driverAdapter.waitFor(
        () -> getWindowCount() > trackedWindowHandles.size(),
        "wait for a new window to open",
        null);

    // The difference between the sets of old and new window handles should be the new windows
    // handle
    // Note: if more than one window opens this method will return first that appears in the list of
    // window handles
    Set<String> currentWindowHandles = new HashSet<>(this.driverAdapter.getWindowHandles());
    currentWindowHandles.removeAll(trackedWindowHandles);
    String newWindowsHandle = currentWindowHandles.toArray(new String[1])[0];

    trackedWindowHandles = null;

    this.driverAdapter.switchTo(newWindowsHandle);
    return new WindowImpl(this.factory);
  }

  @Override
  public <T extends RootPageObject> T waitForNewWindowAndLoad(Class<T> type) {
    waitForNewWindow();

    T instance = factory.create(type);
    instance.load();
    return instance;
  }

  @Override
  public <T extends RootPageObject> T switchToNewWindowAndLoad(String url, Class<T> type) {
    switchToWindow(url);

    T instance = factory.create(type);
    instance.load();
    return instance;
  }
}
