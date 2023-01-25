/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import org.openqa.selenium.WebDriver;
import utam.core.driver.Driver;
import utam.core.driver.Navigation;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.appium.MobileDriverAdapter;

import java.net.URL;
import java.util.Set;

/**
 * Navigation implementation
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public class NavigationImpl implements Navigation {

  static final String ERR_NO_WINDOW_WITH_URL = "can't find window with url ";

  private final Driver driverAdapter;

  public NavigationImpl(Driver driverAdapter) {
    this.driverAdapter = driverAdapter;
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
  public void switchToWindow(String url) {
    // save the current url in case we need to revert to it
    String initialHandle = this.driverAdapter.getWindowHandle();

    Set<String> windowHandles = this.driverAdapter.getWindowHandles();
    boolean found = false;
    for(String handle : windowHandles) {
      this.driverAdapter.switchTo().window(handle);
      if(driverAdapter.getUrl().equals(url)) {
        found = true;
        break;
      }
    }

    if(!found) {
      // revert to the original url
      this.driverAdapter.switchTo().window(initialHandle);
      throw new UtamError(ERR_NO_WINDOW_WITH_URL + url);
    }

    //TODO need to return "Window" - ask what that is
  }
}
