/*
 * Copyright (c) 2026, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.wrapped;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import utam.core.driver.DriverConfig;
import utam.core.element.Element;
import utam.core.selenium.element.DriverAdapter;

/**
 * WrappedDriverAdapter wraps WebDriver
 *
 * @author elizaveta.ivanova
 * @since 264
 */
public class WrappedDriverAdapter extends DriverAdapter {

  /** Functional interface for unwrapping the driver */
  @FunctionalInterface
  interface SmartUnwrapper {
    /**
     * Unwrap to get the underlying WebDriver
     *
     * @return the unwrapped WebDriver
     */
    WebDriver unwrap();
  }

  /** Smart unwrapper instance for driver unwrapping */
  private final SmartUnwrapper smartUnwrapper;

  /**
   * Constructs a WrappedDriverAdapter
   *
   * @param wrappedDriver the wrapped driver decorator
   * @param driverConfig driver configuration
   */
  public WrappedDriverAdapter(WrappedDriverDecorator wrappedDriver, DriverConfig driverConfig) {
    super(wrappedDriver, driverConfig);
    this.smartUnwrapper = () -> wrappedDriver.unwrap();
  }

  /**
   * Unwrap to get the underlying WebDriver instance
   *
   * @return the unwrapped WebDriver
   */
  @Override
  protected WebDriver unwrap() {
    return this.smartUnwrapper.unwrap();
  }

  /**
   * Wrap a WebElement into an Element. If the element is a WrappedElementDecorator, creates a
   * WrappedElementAdapter with smart finding capabilities.
   *
   * @param element the web element to wrap
   * @return the wrapped Element
   */
  @Override
  protected Element wrapElement(WebElement element) {
    if (element instanceof WrappedElementDecorator) {
      return new WrappedElementAdapter((WrappedElementDecorator) element, this);
    }
    return super.wrapElement(element);
  }
}
