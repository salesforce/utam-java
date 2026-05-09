/*
 * Copyright (c) 2026, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.wrapped;

import org.openqa.selenium.WebElement;
import utam.core.element.Element;
import utam.core.selenium.element.ElementAdapter;

/**
 * WrappedElementAdapter wraps WebElement
 *
 * @author elizaveta.ivanova
 * @since 264
 */
public class WrappedElementAdapter extends ElementAdapter {

  /** Functional interface for unwrapping the element */
  @FunctionalInterface
  interface SmartUnwrapper {
    /**
     * Unwrap to get the underlying WebElement
     *
     * @return the unwrapped WebElement
     */
    WebElement unwrap();
  }

  /** Smart unwrapper instance for element unwrapping */
  private final SmartUnwrapper smartUnwrapper;

  /**
   * Constructs a WrappedElementAdapter
   *
   * @param element the wrapped element decorator
   * @param driver the wrapped driver adapter
   */
  WrappedElementAdapter(WrappedElementDecorator element, WrappedDriverAdapter driver) {
    super(element, driver);
    this.smartUnwrapper = () -> element.unwrap();
  }

  /**
   * Unwrap the web element to get the underlying WebElement
   *
   * @return the unwrapped web element
   */
  @Override
  protected WebElement unwrap() {
    return this.smartUnwrapper.unwrap();
  }

  @Override
  protected Element wrapElement(WebElement element) {
    if (element instanceof WrappedElementDecorator) {
      return new WrappedElementAdapter(
          (WrappedElementDecorator) element, (WrappedDriverAdapter) this.driverAdapter);
    }
    return super.wrapElement(element);
  }
}
