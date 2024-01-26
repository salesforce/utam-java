/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.utilities;

import java.util.Map;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import utam.core.selenium.utilities.WebDriverSimulator.WebElementInfo;

/**
 * Describes the interface used to create the driver and element implementations for use with the
 * WebDriverSimulator
 *
 * @author james.evans
 */
public abstract class WebDriverSimulatorObjectFactory {

  /** Creates a new instance of the factory class */
  public WebDriverSimulatorObjectFactory() {}

  /**
   * Creates a valid simulated WebDriver instance
   *
   * @param knownElements a map of elements used in the simulator
   * @return A valid simulated WebDriver instance
   */
  protected abstract WebDriver createDriver(Map<String, WebElementInfo> knownElements);

  /**
   * Creates a valid simulated WebElement instance
   *
   * @param elementName the name of the simulated element
   * @return A valid simulated WebElement instance
   */
  protected abstract WebElement createElement(String elementName);

  /**
   * Sets the value of an attribute of a simulated element
   *
   * @param element the WebElement to set the attribute value for
   * @param attributeName the name of the attribute to set
   * @param attributeValue the value of the attribute to set
   */
  protected abstract void setElementAttribute(
      WebElement element, String attributeName, String attributeValue);

  /**
   * Sets the visible text of a simulated element
   *
   * @param element the WebElement to set the attribute value for
   * @param text the text of the element to set
   */
  protected abstract void setElementText(WebElement element, String text);

  /**
   * Sets the visibility of a simulated element
   *
   * @param element the WebElement to set the visibility for
   * @param isVisible the visibility of the WebElement
   */
  protected abstract void setElementVisibility(WebElement element, boolean isVisible);
}
