/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.utilities;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import org.openqa.selenium.*;
import utam.core.framework.consumer.UtamError;

/**
 * Creates a simulation of a site that can be used with WebDriver, including a mock driver instance
 * that can be used for unit tests.
 *
 * @author james.evans
 */
public class WebDriverSimulator {

  static final String ELEMENT_ALREADY_EXISTS = "element '%s': already registered";
  static final String ELEMENT_SELECTOR_NOT_SET =
      "element '%s': selector may not be null or the empty string";
  private static final String NO_PARAMETERLESS_CONSTRUCTOR_FOR_FACTORY =
      "factory class '%s' does not have a public parameterless constructor, or is in a nested"
          + " class";
  private static final String ERROR_EXECUTING_CONSTRUCTOR_FOR_FACTORY =
      "unexpected error executing constructor to factory class '%s'";

  private final Map<String, WebElementInfo> knownElements = new HashMap<>();
  private WebDriver driver = null;
  private WebDriverSimulatorObjectFactory objectFactory = null;

  /**
   * Creates a new instance of the simulator
   *
   * @param objectFactoryClass the class of an factory for creating driver and element instances
   */
  public WebDriverSimulator(Class<? extends WebDriverSimulatorObjectFactory> objectFactoryClass) {
    Constructor<? extends WebDriverSimulatorObjectFactory> ctor = null;
    try {
      ctor = objectFactoryClass.getConstructor();
    } catch (NoSuchMethodException | SecurityException e) {
      throw new UtamError(
          String.format(NO_PARAMETERLESS_CONSTRUCTOR_FOR_FACTORY, objectFactoryClass.getName()), e);
    }

    WebDriverSimulatorObjectFactory factory = null;
    try {
      factory = ctor.newInstance();
    } catch (InstantiationException
        | IllegalAccessException
        | IllegalArgumentException
        | InvocationTargetException e) {
      throw new UtamError(
          String.format(ERROR_EXECUTING_CONSTRUCTOR_FOR_FACTORY, objectFactoryClass.getName()), e);
    }

    this.objectFactory = factory;
  }

  /**
   * Registers an element to be used by the simulated driver, using a random GUID as the element
   * name
   *
   * @param selector the CSS selector used to locate the element
   * @return a WebElementInfo object describing the registered element
   */
  public WebElementInfo registerElement(String selector) {
    return registerElement(UUID.randomUUID().toString(), selector);
  }

  /**
   * Registers an element to be used by the simulated driver
   *
   * @param elementName a unique internal name for the element to be used in the simulator
   * @param selector the CSS selector used to locate the element
   * @return a WebElementInfo object describing the registered element
   */
  public WebElementInfo registerElement(String elementName, String selector) {
    WebElementInfo info = new WebElementInfo(elementName, selector);
    knownElements.put(info.getName(), info);
    return info;
  }

  /**
   * Gets the simulated WebDriver instance to be used in tests
   *
   * @return a valid, virtual WebDriver instance
   */
  public WebDriver getDriver() {
    if (driver == null) {
      driver = objectFactory.createDriver(knownElements);
    }
    return driver;
  }

  /**
   * Describes the properties of a WebElement to be used in the simulation of the site
   *
   * @author james.evans
   */
  public class WebElementInfo {
    private final String name;
    private final String selector;
    private String parentElementName;
    private boolean isInShadowDOM;
    private final WebElement element;
    private boolean isFocused;

    /**
     * Creates a new instance of the WebElementInfo class
     *
     * @param elementName a unique internal name for the element to be used in the simulator
     * @param selector the CSS selector used to locate the element
     */
    WebElementInfo(String elementName, String selector) {
      if (elementName == null || elementName.isEmpty()) {
        throw new UnsupportedOperationException("element name may not be null or the empty string");
      }

      if (selector == null || selector.isEmpty()) {
        throw new UnsupportedOperationException(
            String.format(ELEMENT_SELECTOR_NOT_SET, elementName));
      }

      if (knownElements.containsKey(elementName)) {
        throw new UnsupportedOperationException(String.format(ELEMENT_ALREADY_EXISTS, elementName));
      }

      this.name = elementName;
      this.selector = selector;
      this.element = objectFactory.createElement(elementName);
    }

    /**
     * Gets the unique internal name for the element to be used in the simulator
     *
     * @return the unique internal name for the element to be used in the simulator
     */
    public String getName() {
      return name;
    }

    /**
     * Gets the CSS selector used to locate the element
     *
     * @return the CSS selector used to locate the element
     */
    public String getSelector() {
      return selector;
    }

    /**
     * Gets the CSS selector used to locate the element
     *
     * @return the By object containing the CSS selector used to locate the element
     */
    public By getCssSelector() {
      return By.cssSelector(selector);
    }

    /**
     * Gets a value indicating whether the described element is in the ShadowDOM of its parent
     * element
     *
     * @return true if the element is in the Shadow DOM of its parent element; otherwise, false
     */
    public boolean isInShadowDOM() {
      return isInShadowDOM;
    }

    /**
     * Gets the name given to the parent of the element being registered
     *
     * @return the name given to the parent of the element being registered
     */
    public String getParentElementName() {
      return parentElementName;
    }

    /**
     * Gets the mock WebElement instance for the described element
     *
     * @return the mock WebElement instance for the described element
     */
    public WebElement getElement() {
      return element;
    }

    /**
     * Sets a given WebElementInfo object to represent a child of the Shadow DOM of this element
     *
     * @param childInfo the WebElementInfo representing the child element
     * @return this WebElementInfo object
     */
    public WebElementInfo withChildInShadowDOM(WebElementInfo childInfo) {
      setChildElement(childInfo, true);
      return this;
    }

    /**
     * Sets a given WebElementInfo object to represent a child of this element
     *
     * @param childInfo the WebElementInfo representing the child element
     * @return this WebElementInfo object
     */
    public WebElementInfo withChild(WebElementInfo childInfo) {
      setChildElement(childInfo, false);
      return this;
    }

    /**
     * Sets an expected value to be returned for the given attribute name when the Selenium
     * getAttribute method is called
     *
     * @param attributeName the name of the attribute to return the value for
     * @param attributeValue the expected value of the attribute
     * @return this WebElementInfo object
     */
    public WebElementInfo withAttributeValue(String attributeName, String attributeValue) {
      objectFactory.setElementAttribute(element, attributeName, attributeValue);
      return this;
    }

    /**
     * Sets the expected text value to be returned for the element when the Selenium getText method
     * is called
     *
     * @param text string
     * @return this WebElementInfo object
     */
    public WebElementInfo withText(String text) {
      objectFactory.setElementText(element, text);
      return this;
    }

    /**
     * Sets the visibility for the element when the Selenium isDisplayed method is called
     *
     * @param isVisible the boolean visibility value
     * @return this WebElementInfo object
     */
    public WebElementInfo withVisibility(boolean isVisible) {
      objectFactory.setElementVisibility(element, isVisible);
      return this;
    }

    /**
     * Marks the element info to be mocked as having focus when the driver is created.
     *
     * @param isFocused the boolean focus value
     * @return this WebElementInfo object
     */
    public WebElementInfo withFocus(boolean isFocused) {
      this.isFocused = isFocused;
      return this;
    }

    /**
     * Gets a value indicating whether the described element would be focused
     *
     * @return true if the element is going to be set to be focused.
     */
    public boolean isFocused() {
      return isFocused;
    }

    private void setChildElement(WebElementInfo childInfo, boolean isInShadowDOM) {
      childInfo.parentElementName = name;
      childInfo.isInShadowDOM = isInShadowDOM;
      knownElements.put(childInfo.name, childInfo);
    }
  }
}
