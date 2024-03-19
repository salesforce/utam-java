/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import org.openqa.selenium.Keys;
import utam.core.driver.Driver;
import utam.core.element.Actionable;
import utam.core.element.BasicElement;
import utam.core.element.Clickable;
import utam.core.element.DragAndDropOptions;
import utam.core.element.Draggable;
import utam.core.element.Editable;
import utam.core.element.Element;
import utam.core.element.Element.ElementRectangle;
import utam.core.element.Element.ScrollOptions;
import utam.core.element.Touchable;
import utam.core.framework.UtamLogger;
import utam.core.framework.base.UtamBaseImpl;
import utam.core.framework.consumer.UtamError;

/**
 * base element that wraps Element implementation with Driver waits, instantiated on the FOUND
 * element
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class BasePageElement extends UtamBaseImpl
    implements Actionable, Clickable, Editable, Touchable, Draggable {

  /**
   * Do not delete! Class needs constructor without parameters because we use Java Reflection to
   * create an instance
   *
   * @see BasePageElement createInstance static method
   */
  public BasePageElement() {}

  /**
   * Build instance of the BasePageElement using Java Reflection
   *
   * @param element element to inject
   * @param driver driver instance
   * @return instance of the element
   */
  public static BasePageElement createInstance(Element element, Driver driver) {
    return createInstance(BasePageElement.class, element, driver);
  }

  /**
   * Build instance of the basic type using Java Reflection
   *
   * @param implType type to build
   * @param element element to inject
   * @param driver driver instance
   * @param <T> bound for BasicElement
   * @param <R> bound for BasePageElement
   * @return instance of the element
   */
  public static <T extends BasicElement, R extends BasePageElement> T createInstance(
      Class<R> implType, Element element, Driver driver) {
    if (element == null) {
      return null;
    }
    try {
      R instance = implType.getConstructor().newInstance();
      instance.setDriver(driver);
      instance.setElement(element);
      return (T) instance;
    } catch (ReflectiveOperationException e) {
      throw new UtamError(
          String.format("Error creating instance of type '%s'", implType.getSimpleName()), e);
    }
  }

  @Override
  public boolean isEnabled() {
    return getElement().isEnabled();
  }

  @Override
  public String getAttribute(String attribute) {
    log(String.format("get attribute '%s'", attribute));
    return getElement().getAttribute(attribute);
  }

  @Override
  public String getCssPropertyValue(String propertyName) {
    log(String.format("get CSS property value '%s'", propertyName));
    return getElement().getCssPropertyValue(propertyName);
  }

  @Override
  public String getClassAttribute() {
    return getAttribute("class");
  }

  @Override
  public String getText() {
    log("get element text");
    return getElement().getText();
  }

  @Override
  public void setText(String text) {
    log(String.format("set element text to '%s'", text));
    getElement().setText(text);
  }

  @Override
  public String getTitle() {
    return getAttribute("title");
  }

  @Override
  public String getValue() {
    return getAttribute("value");
  }

  @Override
  public void moveTo() {
    log("move to element");
    getElement().moveTo();
  }

  @Override
  public void scrollToCenter() {
    log("scroll to center");
    getElement().scrollIntoView(ScrollOptions.CENTER);
  }

  @Override
  public void scrollToTop() {
    log("scroll to top");
    getElement().scrollIntoView(ScrollOptions.TOP);
  }

  @Override
  public boolean isFocused() {
    return getElement().hasFocus();
  }

  @Override
  public void focus() {
    log("focus on the element");
    getElement().focus();
  }

  @Override
  public void blur() {
    log("blur the element");
    getElement().blur();
  }

  @Override
  public void clear() {
    log("clear content of the element");
    getElement().clear();
  }

  @Override
  public void clearAndType(String text) {
    getElement().clear();
    getElement().setText(text);
  }

  @Override
  public void press(CharSequence key) {
    Keys keyToPress = Keys.valueOf(key.toString().toUpperCase());
    log(String.format("press keyboard key '%s'", keyToPress.name()));
    getElement().setText(keyToPress.toString());
  }

  @Override
  public void click() {
    log("click on the element");
    try {
      getElement().click();
    } catch (Exception e) {
      UtamLogger.error(
          "Error from WebElement.click(), attempting to execute javascript click instead...");
      UtamLogger.error(e);
      getElement().deprecatedClick();
    }
  }

  @Override
  public void doubleClick() {
    log("double-click on the element");
    getElement().doubleClick();
  }

  @Override
  public void clickAndHold(int holdDurationSec) {
    log(String.format("click and hold the element for %d seconds", holdDurationSec));
    getElement().clickAndHold(holdDurationSec);
  }

  @Override
  public void rightClick() {
    log("perform secondary button click ('right-click' on element");
    getElement().rightClick();
  }

  @Override
  public void flick(int xOffset, int yOffset) {
    log(String.format("flick element at X '%d' Y '%d'", xOffset, yOffset));
    String originalContext = getDriver().getPageContext();
    try {
      getElement().flick(xOffset, yOffset);
    } finally {
      if (!getDriver().isNativeContext()) {
        getDriver().setPageContextToWebView(originalContext);
      }
    }
  }

  @Override
  public void dragAndDrop(BasicElement target, int holdDurationSec) {
    DragAndDropOptions options =
        new DragAndDropOptions.ByElement(((BasePageElement) target).getElement(), holdDurationSec);
    getElement().dragAndDrop(options);
  }

  @Override
  public void dragAndDrop(BasicElement target) {
    dragAndDrop(target, 0);
  }

  @Override
  public void dragAndDropByOffset(int xOffset, int yOffset, int holdDurationSec) {
    DragAndDropOptions options = new DragAndDropOptions.ByOffset(xOffset, yOffset, holdDurationSec);
    getElement().dragAndDrop(options);
  }

  @Override
  public void dragAndDropByOffset(int xOffset, int yOffset) {
    dragAndDropByOffset(xOffset, yOffset, 0);
  }

  @Override
  public ElementRectangle getRect() {
    return getElement().getRect();
  }
}
