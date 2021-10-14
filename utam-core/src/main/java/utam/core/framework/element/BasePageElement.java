/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import static utam.core.selenium.element.ElementAdapter.ERR_DRAG_AND_DROP_NULL_ELEMENT;

import org.openqa.selenium.Keys;
import utam.core.element.BasicElement;
import utam.core.element.DragAndDropOptions;
import utam.core.element.Element;
import utam.core.element.Element.ScrollOptions;
import utam.core.element.RootElement;
import utam.core.framework.UtamCoreError;
import utam.core.framework.UtamLogger;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.UtamBaseImpl;
import utam.core.framework.consumer.UtamError;

/**
 * base element that wraps Element implementation with Driver waits, instantiated on the FOUND
 * element
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class BasePageElement extends UtamBaseImpl implements RootElement {

  private Element element;
  private PageObjectsFactory factory;

  // empty constructor is needed for union types to work
  public BasePageElement() {
  }

  public static <T extends BasicElement, R extends BasePageElement> T createInstance(Class<R> implType, Element element, PageObjectsFactory factory) {
    if(element.isNull()) {
      return null;
    }
    try {
      R instance =  implType.getConstructor().newInstance();
      instance.initialize(factory, element);
      return (T)instance;
    } catch (ReflectiveOperationException e) {
      throw new UtamError(
          String.format("Error creating instance of type '%s'", implType.getSimpleName()),
          e);
    }
  }

  public static BasePageElement createElementInstance(Element element, PageObjectsFactory factory) {
    return createInstance(BasePageElement.class, element, factory);
  }

  // called from method that uses reflection to build an instance
  void initialize(PageObjectsFactory factory, Element element) {
    this.factory = factory;
    this.element = element;
  }

  @Override
  protected final PageObjectsFactory getFactory() {
    return factory;
  }

  @Override
  protected final Element getElement() {
    return element;
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
      if (e.getMessage()
          .contains("javascript error: Cannot read property 'defaultView' of undefined")) {
        UtamLogger.error(
            "Error from WebElement.click(), attempting to execute javascript click instead...");
        UtamLogger.error(e);
        getElement().deprecatedClick();
      } else {
        throw e;
      }
    }
  }

  @Override
  public void javascriptClick() {
    log("deprecated javascript click");
    getElement().deprecatedClick();
  }

  @Override
  public void flick(int xOffset, int yOffset) {
    log(String.format("flick element at X '%d' Y '%d'", xOffset, yOffset));
    String originalContext = getDriver().getContext();
    try {
      getElement().flick(xOffset, yOffset);
    } finally {
      if (!getDriver().isNative()) {
        getDriver().setPageContextToWebView(originalContext, getDriverTimeouts().getWaitForTimeout(), getDriverTimeouts().getPollingInterval());
      }
    }
  }

  @Override
  public void dragAndDrop(BasicElement target, int holdDurationSec) {
    if(target == null) {
      throw new UtamCoreError(ERR_DRAG_AND_DROP_NULL_ELEMENT);
    }
    DragAndDropOptions options = new DragAndDropOptions.ByElement(((BasePageElement)target).getElement(), holdDurationSec);
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
}
