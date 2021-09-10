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
import utam.core.driver.Expectations;
import utam.core.element.BasicElement;
import utam.core.element.DragAndDropOptions;
import utam.core.element.Element;
import utam.core.element.Element.ScrollOptions;
import utam.core.element.RootElement;
import utam.core.framework.UtamCoreError;
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

  private <T> T apply(Expectations<T> expectations) {
    log(expectations.getLogMessage());
    return getDriver()
        .waitFor(getDriverTimeouts().getFluentWaitTimeout(), getDriverTimeouts().getPollingInterval(), expectations,
            getElement());
  }

  @Override
  public boolean isEnabled() {
    return getElement().isEnabled();
  }

  @Override
  public String getAttribute(String attribute) {
    Expectations<String> expectations = ElementExpectations.getAttribute(attribute);
    return apply(expectations);
  }

  @Override
  public String getClassAttribute() {
    return getAttribute("class");
  }

  @Override
  public String getText() {
    Expectations<String> expectations = ElementExpectations.getText();
    return apply(expectations);
  }

  @Override
  public void setText(String text) {
    Expectations<Boolean> expectations = ElementExpectations.setText(text);
    apply(expectations);
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
    Expectations<Boolean> expectations = ElementExpectations.moveTo();
    apply(expectations);
  }

  @Override
  public void scrollToCenter() {
    Expectations<Boolean> expectations = ElementExpectations.scrollTo(ScrollOptions.CENTER);
    apply(expectations);
  }

  @Override
  public void scrollToTop() {
    Expectations<Boolean> expectations = ElementExpectations.scrollTo(ScrollOptions.TOP);
    apply(expectations);
  }

  @Override
  public boolean isFocused() {
    return getElement().hasFocus();
  }

  @Override
  public void focus() {
    Expectations<Boolean> expectations = ElementExpectations.focus();
    apply(expectations);
  }

  @Override
  public void blur() {
    Expectations<Boolean> expectations = ElementExpectations.blur();
    apply(expectations);
  }

  @Override
  public void clear() {
    Expectations<Boolean> expectations = ElementExpectations.clear();
    apply(expectations);
  }

  @Override
  public void clearAndType(String text) {
    Expectations<Boolean> expectations = ElementExpectations.clearAndType(text);
    apply(expectations);
  }

  @Override
  public void press(CharSequence key) {
    Keys keyToPress = Keys.valueOf(key.toString().toUpperCase());
    log(String.format("press keyboard key '%s'", keyToPress.name()));
    Expectations<Boolean> expectation = ElementExpectations.setText(keyToPress.toString());
    apply(expectation);
  }

  @Override
  public void click() {
    Expectations<Boolean> expectations = ElementExpectations.click();
    apply(expectations);
  }

  @Override
  public void javascriptClick() {
    Expectations<Boolean> expectations = ElementExpectations.javascriptClick();
    apply(expectations);
  }

  @Override
  public void flick(int xOffset, int yOffset) {
    Expectations<Boolean> expectations = ElementExpectations.flick(xOffset, yOffset);
    String originalContext = getDriver().getContext();
    try {
      apply(expectations);
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
