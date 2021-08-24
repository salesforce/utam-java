/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import static utam.core.element.FindContext.Type.EXISTING;
import static utam.core.element.FindContext.Type.NULLABLE;
import static utam.core.framework.base.FrameElementImpl.getUnwrappedElement;

import java.time.Duration;
import java.util.function.Supplier;
import utam.core.driver.Document;
import utam.core.driver.Driver;
import utam.core.driver.Expectations;
import utam.core.element.ElementLocation;
import utam.core.element.Locator;
import utam.core.framework.UtamCoreError;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.RootPageObject;
import utam.core.element.FrameElement;

/**
 * implementation of the document object
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class DocumentObject implements Document {

  static final String DOM_READY_JAVASCRIPT = "document.readyState === 'complete'";
  static final String ERR_CANT_ENTER_NULL_FRAME = "Can't enter null frame element";

  private final Driver driver;
  private final Duration timeout;
  private final Duration interval;
  private final PageObjectsFactory factory;

  public DocumentObject(PageObjectsFactory factory) {
    this.driver = factory.getDriver();
    this.timeout = factory.getDriverContext().getTimeouts().getWaitForTimeout();
    this.interval = factory.getDriverContext().getTimeouts().getPollingInterval();
    this.factory = factory;
  }

  @Override
  public String getUrl() {
    return driver.getUrl();
  }

  @Override
  public void waitForDocumentReady() {
    waitFor(() -> (Boolean) driver.executeScript(DOM_READY_JAVASCRIPT));
  }

  @Override
  public boolean containsElement(Locator locator) {
    return driver.findElements(locator, NULLABLE).size() > 0;
  }

  @Override
  public boolean containsObject(Class<? extends RootPageObject> pageObjectType) {
    RootPageObject instance = factory.getPageContext().getBean(pageObjectType);
    ElementLocation finder = instance.setRootLocator(NULLABLE);
    return !finder.findElements(driver).isEmpty();
  }

  @Override
  public void enterFrame(FrameElement frame) {
    if(frame == null) {
      throw new UtamCoreError(ERR_CANT_ENTER_NULL_FRAME);
    }
    driver.enterFrame(getUnwrappedElement(frame));
  }

  @Override
  public void exitToParentFrame() {
    driver.exitToParentFrame();
  }

  @Override
  public void exitFrame() {
    driver.exitFrame();
  }

  @Override
  public <T extends RootPageObject> T enterFrameAndLoad(FrameElement frame, Class<T> type) {
    enterFrame(frame);
    T instance = factory.getPageContext().getBean(type);
    ElementLocation finder = instance.setRootLocator(EXISTING);
    factory.bootstrap(instance, finder);
    instance.load();
    return instance;
  }

  @Override
  public final <T> T waitFor(Supplier<T> condition) {
    Expectations<T> expectations =
        new ExpectationsImpl<>("wait for condition", (driver) -> condition.get());
    return driver.waitFor(timeout, interval, expectations);
  }
}
