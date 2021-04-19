/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import java.time.Duration;
import utam.core.driver.Document;
import utam.core.driver.Driver;
import utam.core.driver.Expectations;
import utam.core.element.FindContext.Type;
import utam.core.element.Locator;
import utam.core.framework.base.PageObjectsFactory;

/**
 * implementation of the document object
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class DocumentObject implements Document {

  static final String DOM_READY_JAVASCRIPT = "document.readyState === 'complete'";

  private static final Expectations<Boolean> isDOMReady =
      new ExpectationsImpl<>("wait for document ready state", driver ->
          (Boolean) driver.executeScript(DOM_READY_JAVASCRIPT)
      );
  private final Driver driver;
  private final Duration timeout;
  private final Duration interval;

  public DocumentObject(PageObjectsFactory factory) {
    this.driver = factory.getDriver();
    this.timeout = factory.getDriverContext().getTimeouts().getWaitForTimeout();
    this.interval = factory.getDriverContext().getTimeouts().getPollingInterval();
  }

  @Override
  public String getUrl() {
    return driver.getUrl();
  }

  @Override
  public void waitForDocumentReady() {
    driver.waitFor(timeout, interval, isDOMReady);
  }

  @Override
  public boolean containsElement(Locator locator) {
    return driver.findElements(locator, Type.NULLABLE).size() > 0;
  }
}
