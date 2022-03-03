/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import static utam.core.selenium.element.DriverAdapter.getNotFoundErr;

import java.util.List;
import java.util.stream.Collectors;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebElement;
import utam.core.element.Element;
import utam.core.element.Locator;

/**
 * wraps regular adapter to search inside its shadow root
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public class ShadowRootElementAdapter extends ElementAdapter {

  /**
   * Initializes a new instance of the ShadowRootElementAdapter class
   *
   * @param elementAdapter the element adaptor to use
   */
  public ShadowRootElementAdapter(Element elementAdapter) {
    super(elementAdapter);
  }

  @Override
  public Element findElement(Locator locator) {
    By by = ((LocatorBy) locator).getValue();
    WebElement res = driverAdapter
        .waitFor(() -> getWebElement().findElement(by), getNotFoundErr(locator),
            driverAdapter.getDriverConfig().getImplicitTimeout());
    return wrapElement(res);
  }

  @Override
  public List<Element> findElements(Locator locator) {
    By by = ((LocatorBy) locator).getValue();
    List<WebElement> res = driverAdapter
        .waitFor(() -> {
              List<WebElement> found = getWebElement().findElements(by);
              if (found == null || found.isEmpty()) {
                throw new NoSuchElementException(getNotFoundErr(locator));
              }
              return found;
            }, getNotFoundErr(locator),
            driverAdapter.getDriverConfig().getImplicitTimeout());
    return res.stream().map(el -> wrapElement(el)).collect(Collectors.toList());
  }
}
