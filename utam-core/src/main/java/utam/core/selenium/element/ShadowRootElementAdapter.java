/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import static utam.core.selenium.element.DriverAdapter.getNotFoundErr;

import java.util.ArrayList;
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
  public Element findElement(Locator locator, boolean isNullable) {
    List<Element> res = findElements(locator, isNullable);
    // null only returned for nullable, otherwise throws
    if (res == null) {
      return null;
    }
    return res.get(0);
  }

  @Override
  public List<Element> findElements(Locator locator, boolean isNullable) {
    By by = ((LocatorBy) locator).getValue();
    List<WebElement> res =
        driverAdapter.waitFor(
            () -> {
              List<WebElement> found = getWebElement().findElements(by);
              if (found == null || found.isEmpty()) {
                if (isNullable) {
                  return new ArrayList<>();
                }
                throw new NoSuchElementException(getNotFoundErr(locator));
              }
              return found;
            },
            getNotFoundErr(locator),
            driverAdapter.getDriverConfig().getImplicitTimeout());
    // empty only returned for nullable, otherwise throws
    if (res.isEmpty()) {
      return null;
    }
    return res.stream().map(this::wrapElement).collect(Collectors.toList());
  }
}
