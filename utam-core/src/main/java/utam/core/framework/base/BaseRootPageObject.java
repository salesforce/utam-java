/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static utam.core.framework.element.BasePageElement.createInstance;

import utam.core.element.Element;
import utam.core.framework.element.BasePageElement;

/**
 * base class for a Root Page Objects
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class BaseRootPageObject extends BasePageObject implements RootPageObject {

  @Override
  protected final BasePageElement getRootElement() {
    if (rootElement == null) {
      if (getElement() == null) {
        Element root = getDriver().findElement(getRootLocator());
        setElement(root);
      }
      rootElement = createInstance(getElement(), getDriver());
    }
    return rootElement;
  }

  @Override
  public Object load() {
    log("load the object - wait for a root element to be found");
    this.waitFor(this::getRootElement);
    return this;
  }
}
