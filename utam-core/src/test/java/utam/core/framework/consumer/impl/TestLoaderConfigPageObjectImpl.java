/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer.impl;

import utam.core.element.Locator;
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageMarker;
import utam.core.framework.consumer.TestLoaderConfigPageObject;

@PageMarker.Find(css = "root")
public class TestLoaderConfigPageObjectImpl extends BasePageObject
    implements TestLoaderConfigPageObject {

  @Override
  public Locator getRoot() {
    return getRootLocator();
  }

  @Override
  public Object load() {
    // nothing
    return this;
  }
}
