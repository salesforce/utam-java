/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import utam.core.element.Locator;
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageMarker;

/** used to test loader config */
@PageMarker.Find(css = "root")
public class TestLoaderConfigPageObjectProfile extends BasePageObject
    implements TestLoaderConfigPageObject, TestLoaderConfigDefault {

  @Override
  public Locator getRoot() {
    return super.getRootLocator();
  }

  @Override
  public Object load() {
    // nothing
    return this;
  }
}
