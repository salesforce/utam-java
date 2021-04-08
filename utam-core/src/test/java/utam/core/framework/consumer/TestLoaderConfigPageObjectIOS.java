/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageMarker;
import utam.core.framework.consumer.TestLoaderConfigPageObject;
import utam.core.selenium.element.Actionable;

/**
 * used to test loader config
 */
@PageMarker.Find(css = "root")
public class TestLoaderConfigPageObjectIOS extends BasePageObject implements TestLoaderConfigPageObject {

  @Override
  public Actionable getRoot() {
    return getRootElement();
  }

  @Override
  public void load() {
    // nothing
  }
}
