/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import utam.core.element.Locator;
import utam.core.framework.base.RootPageObject;

/**
 * used in tests
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public interface TestLoaderConfigPageObject extends RootPageObject {

  Locator getRoot();
}
