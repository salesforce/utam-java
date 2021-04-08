/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import utam.core.selenium.element.Locator;

/**
 * bootstrap parameters
 *
 * @author elizaveta.ivanova
 * @since 226
 */
interface BootstrapParameters {

  Locator getScopedRoot();
}
