/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

/**
 * represents imperative extensions
 *
 * @author elizaveta.ivanova
 * @since 228
 */
interface ImperativeExtension<T extends PageObject> {

  T getInstance();
}
