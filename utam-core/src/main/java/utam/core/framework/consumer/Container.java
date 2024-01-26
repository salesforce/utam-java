/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import java.util.function.Supplier;
import org.openqa.selenium.SearchContext;

/**
 * External page object that can contain a UTAM page object, compatibility mode only, Selenium only.
 * Example of usage: public MyPageObjectClass implements Container { // override
 * "getScopeSupplier()" }.
 *
 * @author elizaveta.ivanova
 * @since 228
 * @deprecated not supported outside Salesforce engineering teams
 */
@Deprecated
public interface Container {

  /**
   * Supplier to get scope WebElement. Invoked from UtamLoader.create(Container
   * externalScopeProvider, Class&lt;T&gt; utamPageObjectType, Locator utamPageObjectRoot)
   *
   * @return supplier of a Selenium SearchContext
   */
  Supplier<SearchContext> getScope();
}
