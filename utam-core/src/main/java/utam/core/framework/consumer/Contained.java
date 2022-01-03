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
 * External page object that can be located inside UTAM page object, compatibility mode only,
 * Selenium only. Example of usage: public MyPageObjectClass implements Contained { // override
 * setRoot, setScope }.
 *
 * @author elizaveta.ivanova
 * @since 228
 * @deprecated not supported outside Salesforce engineering teams
 */
@Deprecated
public interface Contained {

  /**
   * used in CustomElementBuilder: when loading Contained as custom element, inject element locator
   * as root
   *
   * @param rootSupplier supplier of the root from UTAM element
   */
  void setRoot(Supplier<SearchContext> rootSupplier);

  /**
   *
   * @param scopeSupplier
   */
  void setScope(Supplier<SearchContext> scopeSupplier);
}
