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
   * used in ContainerElement.setScope(Contained externalObjectInsideContainer)
   *
   * @param scopeSupplier supplier of the SearchContext from UTAM element root
   * @deprecated not supported outside Salesforce engineering teams
   */
  @Deprecated
  void setScope(Supplier<SearchContext> scopeSupplier);

  /**
   * used to set the root element of the Page Object
   *
   * @param rootSupplier supplier of the SearchContext from UTAM element root
   * @deprecated legacy method, to be removed
   */
  @Deprecated
  default void setRoot(Supplier<SearchContext> rootSupplier) {
    throw new UnsupportedOperationException("Unsupported functionality");
  }
}
