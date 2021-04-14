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
 * external PO scoped inside UTAM, only supported for Selenium!
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface Contained {

  void setRoot(Supplier<SearchContext> rootSupplier);

  void setScope(Supplier<SearchContext> scopeSupplier);
}
