/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import java.util.List;
import java.util.function.Predicate;

/**
 * builder for a custom element scoped inside a page object
 *
 * @author elizaveta.ivanova
 * @since 224
 */
public interface PageObjectBuilder {

  /**
   * scope page object of the custom type
   * if nothing found and element is not nullable, throws an error <br>
   *
   * @param type custom type
   * @param <T> custom generic type
   * @return instance of the Page Object of given type
   */
  <T extends PageObject> T build(Class<T> type);

  /**
   * scope page object of the custom type, then find all and apply filter to find first match <br>
   * if nothing found, throws an error <br>
   * if no match for filter found, throws if not nullable
   *
   * @param type custom type
   * @param <T> custom generic type
   * @param filter filter to apply to the found instances
   * @return instance of the Page Object of given type
   */
  <T extends PageObject> T build(Class<T> type, Predicate<T> filter);

  /**
   * scope page object of the custom type, then find all
   * if nothing found, throws an error <br>
   *
   * @param type custom type
   * @param <T> custom generic type
   * @return all found instances of the Page Object of given type
   */
  <T extends PageObject> List<T> buildList(Class<T> type);

  /**
   * scope page object of the custom type, then find all and apply filter<br>
   * if nothing found, throws <br>
   * if no match for filter found, throws if not nullable otherwise returns empty list
   *
   * @param type custom type
   * @param <T> custom generic type
   * @param filter filter to apply to the found instances
   * @return instance of the Page Object of given type
   */
  <T extends PageObject> List<T> buildList(Class<T> type, Predicate<T> filter);
}
