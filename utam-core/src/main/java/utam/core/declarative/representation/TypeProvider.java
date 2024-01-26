/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.representation;

import java.util.Collections;
import java.util.List;

/**
 * type of utam objects for imports and declarations
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface TypeProvider {

  /**
   * full class name to be used in imports statement
   *
   * @return full class name to use in import, ex. spec.generated.ui.Input
   */
  String getFullName();

  /**
   * Simple class name to be used in declaration
   *
   * @return simple class name
   */
  String getSimpleName();

  /**
   * When using type as return in method declaration, string can be different for "bounded class"
   * types
   *
   * @return string, by default it's simple name
   */
  default String getReturnString() {
    return getSimpleName();
  }

  /**
   * name of the package, used in translator to declare package
   *
   * @return package name
   */
  String getPackageName();

  /**
   * gets a value indicating whether a type provider represents the same type as this type provider
   *
   * @param anotherType the type provider to declare
   * @return true if the type provider represents the same type as this type provider; otherwise,
   *     false
   */
  default boolean isSameType(TypeProvider anotherType) {
    return anotherType != null
        && this.getFullName().equals(anotherType.getFullName())
        && (this.getSimpleName().equals(anotherType.getSimpleName()));
  }

  /**
   * used to return from predicate if element is nullable and returned null
   *
   * @return string with falsy value
   */
  default String getFalsyValue() {
    return "null";
  }

  /**
   * Get "bound" type, for example for List&lt;PageObject&gt; the returned type would be PageObject
   *
   * @return bound type or null
   */
  default TypeProvider getBoundType() {
    return null;
  }

  /**
   * because of bound types, we might need to import more than type itself
   *
   * @return list of all types to import
   */
  default List<TypeProvider> getImportableTypes() {
    return Collections.singletonList(this);
  }
}
