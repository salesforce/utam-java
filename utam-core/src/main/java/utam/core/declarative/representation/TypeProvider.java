/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.representation;

import java.util.ArrayList;
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
   * Simple class name to be used in field declaration
   *
   * @return simple class name
   */
  String getSimpleName();

  /**
   * name of the package, used in translator to declare package
   *
   * @return package name
   */
  String getPackageName();

  /**
   * returns Class of the type if possible
   *
   * @return class
   */
  Class getClassType();

  default boolean isSameType(TypeProvider anotherType) {
    return this.getFullName().equals(anotherType.getFullName())
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
   * some types require more than one import because they set bound, for example List&lt;PageObject&gt;
   * @return list of bound types, by default empty
   */
  default List<TypeProvider> getBoundTypes() {
    return new ArrayList<>();
  }
}
