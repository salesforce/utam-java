/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.representation;

import java.util.List;

/**
 * page object method
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface PageObjectMethod {

  MethodDeclaration getDeclaration();

  List<String> getCodeLines();

  List<TypeProvider> getClassImports();

  boolean isPublic();

  default boolean isElementMethod() {
    return false;
  }
}
