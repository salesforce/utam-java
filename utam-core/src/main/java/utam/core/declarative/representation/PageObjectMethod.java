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

  /**
   * get method declaration
   *
   * @return object
   */
  MethodDeclaration getDeclaration();

  /**
   * get generated code lines, statements without ";" att he end
   *
   * @return list
   */
  List<String> getCodeLines();

  /**
   * get types to be imported for a class. imports for declaration in an interface are part of
   * MethodDeclaration
   *
   * @return list of types to import
   */
  List<TypeProvider> getClassImports();

  /**
   * check if method is declared as public
   *
   * @return true if public
   */
  boolean isPublic();
}
