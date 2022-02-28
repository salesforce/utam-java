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
 * method declaration signature
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface MethodDeclaration {

  /**
   * Gets the name of the method declaration
   *
   * @return the name of the method declaration
   */
  String getName();

  /**
   * Gets the list of parameters of the method declaration
   *
   * @return the list of parameters of the method declaration
   */
  List<MethodParameter> getParameters();

  /**
   * Gets the return type of the method declaration
   *
   * @return the return type of the method declaration
   */
  TypeProvider getReturnType();

  /**
   * Gets the list of imports of the method declaration
   *
   * @return the list of imports of the method declaration
   */
  List<TypeProvider> getImports();

  /**
   * Gets the code line of the method declaration
   *
   * @return the code line of the method declaration
   */
  String getCodeLine();

  /**
   * get text for javadoc
   *
   * @return list of strings of javadoc compatible format
   */
  List<String> getDescription();

  /**
   * method can be marked as deprecated
   *
   * @return boolean
   */
  boolean isDeprecated();
}
