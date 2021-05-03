/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.representation;

/**
 * method parameter
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface MethodParameter {

  boolean isLiteral();

  boolean isSelectorArgument();

  String getValue();

  String getDeclaration();

  TypeProvider getType();
}
