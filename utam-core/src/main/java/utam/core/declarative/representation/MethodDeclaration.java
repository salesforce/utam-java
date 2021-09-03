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

  String getName();

  List<MethodParameter> getParameters();

  TypeProvider getReturnType();

  List<TypeProvider> getImports();

  String getCodeLine();

  String getComments();
}
