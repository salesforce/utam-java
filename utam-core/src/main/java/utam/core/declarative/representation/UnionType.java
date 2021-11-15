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
 * Declared custom union interface type
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public interface UnionType {

  boolean isPublic();

  TypeProvider getType();

  List<TypeProvider> getExtendedTypes();

  List<String> getDeclarationCode();
}
