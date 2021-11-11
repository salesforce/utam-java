/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;

/**
 * implementation for union type
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class UnionTypeImpl implements UnionType {

  private final boolean isPublic;
  private final TypeProvider declaredType;
  private final List<TypeProvider> extendedTypes;

  public UnionTypeImpl(boolean isPublic,
      TypeProvider declaredType,
      List<TypeProvider> extendedTypes) {
    this.isPublic = isPublic;
    this.declaredType = declaredType;
    this.extendedTypes = extendedTypes;
  }

  @Override
  public boolean isPublic() {
    return isPublic;
  }

  @Override
  public TypeProvider getType() {
    return declaredType;
  }

  @Override
  public List<TypeProvider> getExtendedTypes() {
    return extendedTypes;
  }

  @Override
  public List<String> getDeclarationCode() {
    String extended = extendedTypes.stream().map(TypeProvider::getSimpleName).collect(Collectors.joining(", "));
    String code = String.format("interface %s extends %s {}", declaredType.getSimpleName(), extended);
    return Collections.singletonList(code);
  }
}
