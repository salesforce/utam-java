/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.types;

import java.util.Collections;
import java.util.List;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.helpers.TypeUtilities.FromString;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;

/**
 * implementation class for basic union type
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class BasicElementUnionTypeImpl extends FromString implements UnionType {

  private static final TypeProvider extendedType = TypeUtilities.BASIC_ELEMENT_IMPL_CLASS;
  private final TypeProvider implementedInterface;

  /**
   * Initializes a new instance of the BasicElementUnionTypeImpl class
   *
   * @param implementedInterface the interface to be implemented
   */
  public BasicElementUnionTypeImpl(TypeProvider implementedInterface) {
    super(implementedInterface.getSimpleName() + "Impl");
    this.implementedInterface = implementedInterface;
  }

  @Override
  public List<TypeProvider> getExtendedTypes() {
    return Collections.singletonList(extendedType);
  }

  @Override
  public List<String> getDeclarationCode() {
    String code =
        String.format(
            "public static class %s extends %s implements %s {}",
            getSimpleName(), extendedType.getSimpleName(), implementedInterface.getSimpleName());
    return Collections.singletonList(code);
  }
}
