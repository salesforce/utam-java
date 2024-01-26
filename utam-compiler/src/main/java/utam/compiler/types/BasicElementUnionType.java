/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.types;

import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.helpers.TypeUtilities.FromString;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;

/**
 * implementation for union type
 *
 * @author jim.evans, elizaveta.ivanova
 * @since 236
 */
public class BasicElementUnionType extends FromString implements UnionType {

  private final List<TypeProvider> extendedTypes;

  private BasicElementUnionType(String shortName, List<TypeProvider> extendedTypes) {
    super(shortName);
    this.extendedTypes = extendedTypes;
  }

  // If the basic element being defined is in an implementation-only Page Object,
  // that is, one that has an "implements" property defined, and the element is
  // marked as public, then there must be a method defined in the interface-only
  // Page Object that matches the name of the exposed element. Assuming the method
  // is named "getFoo," that method will have a generated return type of interface
  // "GetFooElement", and the implementation Page Object must have an implementation
  // for the element that will match that interface name. Note carefully that this
  // is distinct from the case where the element is normally exposed in a Page Object
  // using the "public" property in the JSON, in which case, the "Get" prefix is
  // omitted.
  private static String getBasicTypeName(String name, boolean requiresPrefix) {
    final String prefix = requiresPrefix ? "Get" : "";
    return prefix + name.substring(0, 1).toUpperCase() + name.substring(1) + "Element";
  }

  /**
   * if extended interfaces are set, create union type, otherwise return basic
   *
   * @param elementName name of the element will be used to build type name
   * @param interfaceTypes extended types, can be empty
   * @param isPublicImplOnly for impl only POs type name has GET prefix
   * @return union type or basic element type
   */
  public static TypeProvider asBasicOrUnionType(
      String elementName, String[] interfaceTypes, boolean isPublicImplOnly) {
    if (interfaceTypes == null || interfaceTypes.length == 0) {
      return BASIC_ELEMENT;
    }
    String shortName = getBasicTypeName(elementName, isPublicImplOnly);
    List<TypeProvider> basicInterfaces = new ArrayList<>();
    for (String interfaceType : interfaceTypes) {
      basicInterfaces.add(BasicElementInterface.valueOf(interfaceType));
    }
    return new BasicElementUnionType(shortName, basicInterfaces);
  }

  /**
   * if passed type is a union type or list of union types, unwrap and return
   *
   * @param type type that can be regular or union type
   * @return union type or null
   */
  public static UnionType asUnionTypeOrNull(TypeProvider type) {
    if (type instanceof UnionType) {
      return (UnionType) type;
    }
    TypeProvider boundType = type.getBoundType();
    if (boundType != null) {
      return asUnionTypeOrNull(boundType);
    }
    return null;
  }

  @Override
  public List<TypeProvider> getExtendedTypes() {
    return extendedTypes;
  }

  @Override
  public List<String> getDeclarationCode() {
    String extended =
        extendedTypes.stream().map(TypeProvider::getSimpleName).collect(Collectors.joining(", "));
    String code = String.format("interface %s extends %s {}", getSimpleName(), extended);
    return Collections.singletonList(code);
  }
}
