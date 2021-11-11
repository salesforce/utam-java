/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import utam.core.declarative.representation.TypeProvider;

/**
 * basic element union type
 *
 * @since 234
 * @author jim.evans
 */
public class BasicElementUnionType implements TypeProvider {

  private final String name;
  private final List<BasicElementInterface> basicInterfaces = new ArrayList<>();

  BasicElementUnionType(String name, String[] interfaceTypes, boolean requiresPrefix) {
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
    final String prefix = requiresPrefix ? "Get" : "";
    this.name = prefix + name.substring(0, 1).toUpperCase() + name.substring(1) + "Element";
    for(String interfaceType : interfaceTypes) {
      if (BasicElementInterface.isBasicType(interfaceType)) {
        basicInterfaces.add(BasicElementInterface.asBasicType(interfaceType));
      }
    }
  }

  BasicElementInterface[] getTypesArray() {
    return basicInterfaces.toArray(BasicElementInterface[]::new);
  }

  static boolean isBasicType(String[] interfaceTypes) {
    if (interfaceTypes == null) {
      return true;
    }
    for (String interfaceType : interfaceTypes) {
      if (!BasicElementInterface.isBasicType(interfaceType)) {
        return false;
      }
    }
    return true;
  }

  public static BasicElementUnionType asBasicType(
      String name, String[] interfaceTypes, boolean isPublicImplOnly) {
    if (interfaceTypes == null || interfaceTypes.length == 0) {
      return new BasicElementUnionType(
          name, new String[] { BASIC_ELEMENT.getSimpleName() }, isPublicImplOnly);
    }
    if (isBasicType(interfaceTypes)) {
      return new BasicElementUnionType(name, interfaceTypes, isPublicImplOnly);
    }
    return null;
  }

  public List<TypeProvider> getBasicInterfaces() {
    if (basicInterfaces.size() == 0) {
      // If there are no basic interfaces declared, the only interface implemented by this
      // element is BasicElement.
      return Collections.singletonList(BASIC_ELEMENT);
    }
    return new ArrayList<>(basicInterfaces);
  }

  @Override
  public String getFullName() {
    return name;
  }

  @Override
  public String getSimpleName() {
    return name;
  }

  @Override
  public String getPackageName() {
    return "";
  }

  @Override
  public Class getClassType() {
    return null;
  }
}
