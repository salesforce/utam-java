/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.BasicElementInterface.processBasicTypeNode;
import static utam.compiler.helpers.ElementContext.ROOT_ELEMENT_NAME;
import static utam.compiler.helpers.RootElementHelper.RootElementSettings.DEFAULT_TYPE_PROTECTED;

import com.fasterxml.jackson.databind.JsonNode;
import utam.compiler.helpers.TypeUtilities.FromClass;
import utam.compiler.representation.RootElementMethod.PrivateCustomType;
import utam.compiler.representation.RootElementMethod.ProtectedDefaultType;
import utam.compiler.representation.RootElementMethod.PublicCustomType;
import utam.compiler.representation.RootElementMethod.PublicDefaultType;
import utam.compiler.representation.UnionTypeImpl;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;
import utam.core.element.BasicElement;
import utam.core.element.Locator;
import utam.core.framework.element.BasePageElement;

/**
 * helper class for root element declaration
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public final class RootElementHelper {

  private static final TypeProvider DEFAULT_ROOT_ELEMENT_TYPE = new FromClass(
      BasePageElement.class);
  private static final TypeProvider PUBLIC_DEFAULT_ROOT_ELEMENT_TYPE = new FromClass(
      BasicElement.class);

  private final RootElementSettings settings;
  private final String[] rootElementType;

  public RootElementHelper(
      JsonNode typeNode,
      boolean isExposeRootElement) {
    rootElementType = processBasicTypeNode(typeNode, ROOT_ELEMENT_NAME);
    if (rootElementType.length == 0) {
      settings =
          isExposeRootElement ? RootElementSettings.DEFAULT_TYPE_PUBLIC : DEFAULT_TYPE_PROTECTED;
    } else {
      settings = isExposeRootElement ? RootElementSettings.CUSTOM_TYPE_PUBLIC
          : RootElementSettings.CUSTOM_TYPE_PRIVATE;
    }
  }

  /**
   * depending on root element settings, add root getter and union type
   *
   * @param context     translation context
   * @param rootLocator locator of the root element
   * @return root element context
   */
  public ElementContext setRootElementMethod(TranslationContext context, Locator rootLocator) {
    TypeProvider interfaceType = context.getSelfType();
    final ElementContext rootElement;
    final PageObjectMethod rootElementMethod;
    if (settings == RootElementSettings.DEFAULT_TYPE_PUBLIC) {
      // if type is not set and element should be public - add public getter that returns UtamBase
      rootElement = new ElementContext.Root(interfaceType, rootLocator,
          PUBLIC_DEFAULT_ROOT_ELEMENT_TYPE);
      rootElementMethod = new PublicDefaultType(PUBLIC_DEFAULT_ROOT_ELEMENT_TYPE);
      context.setMethod(rootElementMethod);
    } else if (settings == RootElementSettings.CUSTOM_TYPE_PRIVATE) {
      // if type is set, but element is private - use protected method BasePageObject.getRootElement
      BasicElementUnionType elementType = BasicElementUnionType
          .asBasicType(ROOT_ELEMENT_NAME, rootElementType, false);
      rootElement = new ElementContext.Root(interfaceType, rootLocator, elementType);
      UnionType unionType = new UnionTypeImpl(false, elementType, elementType.getBasicInterfaces());
      rootElementMethod = new PrivateCustomType(unionType);
      context.setMethod(rootElementMethod);
      context.setUnionType(unionType, false);
    } else if (settings == RootElementSettings.CUSTOM_TYPE_PUBLIC) {
      // if type is set and element should be public - add new type and public getter
      BasicElementUnionType elementType =
          BasicElementUnionType.asBasicType(ROOT_ELEMENT_NAME,
              rootElementType, false);
      rootElement = new ElementContext.Root(interfaceType, rootLocator, elementType);
      UnionType unionType = new UnionTypeImpl(false, elementType, elementType.getBasicInterfaces());
      rootElementMethod = new PublicCustomType(unionType);
      context.setMethod(rootElementMethod);
      context.setUnionType(unionType, true);
    } else {
      // if type is not set and root element is not exposed - use protected method BasePageObject.getRootElement
      rootElement = new ElementContext.Root(interfaceType, rootLocator, DEFAULT_ROOT_ELEMENT_TYPE);
      rootElementMethod = new ProtectedDefaultType(DEFAULT_ROOT_ELEMENT_TYPE);
    }
    context.setElement(rootElement);
    rootElement.setElementMethod(rootElementMethod);
    return rootElement;
  }

  /**
   * type of the root element
   */
  enum RootElementSettings {
    DEFAULT_TYPE_PUBLIC,
    DEFAULT_TYPE_PROTECTED,
    CUSTOM_TYPE_PUBLIC,
    CUSTOM_TYPE_PRIVATE
  }
}
