/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.types;

import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;
import utam.core.element.Actionable;
import utam.core.element.Clickable;
import utam.core.element.Draggable;
import utam.core.element.Editable;
import utam.core.element.Touchable;

/**
 * supported types of basic element
 *
 * @author jim.evans
 * @since 234
 */
public enum BasicElementInterface implements TypeProvider {
  /**
   * An actionable element
   */
  actionable(Actionable.class),

  /**
   * A clickable element
   */
  clickable(Clickable.class),

  /**
   * A draggable element
   */
  draggable(Draggable.class),

  /**
   * An editable element
   */
  editable(Editable.class),

  /**
   * A touchable element
   */
  touchable(Touchable.class);

  private final Class type;

  BasicElementInterface(Class type) {
    this.type = type;
  }

  public static boolean isBasicType(String jsonString) {
    for (BasicElementInterface type : BasicElementInterface.values()) {
      if (type.name().equals(jsonString)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Gets a value indicating whether the element is a basic element type
   * @param type the type of the element
   * @return true if the element is a basic type; otherwise, false
   */
  public static boolean isBasicType(TypeProvider type) {
    if (type instanceof UnionType || type.isSameType(BASIC_ELEMENT)) {
      return true;
    }
    for (BasicElementInterface basicType : BasicElementInterface.values()) {
      if (basicType.isSameType(type)) {
        return true;
      }
    }
    return false;
  }

  /**
   * process Json node with basic types, applicable to root or basic elements
   *
   * @param typeNode Json node
   * @param error    error message supplier
   * @return string array with basic types or empty array
   */
  public static String[] processBasicTypeNode(JsonNode typeNode, Function<JsonNode, RuntimeException> error) {
    if (typeNode == null || typeNode.isNull()) {
      return new String[]{};
    }
    if (typeNode.isTextual() && isBasicType(typeNode.textValue())) {
      return new String[]{typeNode.textValue()};
    }
    if (typeNode.isArray()) {
      List<String> values = new ArrayList<>();
      for (JsonNode valueNode : typeNode) {
        if (!valueNode.isTextual()) {
          throw error.apply(typeNode);
        }
        String valueStr = valueNode.textValue();
        if (!isBasicType(valueStr)) {
          throw error.apply(typeNode);
        }
        values.add(valueStr);
      }
      return values.toArray(String[]::new);
    }
    throw error.apply(typeNode);
  }

  @Override
  public String getFullName() {
    return type.getName();
  }

  @Override
  public String getSimpleName() {
    return type.getSimpleName();
  }

  @Override
  public String getPackageName() {
    return type.getPackageName();
  }

  @Override
  public Class getClassType() {
    return type;
  }
}
