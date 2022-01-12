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
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
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

  /**
   * Error template string for an unsupported element type
   */
  public static final String ERR_UNSUPPORTED_ELEMENT_TYPE =
      "element '%s': type %s is not supported, "
          + "valid values are: " + Arrays.stream(values()).map(Enum::name)
          .collect(Collectors.joining(", "));
  private final Class type;

  BasicElementInterface(Class type) {
    this.type = type;
  }

  private static boolean isBasicType(String jsonString) {
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
   * @param typeNode    Json node
   * @param elementName name of the element, used in error message
   * @param isThrowAnError if set to true, non basic type will throw an error
   * @return string array with basic types or empty array
   */
  public static String[] processBasicTypeNode(JsonNode typeNode, String elementName, boolean isThrowAnError) {
    if (typeNode == null || typeNode.isNull()) {
      return new String[]{};
    }
    final String typeNodeValueError = String
        .format(ERR_UNSUPPORTED_ELEMENT_TYPE, elementName, typeNode.toPrettyString());
    if (typeNode.isTextual() && isBasicType(typeNode.textValue())) {
      return new String[]{typeNode.textValue()};
    }
    if (typeNode.isArray()) {
      List<String> values = new ArrayList<>();
      for (JsonNode valueNode : typeNode) {
        if (!valueNode.isTextual()) {
          if(isThrowAnError) {
            throw new UtamCompilationError(typeNodeValueError);
          } else {
            return null;
          }
        }
        String valueStr = valueNode.textValue();
        if (!isBasicType(valueStr)) {
          if(isThrowAnError) {
            throw new UtamCompilationError(String
                .format(ERR_UNSUPPORTED_ELEMENT_TYPE, elementName, "\"" + valueStr + "\""));
          } else {
            return null;
          }
        }
        values.add(valueStr);
      }
      return values.toArray(String[]::new);
    }
    if(isThrowAnError) {
      throw new UtamCompilationError(typeNodeValueError);
    } else {
      return null;
    }
  }

  /**
   * check if "returnType" in an interface method returns basic type
   *
   * @param returnTypeNode the node containing the return type
   * @return true if it is
   */
  public static boolean isReturnBasicType(JsonNode returnTypeNode) {
    if (returnTypeNode == null || returnTypeNode.isNull()) {
      return false;
    }
    return processBasicTypeNode(returnTypeNode, "returnType", false) != null;
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
