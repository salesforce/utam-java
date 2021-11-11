/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.core.declarative.representation.TypeProvider;
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
  actionable(Actionable.class),
  clickable(Clickable.class),
  draggable(Draggable.class),
  editable(Editable.class),
  touchable(Touchable.class);

  public static final String ERR_UNSUPPORTED_ELEMENT_TYPE = "element '%s': type %s is not supported, "
      + "valid values are: " + nameList();
  private final Class type;

  BasicElementInterface(Class type) {
    this.type = type;
  }

  public static boolean isBasicType(String jsonString) {
    for (BasicElementInterface type : BasicElementInterface
        .values()) {
      if (type.name().equals(jsonString)) {
        return true;
      }
    }
    return false;
  }

  static BasicElementInterface asBasicType(String jsonString) {
    if (jsonString == null) {
      return actionable;
    }
    for (BasicElementInterface type : BasicElementInterface
        .values()) {
      if (jsonString.equals(type.name())) {
        return type;
      }
    }
    return null;
  }

  public static boolean isBasicType(TypeProvider type) {
    if (type instanceof BasicElementUnionType) {
      return true;
    }
    for (BasicElementInterface basicType : BasicElementInterface.values()) {
      if (basicType.isSameType(type)) {
        return true;
      }
    }
    return false;
  }

  public static BasicElementInterface[] getBasicElementTypes(TypeProvider type) {
    if (type instanceof BasicElementUnionType) {
      return ((BasicElementUnionType) type).getTypesArray();
    }
    BasicElementInterface basicInterface = getBasicElementType(type);
    if (basicInterface != null) {
      return new BasicElementInterface[]{
          basicInterface
      };
    }
    return null;
  }

  public static BasicElementInterface getBasicElementType(TypeProvider type) {
    for (BasicElementInterface basicType : BasicElementInterface
        .values()) {
      if (basicType.isSameType(type)) {
        return basicType;
      }
    }
    return null;
  }

  public static String nameList() {
    return Arrays.stream(values())
        .map(Enum::name).collect(Collectors.joining(", "));
  }

  /**
   * process Json node with basic types, applicable to root or basic elements
   *
   * @param typeNode    Json node
   * @param elementName name of the element, used in error message
   * @return string array with basic types or empty array
   */
  public static String[] processBasicTypeNode(JsonNode typeNode, String elementName) {
    if (typeNode == null || typeNode.isNull()) {
      return new String[]{};
    }
    final String typeNodeValueError = String
        .format(ERR_UNSUPPORTED_ELEMENT_TYPE, elementName, typeNode.toPrettyString());
    if (typeNode.isTextual() && BasicElementInterface.isBasicType(typeNode.textValue())) {
      return new String[]{typeNode.textValue()};
    }
    if (typeNode.isArray()) {
      List<String> values = new ArrayList<>();
      for (JsonNode valueNode : typeNode) {
        if (!valueNode.isTextual()) {
          throw new UtamCompilationError(typeNodeValueError);
        }
        String valueStr = valueNode.textValue();
        if (!BasicElementInterface.isBasicType(valueStr)) {
          throw new UtamCompilationError(String
              .format(ERR_UNSUPPORTED_ELEMENT_TYPE, elementName, "\"" + valueStr + "\""));
        }
        values.add(valueStr);
      }
      return values.toArray(String[]::new);
    }
    throw new UtamCompilationError(typeNodeValueError);
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
