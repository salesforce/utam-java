/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import utam.core.declarative.representation.TypeProvider;

/**
 * primitive types supported as either return type or parameter in JSON
 *
 * @author elizaveta.ivanova
 * @since 226
 */
@SuppressWarnings("rawtypes")
public enum PrimitiveType implements TypeProvider {
  /** A string type */
  STRING(String.class, "String", "string"),

  /** Aa numeric type */
  NUMBER(Integer.class, "Integer", "number"),

  /** A boolean type */
  BOOLEAN(Boolean.class, "Boolean", "boolean");

  /** An empty array of primitive types */
  public static final PrimitiveType[] EMPTY_ARRAY = new PrimitiveType[0];

  private final Class type; // used in tests
  private final String typeName;
  private final String typeFromJson;

  PrimitiveType(Class type, String typeName, String typeFromJson) {
    this.type = type;
    this.typeName = typeName;
    this.typeFromJson = typeFromJson;
  }

  /**
   * Creates a primitive type from a string
   *
   * @param typeString the string describing to create
   * @return the type described by the string
   */
  public static PrimitiveType fromString(String typeString) {
    for (PrimitiveType primitive : PrimitiveType.values()) {
      if (primitive.typeFromJson.equals(typeString)) {
        return primitive;
      }
    }
    return null;
  }

  /**
   * Gets a value indicating whether the JSON string represents a primitive type
   *
   * @param jsonString the JSON string to evaluate
   * @return true if the JSON string represents a primitive type; otherwise false
   */
  public static boolean isPrimitiveType(String jsonString) {
    if (jsonString == null) {
      return false;
    }
    for (PrimitiveType primitive : PrimitiveType.values()) {
      if (jsonString.equals(primitive.typeFromJson)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Gets the JSON type name
   *
   * @return the JSON type name
   */
  public String getJsonTypeName() {
    return typeFromJson;
  }

  @Override
  public String getFalsyValue() {
    return BOOLEAN == this ? "false" : "null";
  }

  @Override
  public String getFullName() {
    return type.getName();
  }

  @Override
  public String getSimpleName() {
    return typeName;
  }

  @Override
  public String getPackageName() {
    return type.getPackageName();
  }
}
