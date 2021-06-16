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
  STRING(String.class, "String", "string"),
  NUMBER(Integer.class, "Integer", "number"),
  BOOLEAN(Boolean.class, "Boolean", "boolean");

  public static final PrimitiveType[] EMPTY_ARRAY = new PrimitiveType[0];
  private final Class type; // used in tests
  private final String typeName;
  private final String typeFromJson;

  PrimitiveType(Class type, String typeName, String typeFromJson) {
    this.type = type;
    this.typeName = typeName;
    this.typeFromJson = typeFromJson;
  }

  public static PrimitiveType fromString(String typeString) {
    for (PrimitiveType primitive : PrimitiveType.values()) {
      if (primitive.typeFromJson.equals(typeString)) {
        return primitive;
      }
    }
    return null;
  }

  public static boolean isPrimitiveType(String jsonString) {
    for (PrimitiveType primitive : PrimitiveType.values()) {
      if (jsonString.equals(primitive.typeFromJson)) {
        return true;
      }
    }
    return false;
  }

  public String getJsonTypeName() {
    return typeFromJson;
  }

  @Override
  public Class getClassType() {
    return type;
  }

  @Override
  public String getFalsyValue() {
    return BOOLEAN.equals(this)? "false" : "null";
  }

  @Override
  public String getFullName() {
    return getClassType().getName();
  }

  @Override
  public String getSimpleName() {
    return typeName;
  }

  @Override
  public String getPackageName() {
    return getClassType().getPackageName();
  }


}
