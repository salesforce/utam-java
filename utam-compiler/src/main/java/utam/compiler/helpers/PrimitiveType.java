package utam.compiler.helpers;

import declarative.representation.TypeProvider;

import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * primitive types supported for element actions
 *
 * @author elizaveta.ivanova
 * @since 226
 */
@SuppressWarnings("rawtypes")
public enum PrimitiveType implements TypeProvider {
  STRING(String.class, "String"),
  NUMBER(Integer.class, "Integer"),
  BOOLEAN(Boolean.class, "Boolean"),
  CLASS(Class.class, "Class<T>"),
  VOID(Void.class, "void");

  public static final PrimitiveType[] EMPTY_ARRAY = new PrimitiveType[0];
  public static final String SUPPORTED_TYPES =
      Stream.of(STRING, NUMBER, BOOLEAN)
          .map(value -> value.name().toLowerCase())
          .collect(Collectors.joining(","));
  private final Class type; // used in tests
  private final String typeName;

  PrimitiveType(Class type, String typeName) {
    this.type = type;
    this.typeName = typeName;
  }

  public static PrimitiveType fromString(String type) {
    for (PrimitiveType primitive : PrimitiveType.values()) {
      if (primitive.is(type)) {
        return primitive;
      }
    }
    return null;
  }

  public Class getClassType() {
    return type;
  }

  @Override
  public String getFullName() {
    return "";
  }

  @Override
  public String getSimpleName() {
    return typeName;
  }

  @Override
  public String getPackageName() {
    return "";
  }

  private boolean is(String typeString) {
    return this.name().toLowerCase().equals(typeString);
  }

  //used in test to check action return types and parameters
  public boolean equals(Class clazz) {
    if(this == VOID) {
      return clazz.getName().toLowerCase().contains("void");
    }
    if(clazz.equals(type)) {
      return true;
    }
    if(this == NUMBER) {
      return clazz.getName().toLowerCase().startsWith("int");
    }
    if(this == BOOLEAN) {
      return clazz.getName().toLowerCase().equals(typeName.toLowerCase());
    }
    return false;
  }
}
