package utam.compiler.helpers;

import java.util.function.Function;
import java.util.function.Supplier;
import utam.core.declarative.representation.TypeProvider;

import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.core.selenium.element.Selector;

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
  BOOLEAN(Boolean.class, "Boolean", "boolean"),
  PREDICATE(Supplier.class, "Supplier<T>", "predicate"),
  LOCATOR(Selector.class, "Selector", "locator");

  public static final PrimitiveType[] EMPTY_ARRAY = new PrimitiveType[0];
  public static final String SUPPORTED_TYPES =
      Stream.of(PrimitiveType.values())
          .map(value -> value.typeFromJson)
          .collect(Collectors.joining(","));
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

  @Override
  public Class getClassType() {
    return type;
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
