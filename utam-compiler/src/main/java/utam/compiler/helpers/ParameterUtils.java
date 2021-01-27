package utam.compiler.helpers;

import declarative.representation.MethodParameter;
import declarative.representation.TypeProvider;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class ParameterUtils {

  public static final List<MethodParameter> EMPTY_PARAMETERS = new ArrayList<>();

  public static String getParametersValuesString(List<MethodParameter> parameters) {
    return parameters.stream().map(MethodParameter::getValue).collect(Collectors.joining(","));
  }

  public static class Regular implements MethodParameter {

    private final String valueAsString;
    private final TypeProvider type;

    public Regular(String valueAsString, TypeProvider type) {
      this.valueAsString = valueAsString;
      this.type = type;
    }

    @Override
    public boolean isLiteral() {
      return false;
    }

    @Override
    public String getValue() {
      return valueAsString;
    }

    @Override
    public String getDeclaration() {
      return String.format("%s %s", type.getSimpleName(), getValue());
    }

    @Override
    public TypeProvider getType() {
      return type;
    }

    @Override
    public boolean equals(Object obj) {
      if(!(obj instanceof MethodParameter)) {
        return false;
      }
      return this.getDeclaration().equals(((MethodParameter)obj).getDeclaration());
    }
  }

  public static class Primitive extends Regular {

    public Primitive(String value, PrimitiveType type) {
      super(value, type);
    }
  }

  public static class Literal extends Regular {

    public Literal(Object value, PrimitiveType type) {
      super(value.toString(), type);
    }

    @Override
    public String getDeclaration() {
      return "";
    }

    @Override
    public boolean isLiteral() {
      return true;
    }
  }
}
