package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.PrimitiveType;
import declarative.representation.MethodParameter;
import declarative.representation.TypeProvider;
import framework.consumer.UtamError;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static utam.compiler.helpers.PrimitiveType.*;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
class UtamArgument {

  static final String ERR_ARGS_NAME_TYPE_MANDATORY =
      "%s: argument name and type are required";
  static final String ERR_ARGS_TYPE_NOT_SUPPORTED =
      "%s: type '%s' is not supported, supported are primitive types " + SUPPORTED_TYPES;
  static final String ERR_ARGS_WRONG_TYPE =
      "%s: for parameter '%s' expected type is '%s', actual was '%s'";
  static final String ERR_ARGS_NUMBER_MISMATCH = "%s: expected %d parameters, provided %d";
  static final String ERR_LITERAL_NOT_SUPPORTED = "%s: literal argument is not supported";
  static final String ERR_LITERAL_REDUNDANT =
      "%s: argument already has literal value, name and type are redundant";

  private final String name;
  private final String type;
  private final Object value;

  @JsonCreator
  UtamArgument(
      @JsonProperty(value = "value") Object value,
      @JsonProperty(value = "name") String name,
      @JsonProperty(value = "type") String type) {
    this.name = name;
    this.type = type;
    this.value = value;
  }

  // used in tests
  UtamArgument(String name, String type) {
    this(null, name, type);
  }

  // used in tests
  UtamArgument(Object value) {
    this(value, null, null);
  }

  static Processor nonLiteralParameters(
      UtamArgument[] args, List<TypeProvider> expectedTypes, String elementName) {
    return new Processor(args, expectedTypes, elementName);
  }

  static Processor literalParameters(
      UtamArgument[] args, List<TypeProvider> expectedTypes, String elementName) {
    return new ProcessorSupportsLiteral(args, expectedTypes, elementName);
  }

  static Processor unknownTypesParameters(UtamArgument[] args, String elementName) {
    return new Processor(args, null, elementName);
  }

  private MethodParameter getNamedParameter(String elementName, TypeProvider expectedType) {
    if (name == null || type == null) {
      throw new UtamError(String.format(ERR_ARGS_NAME_TYPE_MANDATORY, elementName));
    }
    if (expectedType != null && expectedType.getSimpleName().equals(type)) {
      return new ParameterUtils.Regular(name, expectedType);
    }
    PrimitiveType primitiveType = PrimitiveType.fromString(type);
    if (primitiveType == null) {
      throw new UtamError(String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, elementName, type));
    }
    if (expectedType != null && !primitiveType.equals(expectedType)) {
      throw new UtamError(
          String.format(
              ERR_ARGS_WRONG_TYPE,
              elementName,
              name,
              expectedType.getSimpleName(),
              primitiveType.getSimpleName()));
    }
    return new ParameterUtils.Regular(name, primitiveType);
  }

  private MethodParameter getLiteralParameter(String elementName, TypeProvider expectedType) {
    if (name != null || type != null) {
      throw new UtamError(String.format(ERR_LITERAL_REDUNDANT, elementName));
    }
    PrimitiveType actualType = getValuePrimitiveType(elementName);
    if (expectedType != null && !actualType.equals(expectedType)) {
      throw new UtamError(
          String.format(
              ERR_ARGS_WRONG_TYPE,
              elementName,
              value,
              expectedType.getSimpleName(),
              actualType.getSimpleName()));
    }
    return new ParameterUtils.Literal(value, actualType);
  }

  private PrimitiveType getValuePrimitiveType(String nodeName) {
    if (value instanceof Boolean) {
      return BOOLEAN;
    } else if (value instanceof Number) {
      return NUMBER;
    } else if (value instanceof String) {
      return STRING;
    }
    throw new UtamError(
        String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, nodeName, value.getClass().getName()));
  }

  static class ProcessorSupportsLiteral extends Processor {

    ProcessorSupportsLiteral(
        UtamArgument[] args, List<TypeProvider> expectedTypes, String elementName) {
      super(args, expectedTypes, elementName);
    }

    @Override
    MethodParameter getParameter(int index, Set<String> uniqueNames) {
      if (args[index].value != null) {
        return args[index].getLiteralParameter(structureWithArgs, getExpectedType(index));
      }
      return super.getParameter(index, uniqueNames);
    }
  }

  static class Processor {

    static final String ERR_ARGS_DUPLICATE_NAMES = "'%s': duplicate arguments names '%s'";
    final UtamArgument[] args;
    final String structureWithArgs;
    private final List<TypeProvider> expectedTypes;

    Processor(UtamArgument[] args, List<TypeProvider> expectedTypes, String structureWithArgs) {
      this.args = args;
      this.expectedTypes = expectedTypes;
      this.structureWithArgs = structureWithArgs;
      int actualCnt = args == null ? 0 : args.length;
      if (expectedTypes != null && expectedTypes.size() != actualCnt) {
        throw new UtamError(
            String.format(ERR_ARGS_NUMBER_MISMATCH, structureWithArgs, expectedTypes.size(), actualCnt));
      }
    }

    final TypeProvider getExpectedType(int i) {
      return expectedTypes == null ? null : expectedTypes.get(i);
    }

    // override when supporting literals
    MethodParameter getParameter(int index, Set<String> uniqueNames) {
      if (args[index].value != null) {
        throw new UtamError(String.format(ERR_LITERAL_NOT_SUPPORTED, structureWithArgs));
      }
      MethodParameter parameter =
          args[index].getNamedParameter(structureWithArgs, getExpectedType(index));
      // check unique name
      if (uniqueNames.contains(parameter.getValue())) {
        throw new UtamError(
            String.format(ERR_ARGS_DUPLICATE_NAMES, structureWithArgs, parameter.getValue()));
      }
      uniqueNames.add(parameter.getValue());
      return parameter;
    }

    final List<MethodParameter> getOrdered() {
      List<MethodParameter> res = new ArrayList<>();
      if (args == null) {
        return res;
      }
      Set<String> uniqueNames = new HashSet<>();
      for (int i = 0; i < args.length; i++) {
        res.add(getParameter(i, uniqueNames));
      }
      return res;
    }
  }
}
