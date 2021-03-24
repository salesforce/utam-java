package utam.compiler.grammar;

import static utam.compiler.helpers.PrimitiveType.BOOLEAN;
import static utam.compiler.helpers.PrimitiveType.NUMBER;
import static utam.compiler.helpers.PrimitiveType.STRING;
import static utam.compiler.helpers.PrimitiveType.isPrimitiveType;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParameterUtils.Literal;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
@JsonDeserialize(using = UtamArgumentDeserializer.class)
class UtamArgument {

  static final String FUNCTION_TYPE_PROPERTY = "function";
  static final String SELECTOR_TYPE_PROPERTY = "locator";

  static final String SUPPORTED_ARGS_TYPES =
      Stream.concat(Stream.of(FUNCTION_TYPE_PROPERTY, SELECTOR_TYPE_PROPERTY),
          Stream.of(PrimitiveType.values())
              .map(value -> value.getJsonTypeName()))
          .collect(Collectors.joining(","));

  static final String ERR_ARGS_NAME_TYPE_MANDATORY =
      "%s: argument name and type are required";
  static final String ERR_ARGS_TYPE_NOT_SUPPORTED =
      "%s: args type '%s' is not supported, supported are { " + SUPPORTED_ARGS_TYPES + " }";
  static final String ERR_ARGS_WRONG_TYPE =
      "%s: for parameter '%s' expected type is '%s', actual was '%s'";
  static final String ERR_VALUE_REDUNDANT =
      "%s: property 'value' is redundant";
  static final String ERR_NAME_TYPE_REDUNDANT =
      "%s: argument already has value, properties 'name' and 'type' are redundant";
  static final String ERR_FUNCTION_NEEDS_PREDICATE = "%s: type function needs a non-empty predicate";
  static final String ERR_PREDICATE_REDUNDANT =
      "%s: property 'predicate' is redundant";

  String name;
  String type;
  Object value;
  UtamMethodAction[] conditions;

  @JsonCreator
  UtamArgument(
      @JsonProperty(value = "value") Object value,
      @JsonProperty(value = "name") String name,
      @JsonProperty(value = "type") String type,
      @JsonProperty(value = "predicate") UtamMethodAction[] conditions) {
    this.name = name;
    this.type = type;
    this.value = value;
    this.conditions = conditions;
  }

  // used in tests
  UtamArgument(String name, String type) {
    this(null, name, type, null);
  }

  // used in tests
  UtamArgument(Object value) {
    this(value, null, null, null);
  }

  static Processor getArgsProcessor(
      UtamArgument[] args, List<TypeProvider> expectedTypes, String argsContext) {
    return new Processor(argsContext, args, expectedTypes);
  }

  static Processor getArgsProcessor(UtamArgument[] args, String argsContext) {
    return getArgsProcessor(args, null, argsContext);
  }

  MethodParameter getParameterOrValue(String argsContext, TypeProvider expectedType) {
    if (FUNCTION_TYPE_PROPERTY.equals(type)) {
      if (value != null) {
        throw new UtamError(String.format(ERR_VALUE_REDUNDANT, argsContext));
      }
      // predicate is not a parameter
      return null;
    }
    if (conditions != null) {
      throw new UtamError(String.format(ERR_PREDICATE_REDUNDANT, argsContext));
    }
    MethodParameter parameter =
        value != null ? getArgByValue(argsContext) : getArgByNameType(argsContext);
    TypeProvider actualType = parameter.getType();
    if (expectedType != null && !actualType.isSameType(expectedType)) {
      throw new UtamError(
          String.format(
              ERR_ARGS_WRONG_TYPE,
              argsContext,
              parameter.getValue(),
              expectedType.getSimpleName(),
              actualType.getSimpleName()));
    }
    return parameter;
  }

  private MethodParameter getArgByValue(String argsContext) {
    if (name != null || type != null) {
      throw new UtamError(String.format(ERR_NAME_TYPE_REDUNDANT, argsContext));
    }
    TypeProvider type;
    if (value instanceof UtamSelector) {
      type = SELECTOR;
    } else if (value instanceof Boolean) {
      type = BOOLEAN;
    } else if (value instanceof Number) {
      type = NUMBER;
    } else if (value instanceof String) {
      type = STRING;
    } else {
      throw new UtamError(
          String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, argsContext, value.getClass().getName()));
    }
    return new Literal(value, type);
  }

  private MethodParameter getArgByNameType(String argsContext) {
    // we already excluded case of value not being null
    if (name == null || type == null) {
      throw new UtamError(String.format(ERR_ARGS_NAME_TYPE_MANDATORY, argsContext));
    }
    if (isPrimitiveType(type)) {
      return new Regular(name, PrimitiveType.fromString(type));
    } else if (SELECTOR_TYPE_PROPERTY.equals(type)) {
      return new Regular(name, SELECTOR);
    }
    throw new UtamError(String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, argsContext, type));
  }

  List<ComposeMethodStatement> getPredicate(TranslationContext context,
      MethodContext methodContext) {
    String argsContext = String.format("method '%s' args", methodContext.getName());
    if (value != null) {
      throw new UtamError(String.format(ERR_VALUE_REDUNDANT, argsContext));
    }
    if (conditions == null || conditions.length == 0) {
      throw new UtamError(String.format(ERR_FUNCTION_NEEDS_PREDICATE, argsContext));
    }
    return Stream.of(conditions).map(c -> c.getComposeAction(context, methodContext))
        .collect(Collectors.toList());
  }


  static class Processor {

    static final String ERR_ARGS_DUPLICATE_NAMES = "%s: duplicate arguments names '%s'";
    static final String ERR_ARGS_WRONG_COUNT = "%s: expected %d parameters, provided %d";

    final UtamArgument[] args;
    final String argsContext;
    private final List<TypeProvider> expectedTypes;
    List<MethodParameter> orderedParameters = new ArrayList<>();

    Processor(String argsContext, UtamArgument[] args, List<TypeProvider> expectedTypes) {
      this.args = args;
      this.expectedTypes = expectedTypes;
      this.argsContext = argsContext;
      int actualCnt = args == null ? 0 : args.length;
      if (expectedTypes != null && expectedTypes.size() != actualCnt) {
        throw new UtamError(
            String.format(ERR_ARGS_WRONG_COUNT, argsContext, expectedTypes.size(),
                actualCnt));
      }
      if (args != null) {
        Set<String> uniqueNamesTracker = new HashSet<>();
        for (int i = 0; i < args.length; i++) {
          setParameter(i, uniqueNamesTracker);
        }
      }
    }

    private void setParameter(int index, Set<String> uniqueNames) {
      MethodParameter parameter =
          args[index].getParameterOrValue(argsContext,
              expectedTypes == null ? null : expectedTypes.get(index));
      if (parameter != null) {
        // check unique name
        if (uniqueNames.contains(parameter.getValue())) {
          throw new UtamError(
              String.format(ERR_ARGS_DUPLICATE_NAMES, argsContext, parameter.getValue()));
        }
        String name = args[index].name;
        if (name != null) {
          uniqueNames.add(name);
        }
        orderedParameters.add(parameter);
      }
    }

    final List<MethodParameter> getOrdered() {
      return orderedParameters;
    }
  }
}
