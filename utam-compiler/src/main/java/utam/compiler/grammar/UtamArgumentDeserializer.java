/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.UtamArgument.ELEMENT_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.FUNCTION_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.SELECTOR_TYPE_PROPERTY;
import static utam.compiler.helpers.LocatorCodeGeneration.SUPPORTED_SELECTOR_TYPES;
import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.REFERENCE;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.IOException;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.UtamArgument.ArgsProcessor;
import utam.compiler.grammar.UtamArgument.ArgsProcessorWithExpectedTypes;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ParameterUtils.Literal;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * Custom deserializer for UtamArgument with purpose of parse literal objects, currently selector
 * and element. Parses JSON and provides validations that are possible without getting a Page Object
 * context.
 *
 * @author elizaveta.ivanova
 * @since 232
 */
class UtamArgumentDeserializer extends
    com.fasterxml.jackson.databind.JsonDeserializer<UtamArgument> {

  static final String ERR_PREDICATE_REDUNDANT = "property 'predicate' is only supported for 'function' type";
  static final String ERR_PREDICATE_MANDATORY = "property 'predicate' is mandatory for 'function' type";
  static final String ERR_ARGS_NAME_MANDATORY = "argument 'name' property, is required except for 'function' type";
  static final String ERR_ARGS_TYPE_MANDATORY = "argument 'type' is required except for 'function' type";
  static final String ERR_ARGS_LITERAL_TYPE_NOT_SUPPORTED = "args literal type for value '%s' is not supported";
  static final String ERR_ARGS_UNKNOWN_LITERAL_OBJECT = "args unknown literal object type ";
  private static final String ERR_NAME_TYPE_PREDICATE_REDUNDANT = "for literal argument '%s' properties 'name' or 'type' or 'predicate' are redundant";
  private static final Set<String> SUPPORTED_TYPES = getSupportedJsonTypes();
  private static final String SUPPORTED_TYPES_STRING = String.join(", ", getSupportedJsonTypes());
  private static final String ERR_ARGS_TYPE_NOT_SUPPORTED =
      "args type '%s' is not supported, supported are { " + SUPPORTED_TYPES_STRING + " }";

  private static Set<String> getSupportedJsonTypes() {
    Set<String> supported = Stream.of(PrimitiveType.values())
        .map(PrimitiveType::getJsonTypeName)
        .collect(Collectors.toSet());
    supported.add(SELECTOR_TYPE_PROPERTY);
    supported.add(FUNCTION_TYPE_PROPERTY);
    supported.add(REFERENCE.getSimpleName());
    supported.add(ELEMENT_TYPE_PROPERTY);
    return supported;
  }

  static String getRedundantForValueErr(String value) {
    return String.format(ERR_NAME_TYPE_PREDICATE_REDUNDANT, value);
  }

  static String getUnsupportedTypeErr(String type) {
    return String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, type);
  }

  private static boolean isLiteralSelector(JsonNode valueNode) {
    return SUPPORTED_SELECTOR_TYPES.stream().anyMatch(valueNode::has);
  }

  @Override
  public UtamArgument deserialize(JsonParser parser, DeserializationContext ctxt)
      throws IOException {
    ObjectMapper mapper = (ObjectMapper) parser.getCodec();
    ObjectNode rootArgNode = mapper.readTree(parser);

    JsonNode valueNode = rootArgNode.get("value");
    if (valueNode != null) {
      if (rootArgNode.has("name") || rootArgNode.has("type") || rootArgNode.has("predicate")) {
        throw new UtamCompilationError(getRedundantForValueErr(valueNode.toPrettyString()));
      }
      return getLiteralArg(valueNode, mapper);
    }

    return getNonLiteralArg(rootArgNode, mapper);
  }

  private UtamMethodAction[] getPredicateStatements(JsonNode predicateArrayNode,
      ObjectMapper mapper) {
    return StreamSupport.stream(predicateArrayNode.spliterator(), false)
        .map(predicateStatement -> {
          try {
            return mapper.treeToValue(predicateStatement, UtamMethodAction.class);
          } catch (JsonProcessingException e) {
            throw new UtamCompilationError(e);
          }
        })
        .toArray(UtamMethodAction[]::new);
  }

  private UtamArgument getNonLiteralArg(ObjectNode argNode, ObjectMapper mapper) {
    String name = argNode.has("name") ? argNode.get("name").asText() : null;
    if (!argNode.has("type")) {
      throw new UtamCompilationError(ERR_ARGS_TYPE_MANDATORY);
    }
    String type = argNode.get("type").asText();
    if (FUNCTION_TYPE_PROPERTY.equals(type)) {
      if (!argNode.has("predicate")) {
        throw new UtamCompilationError(ERR_PREDICATE_MANDATORY);
      }
      JsonNode predicateArrayNode = argNode.get("predicate");
      UtamMethodAction[] predicate = getPredicateStatements(predicateArrayNode, mapper);
      return new UtamArgument(predicate);
    }
    if (name == null) {
      throw new UtamCompilationError(ERR_ARGS_NAME_MANDATORY);
    }
    if (argNode.has("predicate")) {
      throw new UtamCompilationError(ERR_PREDICATE_REDUNDANT);
    }
    if (!SUPPORTED_TYPES.contains(type)) {
      throw new UtamCompilationError(getUnsupportedTypeErr(type));
    }
    return new UtamArgument(name, type);
  }

  private UtamArgument getLiteralArg(JsonNode valueNode, ObjectMapper mapper) throws IOException {
    Object object;
    if (valueNode.isObject()) {
      if (ElementReference.isLiteralElement(valueNode)) {
        object = mapper.treeToValue(valueNode, ElementReference.class);
      } else if (isLiteralSelector(valueNode)) {
        object = mapper.treeToValue(valueNode, UtamSelector.class);
      } else {
        throw new UtamCompilationError(
            ERR_ARGS_UNKNOWN_LITERAL_OBJECT + valueNode.toPrettyString());
      }
    } else if (valueNode.isTextual()) {
      object = valueNode.asText();
    } else if (valueNode.isBoolean()) {
      object = valueNode.asBoolean();
    } else if (valueNode.isInt()) {
      object = valueNode.asInt();
    } else {
      throw new UtamCompilationError(
          String.format(ERR_ARGS_LITERAL_TYPE_NOT_SUPPORTED, valueNode.toPrettyString()));
    }
    return new UtamArgument(object);
  }

  /**
   * Literal argument type to pass an element reference, format is { "element" : "elementName",
   * "args" : [] }
   *
   * @since 236
   */
  static class ElementReference {

    // name of the element
    private final String elementName;
    // literal arguments. non literal ones are picked up from element context
    private final UtamArgument[] args;

    @JsonCreator
    ElementReference(
        @JsonProperty(value = "element") String elementName,
        @JsonProperty(value = "args") UtamArgument[] args
    ) {
      this.elementName = elementName;
      this.args = args;
    }

    // for tests
    ElementReference(String elementName) {
      this(elementName, null);
    }

    private static boolean isLiteralElement(JsonNode valueNode) {
      return valueNode.has("element");
    }

    /**
     * returns element getter invocation code like this.getMyElement() also returns all its args as
     * they need to be added to the method parameters
     *
     * @param translationContext translation context allows to get element getter name
     * @return literal value as an invocation code
     */
    MethodParameter getElementGetterAsLiteralArg(TranslationContext translationContext) {
      ElementContext elementContext = translationContext.getElement(elementName);
      PageObjectMethod elementGetter = elementContext.getElementMethod();
      List<MethodParameter> getterArgs = elementGetter
          .getDeclaration()
          .getParameters();
      List<MethodParameter> actualParameters = getElementParameters(getterArgs, translationContext);
      String argsString = getParametersValuesString(actualParameters);
      String elementGetterName = elementGetter.getDeclaration().getName();
      translationContext.setMethodUsage(elementGetterName);
      String literalValue = String.format("this.%s(%s)", elementGetterName, argsString);
      return new Literal(literalValue, BASIC_ELEMENT, actualParameters);
    }

    /**
     * if "args" property is set, compare provided parameters with element getter parameter types
     * and return what is in json
     *
     * @param getterArgs         parameters of the element getter
     * @param translationContext context
     * @return list of the parameters set via "args" or original getter list
     */
    private List<MethodParameter> getElementParameters(List<MethodParameter> getterArgs,
        TranslationContext translationContext) {
      if (this.args == null) {
        return getterArgs;
      }
      List<TypeProvider> expectedElementArgs = getterArgs
          .stream()
          .map(MethodParameter::getType)
          .collect(Collectors.toList());
      ArgsProcessor argsProcessor =
          new ArgsProcessorWithExpectedTypes(translationContext,
              String.format("element '%s' reference", elementName), expectedElementArgs);
      return argsProcessor.getParameters(args);
    }
  }
}
