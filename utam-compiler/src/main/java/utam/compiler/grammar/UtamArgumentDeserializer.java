/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.UtamArgument.ELEMENT_REFERENCE_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.ELEMENT_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.FRAME_ELEMENT_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.FUNCTION_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.PAGE_OBJECT_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.SELECTOR_TYPE_PROPERTY;
import static utam.compiler.helpers.LocatorCodeGeneration.SUPPORTED_SELECTOR_TYPES;
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
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.UtamArgument.UtamArgumentElementReference;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
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
    supported.add(ELEMENT_REFERENCE_TYPE_PROPERTY);
    supported.add(ELEMENT_TYPE_PROPERTY);
    supported.add(PAGE_OBJECT_TYPE_PROPERTY);
    supported.add(FRAME_ELEMENT_TYPE_PROPERTY);
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
    if (!rootArgNode.has("type")) {
      throw new UtamCompilationError(ERR_ARGS_TYPE_MANDATORY);
    }
    String type = rootArgNode.get("type").asText();
    if(ELEMENT_REFERENCE_TYPE_PROPERTY.equals(type)) {
      return getElementReference(rootArgNode, mapper);
    }
    if(FUNCTION_TYPE_PROPERTY.equals(type)) {
      return getPredicate(rootArgNode, mapper);
    }
    return getNonLiteralArg(rootArgNode, type);
  }

  private UtamArgument getNonLiteralArg(ObjectNode argNode, String type) {
    if (!argNode.has("name")) {
      throw new UtamCompilationError(ERR_ARGS_NAME_MANDATORY);
    }
    String name = argNode.get("name").asText();
    if (argNode.has("predicate")) {
      throw new UtamCompilationError(ERR_PREDICATE_REDUNDANT);
    }
    if (!SUPPORTED_TYPES.contains(type)) {
      throw new UtamCompilationError(getUnsupportedTypeErr(type));
    }
    return new UtamArgument.UtamArgumentNonLiteral(name, type);
  }

  private UtamArgument getLiteralArg(JsonNode valueNode, ObjectMapper mapper) throws IOException {
    Object object;
    if (valueNode.isObject()) {
      if (isLiteralSelector(valueNode)) {
        object = mapper.treeToValue(valueNode, UtamSelector.class);
      } else if (PageObjectType.isLiteralPageObject(valueNode)) {
        object = mapper.treeToValue(valueNode, PageObjectType.class);
      }
      else {
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
    return new UtamArgument.UtamArgumentLiteral(object);
  }

  private UtamArgument getPredicate(ObjectNode argNode, ObjectMapper mapper) {
    if (!argNode.has("predicate")) {
      throw new UtamCompilationError(ERR_PREDICATE_MANDATORY);
    }
    JsonNode predicateArrayNode = argNode.get("predicate");
    UtamMethodAction[] predicate = StreamSupport.stream(predicateArrayNode.spliterator(), false)
        .map(predicateStatement -> {
          try {
            return mapper.treeToValue(predicateStatement, UtamMethodAction.class);
          } catch (JsonProcessingException e) {
            throw new UtamCompilationError(e);
          }
        })
        .toArray(UtamMethodAction[]::new);
    return new UtamArgument.UtamArgumentPredicate(predicate);
  }

  private UtamArgument getElementReference(ObjectNode argNode, ObjectMapper mapper) {
    if (!argNode.has("name")) {
      throw new UtamCompilationError(ERR_ARGS_NAME_MANDATORY);
    }
    String name = argNode.get("name").asText();
    if (argNode.has("predicate")) {
      throw new UtamCompilationError(ERR_PREDICATE_REDUNDANT);
    }
    UtamArgument[] nestedArgs =
        argNode.has("args") ? StreamSupport.stream(argNode.get("args").spliterator(), false)
            .map(arg -> {
              try {
                // only literal can be inside!
                return mapper.treeToValue(arg, UtamArgument.UtamArgumentLiteral.class);
              } catch (JsonProcessingException e) {
                throw new UtamCompilationError(e);
              }
            })
            .toArray(UtamArgument[]::new) : null;
    return new UtamArgumentElementReference(name, nestedArgs);
  }

  /**
   * Literal argument type to pass a hardcoded Page Object type, for example when composing container invocation,
   * format: { "type": "my/object/type" }
   * @since 236
   */
  static class PageObjectType {

    private final String pageObjectType;

    @JsonCreator
    PageObjectType(@JsonProperty(value = "type") String pageObjectType) {
      this.pageObjectType = pageObjectType;
    }

    private static boolean isLiteralPageObject(JsonNode valueNode) {
      return valueNode.has("type");
    }

    TypeProvider getLiteralType(TranslationContext translationContext) {
      return translationContext.getType(pageObjectType);
    }
  }
}
