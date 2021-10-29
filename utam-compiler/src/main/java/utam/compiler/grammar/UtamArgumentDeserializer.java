/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.UtamArgument.ELEMENT_REFERENCE_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.FRAME_ELEMENT_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.FUNCTION_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.PAGE_OBJECT_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.ROOT_PAGE_OBJECT_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.SELECTOR_TYPE_PROPERTY;
import static utam.compiler.helpers.TypeUtilities.PARAMETER_REFERENCE;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.IOException;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.UtamArgument.UtamArgumentElementReference;
import utam.compiler.grammar.UtamArgument.UtamArgumentLiteralPrimitive;
import utam.compiler.grammar.UtamArgument.UtamArgumentLiteralSelector;
import utam.compiler.grammar.UtamArgument.UtamArgumentLiteralPageObjectType;
import utam.compiler.grammar.UtamArgument.UtamArgumentNonLiteral;
import utam.compiler.grammar.UtamArgument.UtamArgumentPredicate;
import utam.compiler.helpers.PrimitiveType;
import utam.core.framework.consumer.UtamError;

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

  static final Set<String> SUPPORTED_NON_LITERAL_TYPES = getSupportedJsonTypes();
  private static final String SUPPORTED_TYPES_STRING = String.join(", ", getSupportedJsonTypes());
  private static final String ERR_ARGS_TYPE_NOT_SUPPORTED =
      "args type '%s' is not supported, supported are { " + SUPPORTED_TYPES_STRING + " }";

  private static Set<String> getSupportedJsonTypes() {
    Set<String> supported = Stream.of(PrimitiveType.values())
        .map(PrimitiveType::getJsonTypeName)
        .collect(Collectors.toSet());
    supported.add(SELECTOR_TYPE_PROPERTY);
    supported.add(FUNCTION_TYPE_PROPERTY);
    supported.add(PARAMETER_REFERENCE.getSimpleName());
    supported.add(PAGE_OBJECT_TYPE_PROPERTY);
    supported.add(FRAME_ELEMENT_TYPE_PROPERTY);
    supported.add(ROOT_PAGE_OBJECT_TYPE_PROPERTY);
    return supported;
  }

  static String getUnsupportedTypeErr(String type) {
    return String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, type);
  }

  static String getErrorMessage(JsonNode node, String text) {
    return "\n" + node.toPrettyString() + ": " + text;
  }

  @Override
  public UtamArgument deserialize(JsonParser parser, DeserializationContext ctxt) throws IOException {
    ObjectMapper mapper = (ObjectMapper) parser.getCodec();
    ObjectNode rootArgNode = mapper.readTree(parser);
    String typeStr = rootArgNode.has("type")? rootArgNode.get("type").asText() : null;
    boolean isLiteral = rootArgNode.has("value");
    try {
      if (FUNCTION_TYPE_PROPERTY.equals(typeStr)) {
        return mapper.treeToValue(rootArgNode, UtamArgumentPredicate.class);
      }
      if (ELEMENT_REFERENCE_TYPE_PROPERTY.equals(typeStr)) {
        return mapper.treeToValue(rootArgNode, UtamArgumentElementReference.class);
      }
      if (isLiteral) {
        if (PAGE_OBJECT_TYPE_PROPERTY.equals(typeStr) || ROOT_PAGE_OBJECT_TYPE_PROPERTY.equals(typeStr)) {
          return mapper.treeToValue(rootArgNode, UtamArgumentLiteralPageObjectType.class);
        }
        if (SELECTOR_TYPE_PROPERTY.equals(typeStr)) {
          return mapper.treeToValue(rootArgNode, UtamArgumentLiteralSelector.class);
        }
        return mapper.treeToValue(rootArgNode, UtamArgumentLiteralPrimitive.class);
      }
      return mapper.treeToValue(rootArgNode, UtamArgumentNonLiteral.class);
    } catch (IOException | UtamError e) {
      throw new UtamCompilationError(getErrorMessage(rootArgNode, e.getMessage()), e);
    }
  }
}
