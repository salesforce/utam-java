/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.UtamMethodActionReturnSelf.RETURN_SELF;
import static utam.compiler.grammar.UtamMethodActionWaitFor.WAIT_FOR;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.IOException;
import utam.compiler.UtamCompilationError;

/**
 * depending on compose statement type, deserialize into different objects
 *
 * @author elizaveta.ivanova
 * @since 236
 */
class UtamMethodActionDeserializer extends
    com.fasterxml.jackson.databind.JsonDeserializer<UtamMethodAction> {

  static final String ERR_REQUIRED_KEYS =
      "should either have 'element', 'apply' or 'applyExternal' properties";
  static final String ERR_REDUNDANT_KEYS =
      "should not have both 'apply' and 'applyExternal' properties";
  static final String ERR_REDUNDANT_ELEMENT =
      "should not have 'element' property";

  @Override
  public UtamMethodAction deserialize(JsonParser parser, DeserializationContext ctxt)
      throws IOException {
    ObjectMapper mapper = (ObjectMapper) parser.getCodec();
    ObjectNode objectNode = mapper.readTree(parser);
    String statementString = String.format("statement '%s' ", objectNode.toPrettyString());

    JsonNode applyNode = objectNode.get("apply");
    JsonNode applyExternalNode = objectNode.get("applyExternal");
    JsonNode elementNode = objectNode.get("element");

    String apply = applyNode != null? applyNode.asText() : null;
    String applyExternal = applyExternalNode != null? applyExternalNode.asText() : null;
    String elementName = elementNode != null? elementNode.asText() : null;

    if (apply == null && applyExternal == null) {
      if(elementName == null) {
        throw new UtamCompilationError(statementString + ERR_REQUIRED_KEYS);
      }
      return mapper.treeToValue(objectNode, UtamMethodActionGetter.class);
    }

    if (apply != null && applyExternal != null) {
      throw new UtamCompilationError(statementString + ERR_REDUNDANT_KEYS);
    }

    if (applyExternal != null) {
      if(elementName != null) {
        throw new UtamCompilationError(statementString + ERR_REDUNDANT_ELEMENT);
      }
      return mapper.treeToValue(objectNode, UtamMethodActionUtility.class);
    }
    if(WAIT_FOR.equals(apply)) {
      return mapper.treeToValue(objectNode, UtamMethodActionWaitFor.class);
    }
    if(RETURN_SELF.equals(apply)) {
      return mapper.treeToValue(objectNode, UtamMethodActionReturnSelf.class);
    }
    return mapper.treeToValue(objectNode, UtamMethodActionApply.class);
  }
}
