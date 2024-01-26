/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.grammar.JsonDeserializer.isEmptyNode;
import static utam.compiler.grammar.JsonDeserializer.readNode;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;

/**
 * shadow boundary
 *
 * @author elizaveta.ivanova
 * @since 228
 */
class UtamShadowElement {

  final List<UtamElement> elements;

  @JsonCreator
  UtamShadowElement(@JsonProperty(value = "elements") List<UtamElement> elements) {
    this.elements = elements;
  }

  /**
   * process shadow boundary node
   *
   * @param shadowNode node
   * @param parserContext parser context
   * @return list of elements inside shadow
   */
  static UtamShadowElement processShadowNode(JsonNode shadowNode, String parserContext) {
    if (isEmptyNode(shadowNode)) {
      return null;
    }
    VALIDATION.validateNotEmptyArray(shadowNode.get("elements"), parserContext, "elements");
    return readNode(
        shadowNode, UtamShadowElement.class, VALIDATION.getErrorMessage(1100, parserContext));
  }
}
