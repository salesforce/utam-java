/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.JsonDeserializer.isEmptyNode;
import static utam.compiler.grammar.JsonDeserializer.readNode;
import static utam.compiler.grammar.UtamPageObject.processElementsNode;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import utam.compiler.UtamCompilerIntermediateError;
import utam.compiler.grammar.UtamElement.UtamElementProvider;

/**
 * shadow boundary
 *
 * @author elizaveta.ivanova
 * @since 228
 */
class UtamShadowElement {

  private final JsonNode elementsNode;

  @JsonCreator
  UtamShadowElement(@JsonProperty(value = "elements", required = true) JsonNode elementsNode) {
    this.elementsNode = elementsNode;
  }

  /**
   * process shadow boundary node
   *
   * @param shadowNode    node
   * @param parserContext parser context
   * @return list of elements inside shadow
   */
  static List<UtamElementProvider> processShadowNode(JsonNode shadowNode, String parserContext) {
    List<UtamElementProvider> elements = new ArrayList<>();
    if (isEmptyNode(shadowNode)) {
      return elements;
    }
    Function<Exception, RuntimeException> parserErrorWrapper = causeErr -> new UtamCompilerIntermediateError(
        causeErr, shadowNode, 1100, parserContext, causeErr.getMessage());
    UtamShadowElement shadowElement = readNode(shadowNode, UtamShadowElement.class, parserErrorWrapper);
    return processElementsNode(Objects.requireNonNull(shadowElement).elementsNode, parserContext);
  }
}
