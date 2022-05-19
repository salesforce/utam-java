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
import static utam.compiler.grammar.UtamPageObject.INTERFACE_PROPERTIES;
import static utam.compiler.grammar.UtamPageObject.processElementsNode;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import utam.compiler.UtamCompilationError;
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
   * @param isAbstract boolean to indicate if page object is an interface
   * @param shadowNode    node
   * @param parserContext parser context
   * @return list of elements inside shadow
   */
  static List<UtamElementProvider> processShadowNode(boolean isAbstract, JsonNode shadowNode, String parserContext) {
    if(isAbstract && !isEmptyNode(shadowNode)) {
      throw new UtamCompilerIntermediateError(904, "shadow", INTERFACE_PROPERTIES);
    }
    List<UtamElementProvider> elements = new ArrayList<>();
    if (isEmptyNode(shadowNode)) {
      return elements;
    }
    Function<Exception, RuntimeException> parserErrorWrapper = causeErr -> new UtamCompilerIntermediateError(
        causeErr, shadowNode, 1100, parserContext, causeErr.getMessage());
    UtamShadowElement shadowElement = readNode(shadowNode, UtamShadowElement.class, parserErrorWrapper);
    return processElementsNode(false, Objects.requireNonNull(shadowElement).elementsNode, parserContext);
  }
}
