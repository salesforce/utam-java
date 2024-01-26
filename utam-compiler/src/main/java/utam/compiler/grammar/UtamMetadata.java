/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.HashMap;
import java.util.Map;

/**
 * Page Object metadata object.
 *
 * @author james.evans
 * @since 248
 */
class UtamMetadata {

  private final Map<String, Object> metadataProperties;

  /**
   * Initializes a new instance of the UtamMetadata class.
   *
   * @param metadataNode the node in the JSON representing the metadata object
   */
  UtamMetadata(JsonNode metadataNode) {
    // Turn the metadata properties from a JsonNode to a POJO map
    // with String keys and Object values.
    this.metadataProperties =
        new ObjectMapper()
            .convertValue(metadataNode, new TypeReference<HashMap<String, Object>>() {});
  }

  /**
   * Gets the properties and values of the metadata object.
   *
   * @return a map with string keys and object values representing the metadata object properties
   */
  public Map<String, Object> getMetadataProperties() {
    return this.metadataProperties;
  }

  /**
   * Processes a JSON node into a UtamMetadata object.
   *
   * @param node the JSON node to process
   * @return the UtamMetadata object, if any; otherwise null
   */
  static UtamMetadata processMetadataNode(JsonNode node) {
    VALIDATION.validateIsObject(node, "page object root", "property \"metadata\"");
    if (node == null || node.isNull()) {
      return null;
    }
    return new UtamMetadata(node);
  }
}
