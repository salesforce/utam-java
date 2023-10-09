package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.HashMap;
import java.util.Map;

public class UtamMetadata {

  private Map<String, Object> metadataProperties;

  UtamMetadata(JsonNode metadataNode) {
    // Turn the metadata properties from a JsonNode to a POJO map
    // with String keys and Object values.
    this.metadataProperties = new ObjectMapper().convertValue(
        metadataNode, new TypeReference<HashMap<String, Object>>() {});
  }

  public boolean hasProperty(String propertyName) {
    return this.metadataProperties.containsKey(propertyName);
  }

  public Object getPropertyValue(String propertyName) {
    if (!hasProperty(propertyName)) {
      return null;
    }

    return this.metadataProperties.get(propertyName);
  }

  static UtamMetadata processMetadataNode(JsonNode node) {
    VALIDATION.validateIsObject(node, "page object root", "property \"metadata\"");
    if (node == null || node.isNull()) {
      return null;
    }
    return new UtamMetadata(node);
  }
}
