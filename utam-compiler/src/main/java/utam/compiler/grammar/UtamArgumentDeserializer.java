package utam.compiler.grammar;

import static utam.compiler.grammar.UtamArgument.ERR_ARGS_TYPE_NOT_SUPPORTED;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.IOException;
import utam.core.framework.consumer.UtamError;

/**
 * custom deserializer for UtamArgument to read value as UtamSelector
 *
 * @author elizaveta.ivanova
 * @since 232
 */
class UtamArgumentDeserializer extends
    com.fasterxml.jackson.databind.JsonDeserializer<UtamArgument> {

  @Override
  public UtamArgument deserialize(JsonParser parser, DeserializationContext ctxt)
      throws IOException {
    UtamArgument res = new UtamArgument(null);
    ObjectMapper mapper = (ObjectMapper) parser.getCodec();
    ObjectNode root = mapper.readTree(parser);
    JsonNode valueNode = root.get("value");
    if (valueNode != null) {
      if (valueNode.isObject()) {
        res.value = mapper.treeToValue(valueNode, UtamSelector.class);
      } else if (valueNode.isTextual()) {
        res.value = valueNode.asText();
      } else if (valueNode.isBoolean()) {
        res.value = valueNode.asBoolean();
      } else if (valueNode.isNumber()) {
        res.value = valueNode.asInt();
      } else {
        throw new UtamError(
            String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, "args", valueNode.toPrettyString()));
      }
    }
    if (root.has("name")) {
      res.name = root.get("name").asText();
    }
    if (root.has("type")) {
      res.type = root.get("type").asText();
    }
    JsonNode predicateArrayNode = root.get("predicate");
    if (predicateArrayNode != null) {
      res.conditions = new UtamMethodAction[predicateArrayNode.size()];
      for (int i = 0; i < predicateArrayNode.size(); i++) {
        res.conditions[i] = mapper.treeToValue(predicateArrayNode.get(i), UtamMethodAction.class);
      }
    }
    return res;
  }
}
