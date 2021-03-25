package utam.compiler.grammar;

import static utam.compiler.grammar.UtamArgument.ERR_ARGS_TYPE_NOT_SUPPORTED;
import static utam.compiler.grammar.UtamArgument.ERR_NAME_TYPE_REDUNDANT;
import static utam.compiler.grammar.UtamArgument.FUNCTION_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.SELECTOR_TYPE_PROPERTY;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.IOException;
import java.util.stream.Stream;
import utam.compiler.helpers.PrimitiveType;
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
      throws IOException, JsonProcessingException {
    UtamArgument res = new UtamArgument(null);
    ObjectMapper mapper = (ObjectMapper) parser.getCodec();
    ObjectNode root = mapper.readTree(parser);
    /*try {
      root = mapper.readTree(parser);
    } catch (JsonProcessingException e) {
      throw new UtamError("?", e);
    }*/
    JsonNode valueNode = root.get("value");
    final String validationContext = "args";
    if (valueNode != null) {
      if (valueNode.isObject()) {
        res.value = mapper.treeToValue(valueNode, UtamSelector.class);
      } else if (valueNode.isTextual()) {
        res.value = valueNode.asText();
      } else if (valueNode.isBoolean()) {
        res.value = valueNode.asBoolean();
      } else if (valueNode.isInt()) {
        res.value = valueNode.asInt();
      } else {
        throw new UtamError(
            String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, validationContext,
                valueNode.toPrettyString()));
      }
    }
    if (root.has("name")) {
      if (valueNode != null) {
        throw new UtamError(String.format(ERR_NAME_TYPE_REDUNDANT, validationContext));
      }
      res.name = root.get("name").asText();
    }
    if (root.has("type")) {
      if (valueNode != null) {
        throw new UtamError(String.format(ERR_NAME_TYPE_REDUNDANT, validationContext));
      }
      res.type = root.get("type").asText();
      Stream.concat(Stream.of(SELECTOR_TYPE_PROPERTY, FUNCTION_TYPE_PROPERTY),
          Stream.of(PrimitiveType.STRING, PrimitiveType.BOOLEAN, PrimitiveType.NUMBER)
              .map(PrimitiveType::getJsonTypeName))
          .filter(s -> res.type.equals(s))
          .findAny()
          .orElseThrow(
              () -> new UtamError(
                  String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, validationContext, res.type)));
    }
    JsonNode predicateArrayNode = root.get("predicate");
    if (predicateArrayNode != null) {
      if (valueNode != null) {
        throw new UtamError(String.format(ERR_NAME_TYPE_REDUNDANT, validationContext));
      }
      res.conditions = new UtamMethodAction[predicateArrayNode.size()];
      for (int i = 0; i < predicateArrayNode.size(); i++) {
        res.conditions[i] = mapper.treeToValue(predicateArrayNode.get(i), UtamMethodAction.class);
      }
    }
    return res;
  }
}
