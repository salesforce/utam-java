package declarative.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import declarative.helpers.ElementContext;
import declarative.helpers.TranslationContext;
import declarative.representation.ChainMethod;
import framework.consumer.UtamError;

/**
 * chain method statement
 *
 * @author elizaveta.ivanova
 * @since 228
 */
class UtamMethodChainLink {

  static final String ERR_WRONG_RETURN_TYPE_FOR_FIRST_LINK =
      "First chain statement for element '%s' should return type '%s', currently returns '%s'";

  static final String ERR_WRONG_CARDINALITY_FOR_FIRST_LINK =
      "First chain statement for element '%s' should %sbe list";

  final String elementName;
  final boolean isReturnList;
  final String type;

  @JsonCreator
  UtamMethodChainLink(
      @JsonProperty(value = "element", required = true) String elementName,
      @JsonProperty(value = "returnAll") boolean isReturnList,
      @JsonProperty(value = "type", required = true) String type) {
    this.elementName = elementName;
    this.isReturnList = isReturnList;
    this.type = type;
  }

  ChainMethod.Link getChainStatement(TranslationContext context, ElementContext firstElement) {
    if (firstElement != null) {
      if (!context.getType(type).equals(firstElement.getType())) {
        throw new UtamError(
            String.format(
                ERR_WRONG_RETURN_TYPE_FOR_FIRST_LINK,
                firstElement.getName(),
                firstElement.getType().getFullName(),
                context.getType(type).getFullName()));
      }
      if (firstElement.isList() != isReturnList) {
        throw new UtamError(
            String.format(
                ERR_WRONG_CARDINALITY_FOR_FIRST_LINK,
                firstElement.getName(),
                (firstElement.isList() ? "" : "not ")));
      }
      return new ChainMethod.Link(firstElement);
    } else {
      return new ChainMethod.Link(context.getType(type), elementName, isReturnList);
    }
  }
}
