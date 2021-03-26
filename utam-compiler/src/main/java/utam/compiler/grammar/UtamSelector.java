package utam.compiler.grammar;

import utam.core.appium.element.ClassChain;
import utam.core.appium.element.ClassChain.Operator;
import utam.core.appium.element.ClassChain.Quote;
import utam.core.appium.element.UIAutomator;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import utam.compiler.helpers.PrimitiveType;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.Selector;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.grammar.UtamArgument.getArgsProcessor;
import static utam.core.selenium.element.LocatorParameters.SELECTOR_INTEGER_PARAMETER;
import static utam.core.selenium.element.LocatorParameters.SELECTOR_STRING_PARAMETER;

/**
 * selector object mapping
 * class and methods are public to be used in tests
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamSelector {

  static final String ERR_ROOT_SELECTOR_LIST = "root selector can't be list";
  static final String ERR_ROOT_SELECTOR_ARGS = "root selector can't have parameters";
  static final String ERR_SELECTOR_PARAM_UNKNOWN_TYPE =
      "unknown selector parameter type '%s', only string and number are supported";
  private static final String SUPPORTED_SELECTOR_TYPES =
      Stream.of(Selector.Type.values())
          .map(type -> type.name().toLowerCase())
          .collect(Collectors.joining(","));
  private static final String SUPPORTED_UIAUTOMATOR_METHODS =
      Stream.of(UIAutomator.Method.values())
          .map(method -> method.name().toLowerCase())
          .collect(Collectors.joining(","));
  private static final String SUPPORTED_CLASSCHAIN_QUOTES = 
      Stream.of(ClassChain.Quote.values())
          .map(Quote::toString)
          .collect(Collectors.joining(","));
  private static final String SUPPORTED_CLASSCHAIN_OPERATORS = 
      Stream.of(ClassChain.Operator.values())
          .map(Operator::toString)
          .collect(Collectors.joining(","));
  private static final String UIAUTOMATOR_SELECTOR_PREFIX = "new UiSelector()";
  static final String ERR_SELECTOR_MISSING =
      String.format("one of { %s } should be set for selector", SUPPORTED_SELECTOR_TYPES);
  static final String ERR_SELECTOR_REDUNDANT_FORMAT =
      String.format("only one of selector types { %s } can be set", SUPPORTED_SELECTOR_TYPES);
  static final String ERR_SELECTOR_UIAUTOMATOR_WRONG_FORMAT = 
      String.format("should not include {%s} in selector string", UIAUTOMATOR_SELECTOR_PREFIX);
  static final String ERR_SELECTOR_UIAUTOMATOR_UNSUPPORTED_METHOD = 
      String.format("only one of UiSelector methods {%s} can be set", SUPPORTED_UIAUTOMATOR_METHODS);
  static final String ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_QUOTE = 
      String.format("only one of quotes {%s} can be set", SUPPORTED_CLASSCHAIN_QUOTES);
  static final String ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_OPERATOR = 
      String.format("only operator {%s} can be set, and must be leading and ending with space(s)", SUPPORTED_CLASSCHAIN_OPERATORS);

  private final Selector selector;
  final boolean isReturnAll;
  UtamArgument[] args;

  @JsonCreator
  UtamSelector(
      @JsonProperty(value = "css") String css,
      @JsonProperty(value = "accessid") String accessid,
      @JsonProperty(value = "classchain") String classchain,
      @JsonProperty(value = "uiautomator") String uiautomator,
      @JsonProperty(value = "returnAll", defaultValue = "false") boolean isReturnAll,
      @JsonProperty(value = "args") UtamArgument[] args) {
    this.isReturnAll = isReturnAll;
    this.args = args;

    Selector selector;
    if (css != null) {
      selector = Selector.byCss(css);
    } else if (accessid != null) {
      selector = Selector.byAccessibilityId(accessid);
    } else if (classchain != null) {
      validateClassChainSelector(classchain);
      selector = Selector.byClassChain(classchain);
    } else if (uiautomator != null) {
      validateUIAutomatorSelector(uiautomator);
      selector = Selector.byUiAutomator(UIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX + uiautomator);
    } else {
      throw new UtamError(ERR_SELECTOR_MISSING);
    }
    checkRedundantValue(css, accessid, classchain, uiautomator);
    this.selector = selector;
  }

  // used in tests
  UtamSelector(String css, String accessid, String classchain, String uiautomator) {
    this(css, accessid, classchain, uiautomator, false, null);
  }

  // used in tests
  public UtamSelector(String cssSelector) {
    this(cssSelector, null, null, null);
  }

  // used in tests
  public UtamSelector(String cssSelector, boolean isList) {
    this(cssSelector, null, null, null, isList, null);
  }

  // used in tests
  UtamSelector(String cssSelector, UtamArgument[] args) {
    this(cssSelector, null, null, null, false, args);
  }

  private static void checkRedundantValue(String... values) {
    String notNullValue = null;
    for (String value : values) {
      if (value == null) {
        continue;
      }
      if (notNullValue != null) {
        throw new UtamError(ERR_SELECTOR_REDUNDANT_FORMAT);
      }
      notNullValue = value;
    }
  }

  void validateRootSelector() {
    if (isReturnAll) {
      throw new UtamError(ERR_ROOT_SELECTOR_LIST);
    }
    if (args != null || !getParametersTypes().isEmpty()) {
      throw new UtamError(ERR_ROOT_SELECTOR_ARGS);
    }
  }

  void validateClassChainSelector(String classchain) {
    Stream.of(classchain.split("/"))
       .forEach(this::validateSubClassChainSelector);
  }

  void validateSubClassChainSelector(String classchain) {
    // Only do attribute check when using any
    if (classchain.contains("[")) {
      classchain = classchain.substring(classchain.indexOf("[") + 1, classchain.indexOf("]"));
      if (classchain.matches("-?[0-9]*") || classchain.equals("%d")) {
        return;
      }
      validateQuote(classchain);
      validateOperator(classchain);
    }
  }

  void validateQuote(String classchain) {
    if (!classchain.startsWith(ClassChain.Quote.SINGLE_BACKTICK.toString()) 
        && !classchain.startsWith(ClassChain.Quote.SINGLE_DOLLARSIGN.toString())) {
      throw new UtamError(ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_QUOTE);
    }
  }

  void validateOperator(String classchain) {
    String usedOpers = Stream.of(classchain.split(" "))
                           .filter(subString -> (subString.matches("[A-Z]*") || 
                                   subString.equals(ClassChain.Operator.EQUAL.toString())))
                           .collect(Collectors.joining(","));
    if ((Stream.of(usedOpers.split(","))
             .filter(operator -> Arrays.toString(ClassChain.Operator.values()).contains(operator))
             .collect(Collectors.joining(","))).isEmpty()) {
      throw new UtamError(ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_OPERATOR);
    }
  }

  void validateUIAutomatorSelector(String uiautomator) {
    validatePrefix(uiautomator);
    validateMethod(uiautomator);
  }

  void validatePrefix(String uiautomator) {
    if (uiautomator.startsWith(UIAUTOMATOR_SELECTOR_PREFIX)) {
      throw new UtamError(ERR_SELECTOR_UIAUTOMATOR_WRONG_FORMAT);
    }
  }

  void validateMethod(String uiautomator) {
    if(!uiautomator.contains("(")) {
      throw new UtamError(String.format("Invalid UIAutomator selector: '%s'", uiautomator));
    }
    String match = uiautomator.subSequence(0, uiautomator.indexOf("(")).toString();
    if (Stream.of(UIAutomator.Method.values())
             .noneMatch(method -> method.toString().equals(match))){
      throw new UtamError(ERR_SELECTOR_UIAUTOMATOR_UNSUPPORTED_METHOD);
    }
  }

  Selector getSelector() {
    return selector;
  }

  // parse selector string to find parameters %s or %d
  List<TypeProvider> getParametersTypes() {
    List<TypeProvider> res = new ArrayList<>();
    String str = selector.getValue();
    while (str.indexOf("%") > 0) {
      int index = str.indexOf("%");
      if (str.indexOf(SELECTOR_INTEGER_PARAMETER) == index) {
        res.add(PrimitiveType.NUMBER);
        str = str.replaceFirst(SELECTOR_INTEGER_PARAMETER, "");
      } else if (str.indexOf(SELECTOR_STRING_PARAMETER) == index) {
        res.add(PrimitiveType.STRING);
        str = str.replaceFirst(SELECTOR_STRING_PARAMETER, "");
      } else {
        throw new UtamError(
            String.format(ERR_SELECTOR_PARAM_UNKNOWN_TYPE, str.substring(index, index + 2)));
      }
    }
    return res;
  }

  List<MethodParameter> getParameters(String elementName) {
    return getArgsProcessor(args, getParametersTypes(), elementName).getOrdered();
  }
}
