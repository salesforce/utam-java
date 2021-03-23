package utam.compiler.grammar;

import static utam.compiler.helpers.TypeUtilities.VOID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import utam.compiler.grammar.UtamMethodAction.MethodContext;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.ComposeMethodStatement;
import utam.core.framework.consumer.UtamError;
import utam.compiler.representation.ChainMethod;
import utam.compiler.representation.ComposeMethod;
import utam.compiler.representation.InterfaceMethod;
import utam.compiler.representation.UtilityMethod;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * public method declared at PO level
 *
 * @author elizaveta.ivanova
 * @since 228
 */
class UtamMethod {

  static final String ERR_ARGS_NOT_ALLOWED = "method '%s': args not supported";
  static final String ERR_METHOD_EMPTY_STATEMENTS = "method '%s' has no statements";
  static final String ERR_METHOD_SHOULD_BE_ABSTRACT = "method '%s' is abstract";
  static final String ERR_METHOD_RETURN_TYPE_REDUNDANT =
      "method '%s': 'return' property is redundant";
  static final String ERR_METHOD_RETURN_ALL_REDUNDANT =
      "method '%s': 'returnAll' property is redundant";
  private static final String SUPPORTED_METHOD_TYPES = "\"compose\", \"chain\", or \"external\"";
  static final String ERR_METHOD_UNKNOWN_TYPE =
      "method '%s': one of " + SUPPORTED_METHOD_TYPES + " should be set";
  static final String ERR_METHOD_REDUNDANT_TYPE =
      "method '%s': only one of " + SUPPORTED_METHOD_TYPES + " can be set";
  final String name;
  UtamMethodAction[] compose;
  private final String comments = "";
  UtamArgument[] args;
  String returnStr;
  Boolean isReturnList;
  UtamMethodChainLink[] chain;
  UtamMethodUtil externalUtility;

  @JsonCreator
  UtamMethod(
      @JsonProperty(value = "name", required = true) String name,
      @JsonProperty(value = "compose") UtamMethodAction[] compose,
      @JsonProperty(value = "chain") UtamMethodChainLink[] chain,
      @JsonProperty(value = "external") UtamMethodUtil externalUtility,
      @JsonProperty(value = "args") UtamArgument[] args,
      @JsonProperty(value = "return", defaultValue = "void") String returnStr,
      @JsonProperty(value = "returnAll") Boolean isReturnList) {
    this.name = name;
    this.compose = compose;
    this.args = args;
    this.returnStr = returnStr;
    this.isReturnList = isReturnList;
    this.chain = chain;
    this.externalUtility = externalUtility;
  }

  // used in tests - shortcut for utils
  UtamMethod(String name, String returns, UtamMethodUtil externalUtility, Boolean returnAll) {
    this(name, null, null, externalUtility, null, returns, returnAll);
  }

  // used in tests - shortcut for compose
  UtamMethod(String name, UtamMethodAction[] compose) {
    this(name, compose, null, null, null, null, null);
  }

  // used in tests - shortcut for abstract
  UtamMethod(String name, String returns, UtamArgument[] args) {
    this(name, null, null, null, args, returns, null);
  }

  // used in tests - shortcut for chain
  UtamMethod(String name, String returns, UtamMethodChainLink[] chain) {
    this(name, null, chain, null, null, returns, null);
  }

  PageObjectMethod getAbstractMethod(TranslationContext context) {
    if (compose != null || chain != null || externalUtility != null) {
      throw new UtamError(String.format(ERR_METHOD_SHOULD_BE_ABSTRACT, name));
    }
    return new InterfaceMethod(
        name,
        getReturnType(context),
        Boolean.TRUE.equals(isReturnList),
        UtamArgument.getArgsProcessor(args, name).getOrdered(),
        comments);
  }

  PageObjectMethod getMethod(TranslationContext context) {
    if(context.isAbstractPageObject()) {
      return getAbstractMethod(context);
    }
    if (compose != null) {
      if(chain != null || externalUtility != null) {
        throw new UtamError(String.format(ERR_METHOD_REDUNDANT_TYPE, name));
      }
      return getComposeMethod(context);
    }
    if (chain != null) {
      if(compose != null || externalUtility != null) {
        throw new UtamError(String.format(ERR_METHOD_REDUNDANT_TYPE, name));
      }
      return getChainMethod(context);
    }
    if (externalUtility != null) {
      // We already know chain and compose are null if we've gotten here
      return getUtilityMethod(context);
    }
    throw new UtamError(String.format(ERR_METHOD_UNKNOWN_TYPE, name));
  }

  private TypeProvider getReturnType(TranslationContext context) {
    if (returnStr == null) {
      return VOID;
    }
    if (PrimitiveType.isPrimitiveType(returnStr)) {
      return PrimitiveType.fromString(returnStr);
    }
    if(TypeUtilities.Element.isBasicType(returnStr)) {
      return TypeUtilities.Element.asBasicType(returnStr);
    }
    return context.getType(returnStr);
  }

  private PageObjectMethod getUtilityMethod(TranslationContext context) {
    return new UtilityMethod(
        name,
        getReturnType(context),
        isReturnList == null ? false : isReturnList,
        externalUtility.getMethodReference(name, context),
        comments);
  }

  PageObjectMethod getChainMethod(TranslationContext context) {
    if (args != null) {
      throw new UtamError(String.format(ERR_ARGS_NOT_ALLOWED, name));
    }
    if (returnStr != null) {
      throw new UtamError(String.format(ERR_METHOD_RETURN_TYPE_REDUNDANT, name));
    }
    if (isReturnList != null) {
      throw new UtamError(String.format(ERR_METHOD_RETURN_ALL_REDUNDANT, name));
    }
    if (chain.length == 0) {
      throw new UtamError(String.format(ERR_METHOD_EMPTY_STATEMENTS, name));
    }
    List<ChainMethod.Link> statements = new ArrayList<>();
    for (int i = 0; i < chain.length; i++) {
      // first element is from same PO and should be gotten from context
      ElementContext firstElement = i == 0 ? context.getElement(chain[0].elementName) : null;
      statements.add(chain[i].getChainStatement(context, firstElement));
    }
    return new ChainMethod(name, statements, comments);
  }

  PageObjectMethod getComposeMethod(TranslationContext context) {
    if (args != null) {
      throw new UtamError(String.format(ERR_ARGS_NOT_ALLOWED, name));
    }
    // List<Void> should throw error
    if (returnStr == null && isReturnList != null) {
      throw new UtamError(String.format(ERR_METHOD_RETURN_ALL_REDUNDANT, name));
    }
    if (compose.length == 0) {
      throw new UtamError(String.format(ERR_METHOD_EMPTY_STATEMENTS, name));
    }
    TypeProvider returnType = getReturnType(context);
    List<ComposeMethodStatement> statements = new ArrayList<>();
    List<MethodParameter> methodParameters = new ArrayList<>();
    UtamMethodAction.MethodContext methodContext = new MethodContext(name, returnType);
    for (int i = 0; i < compose.length; i ++) {
      ComposeMethodStatement statement = compose[i].getComposeAction(context, methodContext, i == compose.length -1);
      statements.add(statement);
      methodParameters.addAll(statement.getParameters());
    }
    methodParameters.removeIf(p -> p.isLiteral());
    return new ComposeMethod(
        name,
        statements,
        methodParameters.stream()
            .filter(methodParameter -> !methodParameter.isLiteral())
            .collect(Collectors.toList()),
        comments);
  }
}
