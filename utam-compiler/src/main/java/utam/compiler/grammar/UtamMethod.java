/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.helpers.TypeUtilities.processTypeNode;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.UtamArgument.ArgsProcessor;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.BeforeLoadMethod;
import utam.compiler.representation.ChainMethod;
import utam.compiler.representation.ComposeMethod;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.InterfaceMethod;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

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
  static final String ERR_BEFORE_LOAD_HAS_NO_ARGS = "method beforeLoad cannot have parameters";
  private static final String SUPPORTED_METHOD_TYPES = "\"compose\" or \"chain\"";
  static final String ERR_METHOD_UNKNOWN_TYPE =
      "method '%s': one of " + SUPPORTED_METHOD_TYPES + " should be set";
  static final String ERR_METHOD_REDUNDANT_TYPE =
      "method '%s': only one of " + SUPPORTED_METHOD_TYPES + " can be set";
  final String name;
  private final String comments = "";
  UtamMethodAction[] compose;
  UtamArgument[] args;
  String[] returnType;
  Boolean isReturnList;
  UtamMethodChainLink[] chain;

  @JsonCreator
  UtamMethod(
      @JsonProperty(value = "name", required = true) String name,
      @JsonProperty(value = "compose") UtamMethodAction[] compose,
      @JsonProperty(value = "chain") UtamMethodChainLink[] chain,
      @JsonProperty(value = "args") UtamArgument[] args,
      @JsonProperty(value = "return", defaultValue = "void") JsonNode returnType,
      @JsonProperty(value = "returnAll") Boolean isReturnList) {
    this.name = name;
    this.compose = compose;
    this.args = args;
    this.returnType = processTypeNode(name, TypeUtilities.PropertyType.RETURNS, returnType);
    this.isReturnList = isReturnList;
    this.chain = chain;
  }

  PageObjectMethod getAbstractMethod(TranslationContext context) {
    if (compose != null || chain != null) {
      throw new UtamError(String.format(ERR_METHOD_SHOULD_BE_ABSTRACT, name));
    }
    MethodContext methodContext = new MethodContext(name, getReturnType(context, VOID),
        isReturnsList());
    List<MethodParameter> parameters = new ArgsProcessor(context, methodContext).getParameters(args);
    return new InterfaceMethod(
        methodContext,
        parameters,
        comments, false);
  }

  PageObjectMethod getMethod(TranslationContext context) {
    if (context.isAbstractPageObject()) {
      return getAbstractMethod(context);
    }
    if (compose != null) {
      if (chain != null) {
        throw new UtamError(String.format(ERR_METHOD_REDUNDANT_TYPE, name));
      }
      return getComposeMethod(context);
    }
    if (chain != null) {
      return getChainMethod(context);
    }
    throw new UtamError(String.format(ERR_METHOD_UNKNOWN_TYPE, name));
  }

  private TypeProvider getReturnType(TranslationContext context, TypeProvider defaultReturn) {
    TypeProvider type;
    if (returnType == null || returnType.length == 0) {
      type = defaultReturn;
    } else if (PrimitiveType.isPrimitiveType(returnType[0])) {
      type = PrimitiveType.fromString(returnType[0]);
    } else if (TypeUtilities.Element.isBasicType(returnType)) {
      // If the return type is a basic element, it must be in a public method defined
      // in an interface.
      type = TypeUtilities.Element.asBasicType(name, returnType, false);
    } else {
      type = context.getType(returnType[0]);
    }
    return type;
  }

  PageObjectMethod getChainMethod(TranslationContext context) {
    if (args != null) {
      throw new UtamError(String.format(ERR_ARGS_NOT_ALLOWED, name));
    }
    if (returnType.length != 0) {
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

  private boolean isReturnsList() {
    return Boolean.TRUE.equals(isReturnList);
  }

  PageObjectMethod getComposeMethod(TranslationContext context) {
    MethodContext methodContext = new MethodContext(name, getReturnType(context, null),
        isReturnsList());
    if (args != null) {
      new ArgsProcessor(context, methodContext)
          .getParameters(args)
          .forEach(methodContext::setMethodArg);
    }
    List<ComposeMethodStatement> statements = new ArrayList<>();
    List<MethodParameter> methodParameters = new ArrayList<>();
    setComposeStatements(statements, methodParameters, context, methodContext);
    return new ComposeMethod(
        methodContext,
        statements,
        methodParameters,
        comments
    );
  }

  PageObjectMethod getBeforeLoadMethod(TranslationContext context) {
    MethodContext methodContext = new MethodContext(name, VOID, false);
    List<ComposeMethodStatement> statements = new ArrayList<>();
    List<MethodParameter> methodParameters = new ArrayList<>();
    setComposeStatements(statements, methodParameters, context, methodContext);
    if (!methodParameters.isEmpty()) {
      throw new UtamCompilationError(ERR_BEFORE_LOAD_HAS_NO_ARGS);
    }
    return new BeforeLoadMethod(
        methodContext,
        statements,
        methodParameters,
        comments);
  }

  private void setComposeStatements(List<ComposeMethodStatement> statements,
      List<MethodParameter> methodParameters,
      TranslationContext context, MethodContext methodContext) {
    if (compose.length == 0) {
      throw new UtamError(String.format(ERR_METHOD_EMPTY_STATEMENTS, name));
    }
    for (UtamMethodAction utamMethodAction : compose) {
      ComposeMethodStatement statement = utamMethodAction
          .getComposeAction(context, methodContext, false);
      statements.add(statement);
      methodParameters.addAll(statement.getParameters());
      statement.getParameters().forEach(p -> {
        if(p.getNestedParameters() != null) {
          methodParameters.addAll(p.getNestedParameters());
        }
      });
      methodContext.nextStatement();
    }
    methodParameters.removeIf(MethodParameter::isLiteral);
  }
}
