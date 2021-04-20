/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.TypeUtilities.VOID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.*;

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

import static utam.compiler.grammar.UtamPageObject.BEFORELOAD_METHOD_MANE;

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
  private static final String SUPPORTED_METHOD_TYPES = "\"compose\" or \"chain\"";
  static final String ERR_METHOD_UNKNOWN_TYPE =
      "method '%s': one of " + SUPPORTED_METHOD_TYPES + " should be set";
  static final String ERR_METHOD_REDUNDANT_TYPE =
      "method '%s': only one of " + SUPPORTED_METHOD_TYPES + " can be set";
  static final String ERR_DUPLICATED_STATEMENT = "beforeLoad: duplicate declaration { element: %s, apply: %s } - already exists";
  static final String ERR_BEFORELOAD_NAME_NOT_ALLOWED =
          "method name \"load\" is reserved for 'beforeload' property, please use other name";
  final String name;
  private final String comments = "";
  UtamMethodAction[] compose;
  UtamArgument[] args;
  String returnStr;
  Boolean isReturnList;
  UtamMethodChainLink[] chain;

  @JsonCreator
  UtamMethod(
      @JsonProperty(value = "name", required = true) String name,
      @JsonProperty(value = "compose") UtamMethodAction[] compose,
      @JsonProperty(value = "chain") UtamMethodChainLink[] chain,
      @JsonProperty(value = "args") UtamArgument[] args,
      @JsonProperty(value = "return", defaultValue = "void") String returnStr,
      @JsonProperty(value = "returnAll") Boolean isReturnList) {
    this.name = name;
    this.compose = compose;
    this.args = args;
    this.returnStr = returnStr;
    this.isReturnList = isReturnList;
    this.chain = chain;
  }

  // used in tests - shortcut for compose
  UtamMethod(String name, UtamMethodAction[] compose) {
    this(name, compose, null, null, null, null);
  }

  // used in tests - shortcut for abstract
  UtamMethod(String name, String returns, UtamArgument[] args) {
    this(name, null, null, args, returns, null);
  }

  // used in tests - shortcut for chain
  UtamMethod(String name, String returns, UtamMethodChainLink[] chain) {
    this(name, null, chain, null, returns, null);
  }

  PageObjectMethod getAbstractMethod(TranslationContext context) {
    if (compose != null || chain != null) {
      throw new UtamError(String.format(ERR_METHOD_SHOULD_BE_ABSTRACT, name));
    }
    MethodContext methodContext = new MethodContext(name, getReturnType(context, VOID), isReturnsList());
    return new InterfaceMethod(
        methodContext,
        UtamArgument.getArgsProcessor(args, name).getOrdered(),
        comments);
  }

  PageObjectMethod getMethod(TranslationContext context) {
    if (!context.isBeforeLoad() && name.equals(BEFORELOAD_METHOD_MANE)) {
      throw new UtamError(ERR_BEFORELOAD_NAME_NOT_ALLOWED);
    }
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
    if (returnStr == null) {
      type = defaultReturn;
    } else if (PrimitiveType.isPrimitiveType(returnStr)) {
      type = PrimitiveType.fromString(returnStr);
    } else if (TypeUtilities.Element.isBasicType(returnStr)) {
      type = TypeUtilities.Element.asBasicType(returnStr);
    } else {
      type = context.getType(returnStr);
    }
    return type;
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

  private boolean isReturnsList() {
    return Boolean.TRUE.equals(isReturnList);
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
    List<ComposeMethodStatement> statements = new ArrayList<>();
    List<MethodParameter> methodParameters = new ArrayList<>();
    MethodContext methodContext = new MethodContext(name, getReturnType(context, null), isReturnsList());
    for (UtamMethodAction utamMethodAction : compose) {
      ComposeMethodStatement statement = utamMethodAction
          .getComposeAction(context, methodContext, false);
      statements.add(statement);
      methodParameters.addAll(statement.getParameters());
    }
    methodParameters.removeIf(MethodParameter::isLiteral);
    return new ComposeMethod(
        methodContext,
        statements,
        methodParameters,
        comments);
  }

  PageObjectMethod getBeforeLoadMethod(TranslationContext context) {
    if (args != null) {
      throw new UtamError(String.format(ERR_ARGS_NOT_ALLOWED, name));
    }
    if (compose.length == 0) {
      throw new UtamError(String.format(ERR_METHOD_EMPTY_STATEMENTS, name));
    }
    List<ComposeMethodStatement> statements = new ArrayList<>();
    List<MethodParameter> methodParameters = new ArrayList<>();
    Map<String ,String> beforeLoadMethodActions = new HashMap<>();

    MethodContext methodContext = new MethodContext(name, VOID, false);
    for (UtamMethodAction utamMethodAction : compose) {
      // Check if a statement is already added, to avoid duplicates
      if (beforeLoadMethodActions.containsKey(utamMethodAction.elementName) &&
              beforeLoadMethodActions.get(utamMethodAction.elementName).equals(utamMethodAction.apply)) {
        throw new UtamError(String.
                format(ERR_DUPLICATED_STATEMENT, utamMethodAction.elementName, utamMethodAction.apply));
      } else {
        beforeLoadMethodActions.put(utamMethodAction.elementName, utamMethodAction.apply);
      }
      ComposeMethodStatement statement = utamMethodAction
              .getComposeAction(context, methodContext, false);
      statements.add(statement);
      methodParameters.addAll(statement.getParameters());
    }
    methodParameters.removeIf(MethodParameter::isLiteral);
    return new BeforeLoadMethod(
            methodContext,
            statements,
            methodParameters,
            comments);
  }
}
