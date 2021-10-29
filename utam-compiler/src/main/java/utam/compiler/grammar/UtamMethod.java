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
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.ReturnType;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.TranslationContext;
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

  static final String ERR_METHOD_EMPTY_STATEMENTS = "method '%s' has no statements";
  static final String ERR_METHOD_SHOULD_BE_ABSTRACT = "method '%s' is abstract and cannot have statements";
  static final String ERR_BEFORE_LOAD_HAS_NO_ARGS = "method beforeLoad cannot have parameters";
  static final String ERR_RETURN_TYPE_ABSTRACT_ONLY = "method '%s': return type should be set inside last statement instead method level";
  private final String name;
  private final UtamMethodAction[] compose;
  private final UtamArgument[] args;
  private final JsonNode returnType;
  private final Boolean isReturnList;

  @JsonCreator
  UtamMethod(
      @JsonProperty(value = "name", required = true) String name,
      @JsonProperty(value = "compose") UtamMethodAction[] compose,
      @JsonProperty(value = "args") UtamArgument[] args,
      @JsonProperty(value = "returnType") JsonNode returnType,
      @JsonProperty(value = "returnAll") Boolean isReturnList) {
    this.name = name;
    this.compose = compose;
    this.args = args;
    this.returnType = returnType;
    this.isReturnList = isReturnList;
  }

  PageObjectMethod getMethod(TranslationContext context) {
    if (context.isAbstractPageObject()) {
      return getAbstractMethod(context);
    }
    if (compose != null && compose.length > 0) {
      return getComposeMethod(context);
    }
    throw new UtamError(String.format(ERR_METHOD_EMPTY_STATEMENTS, name));
  }

  PageObjectMethod getAbstractMethod(TranslationContext context) {
    if (compose != null) {
      throw new UtamError(String.format(ERR_METHOD_SHOULD_BE_ABSTRACT, name));
    }
    ReturnType returnTypeObject = new ReturnType.AbstractMethodReturnType(returnType, isReturnList, name);
    TypeProvider returnType = returnTypeObject.getReturnTypeOrDefault(context, VOID);
    MethodContext methodContext = new MethodContext(name, returnTypeObject);
    List<MethodParameter> parameters = new ArgsProcessor(context, methodContext).getParameters(args);
    return new InterfaceMethod(
        name,
        returnType,
        parameters);
  }

  private PageObjectMethod getComposeMethod(TranslationContext context) {
    ReturnType returnTypeObject = new ReturnType(returnType, isReturnList, name);
    if(returnTypeObject.isReturnTypeSet()) {
      throw new UtamCompilationError(String.format(ERR_RETURN_TYPE_ABSTRACT_ONLY, name));
    }
    MethodContext methodContext = new MethodContext(name, returnTypeObject);
    if (args != null) {
      new ArgsProcessor(context, methodContext)
          .getParameters(args)
          .forEach(methodContext::setDeclaredParameter);
    }
    List<ComposeMethodStatement> statements = getComposeStatements(context, methodContext, compose);
    ComposeMethodStatement lastStatement = statements.get(statements.size()-1);
    TypeProvider lastStatementReturnType = lastStatement.getReturnType();
    methodContext.checkAllParametersWereUsed();
    return new ComposeMethod(
        name,
        lastStatementReturnType,
        methodContext.getMethodParameters(),
        statements
    );
  }

  static List<ComposeMethodStatement> getComposeStatements(
      TranslationContext context,
      MethodContext methodContext,
      UtamMethodAction[] compose) {
    List<ComposeMethodStatement> statements = new ArrayList<>();
    String name = methodContext.getName();
    TypeProvider previousStatementReturn = null;
    for (int i = 0; i < compose.length; i ++) {
      UtamMethodAction statementDeclaration = compose[i];
      StatementContext statementContext = new StatementContext(
          previousStatementReturn,
          i,
          isUsedAsChain(compose, i),
          statementDeclaration.getStatementType(i, compose.length),
          statementDeclaration.getDeclaredReturnType(name));
      ComposeMethodStatement statement = statementDeclaration.getComposeAction(context, methodContext, statementContext);
      previousStatementReturn = statement.getReturnType();
      statements.add(statement);
    }
    return statements;
  }

  static boolean isUsedAsChain(UtamMethodAction[] compose, int index) {
    if(index == compose.length -1) {
      return false;
    }
    return compose[index + 1].isChain;
  }
}
