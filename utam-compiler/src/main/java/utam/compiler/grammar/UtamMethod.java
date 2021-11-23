/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.types.BasicElementInterface.isReturnBasicType;
import static utam.compiler.types.BasicElementInterface.processBasicTypeNode;
import static utam.compiler.types.BasicElementUnionType.asBasicOrUnionType;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ReturnType;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethod;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.InterfaceMethod;
import utam.compiler.representation.InterfaceMethod.AbstractBasicElementGetter;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

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

  final PageObjectMethod getMethod(TranslationContext context) {
    return context.isAbstractPageObject()? getAbstractMethod(context) : getComposeMethod(context);
  }


  private PageObjectMethod getAbstractMethod(TranslationContext context) {
    if (compose != null) {
      throw new UtamCompilationError(String.format(ERR_METHOD_SHOULD_BE_ABSTRACT, name));
    }
    boolean isReturnsBasicType = isReturnBasicType(returnType);
    final ReturnType returnTypeObject;
    if (isReturnsBasicType) {
      String[] basicUnionType = processBasicTypeNode(returnType, name, true);
      TypeProvider unionReturnType = asBasicOrUnionType(name, basicUnionType, false);
      returnTypeObject = new ReturnType(unionReturnType, isReturnList, name);
    } else {
      returnTypeObject = new ReturnType(returnType, isReturnList, name);
    }
    TypeProvider methodReturnType = returnTypeObject.getReturnTypeOrDefault(context, VOID);
    MethodContext methodContext = new MethodContext(name, returnTypeObject);
    List<MethodParameter> parameters = new ArgsProcessor(context, methodContext).getParameters(args);
    return isReturnsBasicType ? new AbstractBasicElementGetter(name, parameters, methodReturnType)
        : new InterfaceMethod(
            name,
            methodReturnType,
            parameters);
  }

  private PageObjectMethod getComposeMethod(TranslationContext context) {
    if(compose == null || compose.length == 0) {
      throw new UtamCompilationError(String.format(ERR_METHOD_EMPTY_STATEMENTS, name));
    }
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
      statementDeclaration.checkBeforeLoadElements(methodContext);
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
