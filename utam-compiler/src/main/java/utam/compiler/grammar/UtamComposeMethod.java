/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.grammar.JsonDeserializer.readNode;
import static utam.compiler.grammar.UtamMethodActionReturnSelf.RETURN_SELF;
import static utam.compiler.grammar.UtamMethodActionWaitFor.WAIT_FOR;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.ReturnType;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.StatementContext.StatementType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethod;
import utam.compiler.representation.ComposeMethodStatement;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * public method declared at PO level
 *
 * @author elizaveta.ivanova
 * @since 228
 */
class UtamComposeMethod extends UtamMethod {

  private final List<UtamMethodAction> composeList;
  private final boolean isPublic;

  @JsonCreator
  UtamComposeMethod(
      @JsonProperty(value = "name", required = true) String name,
      @JsonProperty(value = "compose") JsonNode composeNodes,
      @JsonProperty(value = "args") JsonNode argsNode,
      @JsonProperty("description") JsonNode descriptionNode) {
    super(name, descriptionNode, argsNode);
    this.composeList = processComposeNodes(name, composeNodes);
    this.isPublic = true;
  }

  /**
   * Constructor used to build waitForElement method
   *
   * @param name name of the method
   * @param description description of the method
   * @param compose statements
   * @param isPublic if the method is public
   */
  UtamComposeMethod(
      String name,
      UtamMethodDescription description,
      List<UtamMethodAction> compose,
      boolean isPublic) {
    super(name, description);
    this.composeList = compose;
    this.isPublic = isPublic;
  }

  /**
   * process compose method nodes
   *
   * @param methodName name of the method
   * @param composeNodes json nodes
   * @return list of statements
   */
  static List<UtamMethodAction> processComposeNodes(String methodName, JsonNode composeNodes) {
    VALIDATION.validateNotEmptyArray(
        composeNodes, String.format("method \"%s\"", methodName), "compose");
    List<UtamMethodAction> res = new ArrayList<>();
    for (JsonNode composeNode : composeNodes) {
      UtamMethodAction action = processComposeStatementNode(methodName, composeNode);
      res.add(action);
    }
    return res;
  }

  private static UtamMethodAction processComposeStatementNode(
      String methodName, JsonNode composeNode) {
    VALIDATION.validateNotNullObject(
        composeNode, String.format("method \"%s\"", methodName), "compose statement");
    JsonNode applyNode = composeNode.get("apply");
    JsonNode applyExternalNode = composeNode.get("applyExternal");
    JsonNode elementNode = composeNode.get("element");
    String apply = applyNode != null ? applyNode.asText() : null;
    String applyExternal = applyExternalNode != null ? applyExternalNode.asText() : null;
    String elementName = elementNode != null ? elementNode.asText() : null;
    if (apply == null && applyExternal == null) {
      if (elementName == null) {
        throw new UtamCompilationError(composeNode, VALIDATION.getErrorMessage(609, methodName));
      }
      return readNode(
          composeNode, UtamMethodActionGetter.class, VALIDATION.getErrorMessage(600, methodName));
    }
    if (apply != null && applyExternal != null) {
      throw new UtamCompilationError(composeNode, VALIDATION.getErrorMessage(610, methodName));
    }
    String error = VALIDATION.getErrorMessage(600, methodName);
    if (applyExternal != null) {
      if (elementName != null) {
        throw new UtamCompilationError(composeNode, VALIDATION.getErrorMessage(611, methodName));
      }
      return readNode(composeNode, UtamMethodActionUtility.class, error);
    }
    if (WAIT_FOR.equals(apply)) {
      return readNode(composeNode, UtamMethodActionWaitFor.class, error);
    }
    if (RETURN_SELF.equals(apply)) {
      return readNode(composeNode, UtamMethodActionReturnSelf.class, error);
    }
    return readNode(composeNode, UtamMethodActionApply.class, error);
  }

  /**
   * get compose statements from compose statements
   *
   * @param context translation context
   * @param methodContext method context
   * @param compose compose statements
   * @return list of processed statements
   */
  static List<ComposeMethodStatement> getComposeStatements(
      TranslationContext context, MethodContext methodContext, List<UtamMethodAction> compose) {
    List<ComposeMethodStatement> statements = new ArrayList<>();
    String name = methodContext.getName();
    TypeProvider previousStatementReturn = null;
    for (int i = 0; i < compose.size(); i++) {
      UtamMethodAction statementDeclaration = compose.get(i);
      StatementContext statementContext =
          new StatementContext(
              previousStatementReturn,
              i,
              isUsedAsChain(compose, i),
              i == compose.size() - 1
                  ? StatementType.LAST_STATEMENT
                  : StatementType.REGULAR_STATEMENT,
              statementDeclaration.getDeclaredReturnType(name));
      statementDeclaration.checkBeforeLoadElements(methodContext);
      ComposeMethodStatement statement =
          statementDeclaration.getComposeAction(context, methodContext, statementContext);
      previousStatementReturn = statement.getReturnType();
      statements.add(statement);
    }
    return statements;
  }

  /**
   * check if statement with a given index is a chain
   *
   * @param compose statements
   * @param index index of the current statement
   * @return boolean
   */
  static boolean isUsedAsChain(List<UtamMethodAction> compose, int index) {
    if (index == compose.size() - 1) {
      return false;
    }
    return compose.get(index + 1).isChain;
  }

  @Override
  final PageObjectMethod getMethod(TranslationContext context) {
    // return type at method level is not supported, so infer from last statement
    ReturnType lastStatementReturn =
        composeList.get(composeList.size() - 1).getDeclaredReturnType(name);
    MethodContext methodContext =
        new MethodContext(name, lastStatementReturn, context, false, hasMethodLevelArgs());
    ParametersContext parametersContext = methodContext.getParametersContext();
    setMethodLevelParameters(context, methodContext);
    List<ComposeMethodStatement> statements =
        getComposeStatements(context, methodContext, composeList);
    List<MethodParameter> parameters = parametersContext.getParameters();
    ComposeMethodStatement lastStatement = statements.get(statements.size() - 1);
    TypeProvider lastStatementReturnType = lastStatement.getReturnType();
    return new ComposeMethod(
        name, lastStatementReturnType, parameters, statements, description, isPublic);
  }
}
