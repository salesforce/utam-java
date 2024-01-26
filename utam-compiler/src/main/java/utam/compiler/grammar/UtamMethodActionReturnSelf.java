/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.ReturnSelf;

/**
 * "returnSelf" statement
 *
 * @author elizaveta.ivanova
 * @since 236
 */
@JsonDeserialize
class UtamMethodActionReturnSelf extends UtamMethodAction {

  static final String RETURN_SELF = "returnSelf";

  @JsonCreator
  UtamMethodActionReturnSelf(@JsonProperty(value = "apply", required = true) String apply) {
    super(null, null, null, null, null, false);
  }

  @Override
  Statement getStatement(
      TranslationContext context, MethodContext methodContext, StatementContext statementContext) {
    throw new IllegalStateException("Compose statement is set without intermittent object");
  }

  @Override
  ComposeMethodStatement getComposeAction(
      TranslationContext context, MethodContext methodContext, StatementContext statementContext) {
    if (!statementContext.isLastStatement() && !statementContext.isLastPredicateStatement()) {
      String message = VALIDATION.getErrorMessage(608, methodContext.getName());
      throw new UtamCompilationError(message);
    }
    return new ReturnSelf(context.getSelfType());
  }
}
