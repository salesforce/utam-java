/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.testng.Assert.expectThrows;
import static utam.compiler.helpers.MethodContext.ERR_ARG_DUPLICATE_NAME;
import static utam.compiler.helpers.MethodContext.ERR_ARG_TYPE_MISMATCH;
import static utam.compiler.helpers.MethodContext.ERR_LITERAL_PARAMETER_NOT_ALLOWED;
import static utam.compiler.helpers.MethodContext.ERR_METHOD_REFERENCE_ARGS;
import static utam.compiler.helpers.MethodContext.ERR_REFERENCE_MISSING;
import static utam.compiler.helpers.PrimitiveType.BOOLEAN;
import static utam.compiler.helpers.PrimitiveType.STRING;
import static utam.compiler.helpers.TypeUtilities.PARAMETER_REFERENCE;

import org.testng.annotations.Test;
import utam.compiler.helpers.ParameterUtils.Literal;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.core.declarative.representation.MethodParameter;
import utam.core.framework.consumer.UtamError;

/**
 * @author elizaveta.ivanova
 * @since 232
 */
public class MethodContextTests {

  @Test
  public void testDuplicateArgNameInMethod() {
    MethodContext methodContext = new MethodContext();
    methodContext.setDeclaredParameter(new Regular("name", STRING));
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setDeclaredParameter(new Regular("name", STRING)));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_ARG_DUPLICATE_NAME, "method 'test'", "name")));
  }

  @Test
  public void testDuplicateArgNameInStmnt() {
    MethodContext methodContext = new MethodContext();
    StatementContext statementContext = new StatementContext();
    methodContext.setStatementParameter(new Regular("name", STRING), statementContext);
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setStatementParameter(new Regular("name", STRING), statementContext));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_ARG_DUPLICATE_NAME, "method 'test'", "name")));
  }

  @Test
  public void testLiteralMethodParameter() {
    MethodContext methodContext = new MethodContext();
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setDeclaredParameter(new Literal(true, BOOLEAN)));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_LITERAL_PARAMETER_NOT_ALLOWED, "method 'test'", "true")));
  }

  @Test
  public void testArgNotReferenced() {
    MethodContext methodContext = new MethodContext();
    methodContext.setDeclaredParameter(new Regular("arg", STRING));
    StatementContext statementContext = new StatementContext();
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setStatementParameter(new Regular("arg1", STRING), statementContext));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_REFERENCE_MISSING, "method 'test'", "arg1")));
  }

  @Test
  public void testArgWrongType() {
    MethodContext methodContext = new MethodContext();
    methodContext.setDeclaredParameter(new Regular("arg", STRING));
    StatementContext statementContext = new StatementContext();
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setStatementParameter(new Regular("arg", BOOLEAN), statementContext));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_ARG_TYPE_MISMATCH, "method 'test'", "arg", "Boolean")));
  }

  @Test
  public void testArgRightType() {
    MethodContext methodContext = new MethodContext();
    MethodParameter parameter = new Regular("arg", STRING);
    StatementContext statementContext = new StatementContext();
    methodContext.setDeclaredParameter(parameter);
    methodContext.setStatementParameter(parameter, statementContext);
  }

  @Test
  public void testComposeWithInvalidArgReference() {
    MethodContext methodContext = new MethodContext();
    StatementContext statementContext = new StatementContext();
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setStatementParameter(new Regular("arg", PARAMETER_REFERENCE), statementContext));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_REFERENCE_MISSING, "method 'test'", "arg")));
  }

  @Test
  public void testReferenceTypeNotAllowed() {
    MethodContext methodContext = new MethodContext();
    UtamError e = expectThrows(UtamError.class, () -> methodContext.setDeclaredParameter(new Regular("arg",
        PARAMETER_REFERENCE)));
    assertThat(e.getMessage(), containsString(String.format(ERR_METHOD_REFERENCE_ARGS, "method 'test'", "arg")));
  }
}
