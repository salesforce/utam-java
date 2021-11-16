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
import static utam.compiler.helpers.MethodContext.ERR_PARAMETER_NEVER_USED;
import static utam.compiler.helpers.MethodContext.ERR_REFERENCE_MISSING;
import static utam.compiler.helpers.PrimitiveType.BOOLEAN;
import static utam.compiler.helpers.PrimitiveType.STRING;
import static utam.compiler.helpers.TypeUtilities.PARAMETER_REFERENCE;

import org.hamcrest.core.StringContains;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.compiler.helpers.ParameterUtils.Literal;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.core.declarative.representation.MethodParameter;
import utam.core.framework.consumer.UtamError;

/**
 * method context tests
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class MethodContextTests {

  private static final String ERR_MESSAGE_PREFIX = "method 'test'";

  private static MethodContext getTestMethodContext() {
    return new MethodContext("test", new ReturnType(null, null, "test"));
  }

  private static void test(String jsonFile, String expectedError) {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getContext("validate/compose/" + jsonFile));
    assertThat(e.getMessage(), StringContains.containsString(expectedError));
  }

  @Test
  public void testDuplicateArgNameInMethod() {
    MethodContext methodContext = getTestMethodContext();
    methodContext.setDeclaredParameter(new Regular("name", STRING));
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setDeclaredParameter(new Regular("name", STRING)));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_ARG_DUPLICATE_NAME, ERR_MESSAGE_PREFIX, "name")));
  }

  @Test
  public void testDuplicateArgNameInStmnt() {
    MethodContext methodContext = getTestMethodContext();
    StatementContext statementContext = new StatementContext();
    methodContext.setStatementParameter(new Regular("name", STRING), statementContext);
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setStatementParameter(new Regular("name", STRING), statementContext));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_ARG_DUPLICATE_NAME, ERR_MESSAGE_PREFIX, "name")));
  }

  @Test
  public void testLiteralMethodParameter() {
    MethodContext methodContext = getTestMethodContext();
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setDeclaredParameter(new Literal(true, BOOLEAN)));
    assertThat(e.getMessage(),
        containsString(
            String.format(ERR_LITERAL_PARAMETER_NOT_ALLOWED, ERR_MESSAGE_PREFIX, "true")));
  }

  @Test
  public void testArgNotReferenced() {
    MethodContext methodContext = getTestMethodContext();
    methodContext.setDeclaredParameter(new Regular("arg", STRING));
    StatementContext statementContext = new StatementContext();
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setStatementParameter(new Regular("arg1", STRING), statementContext));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_REFERENCE_MISSING, ERR_MESSAGE_PREFIX, "arg1")));
  }

  @Test
  public void testArgWrongType() {
    MethodContext methodContext = getTestMethodContext();
    methodContext.setDeclaredParameter(new Regular("arg", STRING));
    StatementContext statementContext = new StatementContext();
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setStatementParameter(new Regular("arg", BOOLEAN), statementContext));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_ARG_TYPE_MISMATCH, ERR_MESSAGE_PREFIX, "arg", "Boolean")));
  }

  @Test
  public void testArgRightType() {
    MethodContext methodContext = getTestMethodContext();
    MethodParameter parameter = new Regular("arg", STRING);
    StatementContext statementContext = new StatementContext();
    methodContext.setDeclaredParameter(parameter);
    methodContext.setStatementParameter(parameter, statementContext);
  }

  @Test
  public void testComposeWithInvalidArgReference() {
    MethodContext methodContext = getTestMethodContext();
    StatementContext statementContext = new StatementContext();
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext
            .setStatementParameter(new Regular("arg", PARAMETER_REFERENCE), statementContext));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_REFERENCE_MISSING, ERR_MESSAGE_PREFIX, "arg")));
  }

  @Test
  public void testReferenceTypeNotAllowed() {
    MethodContext methodContext = getTestMethodContext();
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setDeclaredParameter(new Regular("arg",
            PARAMETER_REFERENCE)));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_METHOD_REFERENCE_ARGS, ERR_MESSAGE_PREFIX, "arg")));
  }

  @Test
  public void testSameParameterReusedThrows() {
    String expectedErr = String.format(ERR_ARG_DUPLICATE_NAME, ERR_MESSAGE_PREFIX, "arg1");
    test("sameArgsNames", expectedErr);
  }

  @Test
  public void testRedundantMethodLevelParameterThrows() {
    String expectedErr = String.format(ERR_PARAMETER_NEVER_USED, ERR_MESSAGE_PREFIX, "arg");
    test("redundantParameters", expectedErr);
  }
}
