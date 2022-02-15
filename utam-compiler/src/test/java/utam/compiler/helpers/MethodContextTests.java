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
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.helpers.PrimitiveType.BOOLEAN;
import static utam.compiler.helpers.PrimitiveType.STRING;
import static utam.compiler.helpers.TypeUtilities.PARAMETER_REFERENCE;

import org.hamcrest.core.StringContains;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.compiler.helpers.ParameterUtils.Literal;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.compiler.helpers.ParametersContext.StatementParametersContext;
import utam.compiler.helpers.ReturnType.MethodReturnType;
import utam.core.declarative.representation.MethodParameter;
import utam.core.framework.consumer.UtamError;

/**
 * method context tests
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class MethodContextTests {

  private static MethodContext getMethodContext() {
    return new MethodContext("test", new MethodReturnType("test"),
        getTestTranslationContext(),
        null, false);
  }

  private static ParametersContext getParametersContext(MethodContext methodContext) {
    return new StatementParametersContext("test", getTestTranslationContext(), null,
        methodContext);

  }

  private static void test(String jsonFile, String expectedError) {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getContext("validate/compose/" + jsonFile));
    assertThat(e.getMessage(), StringContains.containsString(expectedError));
  }

  private static void setStatementParameter(ParametersContext context, MethodParameter p) {
    context.setParameter(p);
  }

  @Test
  public void testDuplicateArgNameInMethodThrows() {
    MethodContext methodContext = getMethodContext();
    ParametersContext context = methodContext.getParametersContext();
    context.setParameter(new Regular("name", STRING));
    UtamError e = expectThrows(UtamError.class,
        () -> context.setParameter(new Regular("name", STRING)));
    assertThat(e.getMessage(),
        containsString("error UA007: method \"test\": parameter with name \"name\" is already declared"));
  }

  @Test
  public void testDuplicateArgNameInStmntThrows() {
    MethodContext methodContext = getMethodContext();
    ParametersContext context = getParametersContext(methodContext);
    setStatementParameter(context, new Regular("name", STRING));
    UtamError e = expectThrows(UtamError.class,
        () -> setStatementParameter(context, new Regular("name", STRING)));
    assertThat(e.getMessage(),
        containsString("error UA007: method \"test\": parameter with name \"name\" is already declared"));
  }

  @Test
  public void testLiteralMethodParameterThrows() {
    MethodContext methodContext = getMethodContext();
    ParametersContext context = methodContext.getParametersContext();
    UtamError e = expectThrows(UtamError.class,
        () -> context.setParameter(new Literal(true, BOOLEAN)));
    assertThat(e.getMessage(),
        containsString("error UA005: method \"test\": literal arguments are not allowed"));
  }

  @Test
  public void testArgNotReferencedThrows() {
    MethodContext methodContext = getMethodContext();
    ParametersContext context = methodContext.getParametersContext();
    context.setParameter(new Regular("arg", STRING));
    UtamError e = expectThrows(UtamError.class, context::getParameters);
    assertThat(e.getMessage(),
        containsString("error UM003: method \"test\": declared parameter \"arg\" is never used"));
  }

  @Test
  public void testComposeWithInvalidArgReferenceThrows() {
    MethodContext methodContext = getMethodContext();
    ParametersContext context = getParametersContext(methodContext);
    UtamError e = expectThrows(UtamError.class,
        () -> setStatementParameter(context, new Regular("arg", PARAMETER_REFERENCE)));
    assertThat(e.getMessage(), containsString(
        "error UM002: method \"test\": statement parameter \"arg\" is not found in declared method args"));
  }

  @Test
  public void testReferenceTypeNotAllowed() {
    ParametersContext context = getMethodContext().getParametersContext();
    UtamError e = expectThrows(UtamError.class,
        () -> context.setParameter(new Regular("arg", PARAMETER_REFERENCE)));
    assertThat(e.getMessage(), containsString(
        "error UM004: method \"test\": parameter \"arg\" at method level can't have type \"argumentReference\""));
  }

  @Test
  public void testSameParameterReusedThrows() {
    test("sameArgsNames", "error UA007: method \"test\": parameter with name \"arg1\" is already declared");
  }

  @Test
  public void testRedundantMethodLevelParameterThrows() {
    test("redundantParameters",
        "error UM003: method \"test\": declared parameter \"arg\" is never used");
  }
}
