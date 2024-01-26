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
    return new MethodContext(
        "test", ReturnType.RETURN_VOID, getTestTranslationContext(), false, false);
  }

  private static void test(String jsonFile, String expectedError) {
    UtamError e =
        expectThrows(
            UtamError.class,
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
    UtamError e =
        expectThrows(UtamError.class, () -> context.setParameter(new Regular("name", STRING)));
    assertThat(
        e.getMessage(),
        containsString(
            "error 107: method \"test\" arguments: argument with name \"name\" is already"
                + " declared"));
  }

  @Test
  public void testDuplicateArgNameInStmntThrows() {
    MethodContext methodContext = getMethodContext();
    ParametersContext context = methodContext.getParametersContext();
    setStatementParameter(context, new Regular("name", STRING));
    UtamError e =
        expectThrows(
            UtamError.class, () -> setStatementParameter(context, new Regular("name", STRING)));
    assertThat(
        e.getMessage(),
        containsString(
            "error 107: method \"test\" arguments: argument with name \"name\" is already"
                + " declared"));
  }

  @Test
  public void testLiteralMethodParameterThrows() {
    MethodContext methodContext = getMethodContext();
    ParametersContext context = methodContext.getParametersContext();
    UtamError e =
        expectThrows(UtamError.class, () -> context.setParameter(new Literal(true, BOOLEAN)));
    assertThat(
        e.getMessage(),
        containsString("error 105: method \"test\" arguments: literal arguments are not allowed"));
  }

  @Test
  public void testArgNotReferencedThrows() {
    MethodContext methodContext = getMethodContext();
    ParametersContext context = methodContext.getParametersContext();
    context.setParameter(new Regular("arg", STRING));
    UtamError e = expectThrows(UtamError.class, context::getParameters);
    assertThat(
        e.getMessage(),
        containsString("error 503: method \"test\": declared parameter \"arg\" is never used"));
  }

  @Test
  public void testComposeWithInvalidArgReferenceThrows() {
    test(
        "referencedArgNotFound",
        "error 502: method \"test\": statement declares a reference to an \"arg1\" argument, "
            + "but there is no matching argument at the method level");
  }

  @Test
  public void testReferenceTypeNotAllowed() {
    ParametersContext context = getMethodContext().getParametersContext();
    UtamError e =
        expectThrows(
            UtamError.class, () -> context.setParameter(new Regular("arg", PARAMETER_REFERENCE)));
    assertThat(
        e.getMessage(),
        containsString(
            "error 501: method \"test\" arguments: parameter \"arg\" at method level can't have"
                + " type \"argumentReference\""));
  }

  @Test
  public void testSameParameterReusedThrows() {
    test(
        "sameArgsNames",
        "error 107: method \"test\" arguments: argument with name \"arg1\" is already declared");
  }

  @Test
  public void testRedundantMethodLevelParameterThrows() {
    test(
        "redundantParameters",
        "error 503: method \"test\": declared parameter \"arg\" is never used");
  }
}
