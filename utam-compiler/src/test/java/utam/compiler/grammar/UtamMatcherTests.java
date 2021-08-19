/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.endsWith;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.grammar.UtamMatcher.ERR_INCORRECT_MATCHER_FOR_METHOD;
import static utam.compiler.helpers.TypeUtilities.BasicElementInterface.actionable;

import org.hamcrest.Matchers;
import org.testng.annotations.Test;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementContext.Basic;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;

/**
 * tests for matcher
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamMatcherTests {

  @Test
  public void testMethodStatementAction() {
    UtamMatcher matcher = new UtamMatcher(MatcherType.isFalse, null);
    matcher.getParameters(getTestTranslationContext(), new MethodContext());

    matcher = new UtamMatcher(MatcherType.isTrue, new UtamArgument[0]);
    matcher.getParameters(getTestTranslationContext(), new MethodContext());
  }

  @Test
  public void testMethodStatementActionWrongArgsNumberThrows() {
    UtamMatcher matcher = new UtamMatcher(MatcherType.isTrue, new UtamArgument[] { new UtamArgument(true)});
    UtamError e = expectThrows(UtamError.class, () -> matcher.getParameters(getTestTranslationContext(), new MethodContext()));
    assertThat(e.getMessage(), is(endsWith("expected 0 parameters, provided 1")));
  }

  @Test
  public void testElementFilterWrongArgsTypeThrows() {
    UtamMatcher matcher = new UtamMatcher(MatcherType.stringContains, new UtamArgument[] { new UtamArgument(true)});
    UtamError e = expectThrows(UtamError.class, () -> matcher.getParameters(getTestTranslationContext(), "element"));
    assertThat(e.getMessage(), is(containsString("expected type is")));
  }

  @Test
  public void testIncorrectActionTypeForMatcherThrows() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    ElementContext elementContext = new Basic("element");
    context.setElement(elementContext);
    PageObjectMethod method = mock(PageObjectMethod.class);
    MethodDeclaration declaration = mock(MethodDeclaration.class);
    when(method.getDeclaration()).thenReturn(declaration);
    when(declaration.getName()).thenReturn("getElement()");
    elementContext.setElementMethod(method);
    UtamMethodAction action = new UtamMethodAction("element", "isPresent", null, new UtamMatcher(
        MatcherType.stringContains, new UtamArgument[] { new UtamArgument("expected")}), null);

    UtamError e = expectThrows(UtamError.class, () -> action.getComposeAction(context, new MethodContext(), false));
    assertThat(
        e.getMessage(),
        Matchers.containsString(
            String.format(ERR_INCORRECT_MATCHER_FOR_METHOD, MatcherType.stringContains, "String")));
  }

  @Test
  public void testIncorrectFilterTypeForMatcherThrows() {
    UtamElementFilter filter =
        new UtamElementFilter(
            "focus",
            new UtamArgument[]{new UtamArgument("text", "string")},
            new UtamMatcher(MatcherType.isFalse, null), false);
    UtamError e = expectThrows(UtamError.class, () ->
        filter.setElementFilter(getTestTranslationContext(), UtamElement.Type.BASIC, actionable,
            "element"));
    assertThat(
        e.getMessage(),
        Matchers.containsString(
            String.format(ERR_INCORRECT_MATCHER_FOR_METHOD, MatcherType.isFalse, "Boolean")));
  }
}
