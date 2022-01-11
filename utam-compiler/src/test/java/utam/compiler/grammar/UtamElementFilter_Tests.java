/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.grammar.UtamArgumentTests.getNonLiteralArg;
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_FILTER_NEEDS_LIST;
import static utam.compiler.types.BasicElementInterface.actionable;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.PrimitiveType;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

/**
 * Provides tests of UtamElementFilter for basic and custom elements
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class UtamElementFilter_Tests {

  private static final String ELEMENT_NAME = "test";
  private static final TypeProvider ACTIONABLE_TYPE = actionable;
  private static final UtamArgument[] ONE_STRING_ARGS =
      new UtamArgument[]{
          getNonLiteralArg("text", "string")
      };
  private static final UtamArgument[] ONE_BOOLEAN_ARGS =
      new UtamArgument[]{
          getNonLiteralArg("bool", "boolean")
      };

  static UtamElementFilter getInnerTextFilter() {
    return new UtamElementFilter(
        "getText", new UtamMatcher(MatcherType.stringContains, ONE_STRING_ARGS));
  }

  private static void setElementFilter(UtamElementFilter filter, UtamElement.Type elementNodeType) {
    filter.setElementFilter(getTestTranslationContext(), elementNodeType, ACTIONABLE_TYPE,
        ELEMENT_NAME);
  }

  @Test
  public void testBasicElementGetTextFilter() {
    UtamElementFilter filter =
        new UtamElementFilter(
            "getText", new UtamMatcher(MatcherType.stringContains, ONE_STRING_ARGS));
    setElementFilter(filter, UtamElement.Type.BASIC);
    List<MethodParameter> applyMethodParams = filter.getApplyMethodParameters();
    assertThat(applyMethodParams, is(empty()));
    assertThat(filter.getMatcherType(), is(equalTo(MatcherType.stringContains)));
    List<MethodParameter> matcherParameters = filter.getMatcherParameters();
    assertThat(
        matcherParameters,
        is(containsInAnyOrder(new ParameterUtils.Regular("text", PrimitiveType.STRING))));
  }

  @Test
  public void testBasicElementGetTextWithArgsFilterThrows() {
    UtamElementFilter filter =
        new UtamElementFilter(
            "getText",
            ONE_STRING_ARGS,
            new UtamMatcher(MatcherType.stringContains, ONE_STRING_ARGS),
            false);
    assertThrows(
        () -> setElementFilter(filter, UtamElement.Type.BASIC));
  }

  @Test
  public void testBasicElementWrongMethodInFilterThrows() {
    UtamElementFilter filter =
        new UtamElementFilter(
            "wrongMethod",
            ONE_STRING_ARGS,
            new UtamMatcher(MatcherType.stringContains, ONE_STRING_ARGS),
            false);
    UtamError e =
        expectThrows(
            UtamError.class,
            () -> setElementFilter(filter, UtamElement.Type.BASIC));
    assertThat(e.getMessage(), containsString("wrongMethod"));
  }

  @Test
  public void testCustomElementGetTextFilter() {
    UtamElementFilter filter =
        new UtamElementFilter(
            "getCustomText",
            ONE_STRING_ARGS,
            new UtamMatcher(MatcherType.stringEquals, ONE_STRING_ARGS),
            false);
    setElementFilter(filter, UtamElement.Type.CUSTOM);
    List<MethodParameter> applyMethodParams = filter.getApplyMethodParameters();
    assertThat(
        applyMethodParams,
        is(containsInAnyOrder(new ParameterUtils.Regular("text", PrimitiveType.STRING))));
    assertThat(filter.getMatcherType(), is(equalTo(MatcherType.stringEquals)));
  }

  @Test
  public void testCustomElementGetTextFilterWithWrongArgThrows() {
    UtamElementFilter filter =
        new UtamElementFilter(
            "getCustomText",
            ONE_STRING_ARGS,
            new UtamMatcher(MatcherType.isTrue, ONE_STRING_ARGS),
            false);
    assertThrows(
        () -> setElementFilter(filter, UtamElement.Type.CUSTOM));
  }

  @Test
  public void testCustomElementGetTextFilterWithWrongArgTypeThrows() {
    UtamElementFilter filter =
        new UtamElementFilter(
            "getCustomText",
            null,
            new UtamMatcher(MatcherType.stringContains, ONE_BOOLEAN_ARGS),
            false);
    assertThrows(() -> setElementFilter(filter, UtamElement.Type.CUSTOM));
  }

  @Test
  public void testBasicFilterWithoutListThrows() {
    UtamElement utamElement = TestUtilities.UtamEntityCreator.createUtamElement("element");
    utamElement.selector = new UtamSelector("css");
    utamElement.filter = getInnerTextFilter();
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ELEMENT_FILTER_NEEDS_LIST, "element")));
  }

  @Test
  public void testCustomFilterWithoutListThrows() {
    UtamElement utamElement = TestUtilities.UtamEntityCreator.createUtamElement("element");
    utamElement.type = new String[]{TestUtilities.TEST_URI};
    utamElement.selector = new UtamSelector("css");
    utamElement.filter = getInnerTextFilter();
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ELEMENT_FILTER_NEEDS_LIST, "element")));
  }

  @Test
  public void testDuplicateArgsNames() {
    UtamError e =
        expectThrows(UtamError.class,
            () -> new DeserializerUtilities().getResultFromFile("filter/basicFilterDuplicateArgs"));
    assertThat(e.getMessage(), containsString("duplicate parameters"));
  }
}
