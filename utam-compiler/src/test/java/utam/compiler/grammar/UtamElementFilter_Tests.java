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
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_FILTER_NEEDS_LIST;
import static utam.compiler.helpers.TypeUtilities.BasicElementInterface.actionable;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
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
      new UtamArgument[]{new UtamArgument("text", "string")};
  private static final UtamArgument[] ONE_BOOLEAN_ARGS =
      new UtamArgument[]{new UtamArgument("bool", "boolean")};

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
    UtamElement object = DeserializerUtilities
        .getObjectFromFile("basicFilterDuplicateArgs", UtamElement.class);
    UtamElement.Traversal element = object.getAbstraction();
    UtamError e =
        expectThrows(UtamError.class, () -> element.testRootTraverse(
            TestUtilities.getTestTranslationContext()));
    assertThat(e.getMessage(), containsString("duplicate parameters"));
  }

  @Test
  public void testFilterByGetAttribute() {
    TranslationContext context = new DeserializerUtilities().getContext("basicFilterGetAttribute");
    ElementContext elementContext = context.getElement("element");
    PageObjectMethod method = elementContext.getElementMethod();
    assertThat(elementContext.isList(), is(equalTo(true)));
    PageObjectValidationTestHelper.MethodInfo methodInfo =
        new PageObjectValidationTestHelper.MethodInfo("getElementElement", "List<ElementElement>");
    for (int i = 1; i <= 3; i++) {
      methodInfo.addParameter(
          new PageObjectValidationTestHelper.MethodParameterInfo("arg" + i, "String"));
    }
    methodInfo.addCodeLines(
        "element(this.element)"
            + ".buildList(ElementElement.class, ElementElementImpl.class, "
            + "elm -> { String tmp = elm.getAttribute(arg2);\n"
            + "return tmp!= null && tmp.contains(arg3); }, arg1)");
    methodInfo.setIsPublic(false);
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testFilterByIsVisibleFalseFindFirst() {
    TranslationContext context = new DeserializerUtilities().getContext("basicFilterIsVisible");
    ElementContext elementContext = context.getElement("element");
    PageObjectMethod method = elementContext.getElementMethod();
    assertThat(elementContext.isList(), is(equalTo(false)));
    PageObjectValidationTestHelper.MethodInfo methodInfo =
        new PageObjectValidationTestHelper.MethodInfo(
            "getElement", "ElementElement");
    methodInfo.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("arg1", "String"));
    methodInfo.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("arg2", "String"));
    methodInfo.addCodeLines(
        "element(this.element).build(ElementElement.class, ElementElementImpl.class, elm -> Boolean.FALSE.equals(elm.isVisible()), arg1, arg2)");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testCustomFilterBooleanMatcherFindAllNullable() {
    TranslationContext context = new DeserializerUtilities().getContext("customFilterBoolean");
    ElementContext elementContext = context.getElement("custom");
    PageObjectMethod method = elementContext.getElementMethod();
    assertThat(elementContext.isList(), is(equalTo(true)));
    PageObjectValidationTestHelper.MethodInfo methodInfo =
        new PageObjectValidationTestHelper.MethodInfo("getCustom", "List<Test>");
    methodInfo.addCodeLines(
        "inScope(this.root, LocatorBy.byCss(\"selector3\"), true, true)"
            + ".buildList(Test.class, elm -> elm.isVisible())");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testCustomNestedFilterFindFirst() {
    UtamElement object = DeserializerUtilities
        .getObjectFromFile("customFilterNested", UtamElement.class);
    TranslationContext context = TestUtilities.getTestTranslationContext();
    object.testTraverse(context);
    ElementContext elementContext = context.getElement("nested");
    PageObjectMethod method = elementContext.getElementMethod();
    assertThat(elementContext.isList(), is(equalTo(false)));

    PageObjectValidationTestHelper.MethodInfo methodInfo =
        new PageObjectValidationTestHelper.MethodInfo("getNested", "Test");
    for (int i = 1; i <= 3; i++) {
      methodInfo.addParameter(
          new PageObjectValidationTestHelper.MethodParameterInfo("arg" + i, "String"));
    }
    methodInfo.addCodeLines(
        "inScope(this.element.setParameters(arg1), LocatorBy.byCss(String.format(\"selector2 %s\", arg2)), false, false)"
            + ".build(Test.class, elm -> elm.customMethod(arg3))");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }
}
