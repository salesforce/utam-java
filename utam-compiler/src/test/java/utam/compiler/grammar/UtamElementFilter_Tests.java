package utam.compiler.grammar;

import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;
import utam.compiler.helpers.*;

import java.util.List;

import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_FILTER_NEEDS_LIST;
import static utam.compiler.grammar.UtamElementFilter.ERR_INCORRECT_MATCHER_FOR_METHOD;
import static utam.compiler.helpers.TypeUtilities.Element.actionable;
import static utam.compiler.helpers.TypeUtilities.Element.editable;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;

/**
 * Provides tests of UtamElementFilter for basic and custom elements
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class UtamElementFilter_Tests {

  private static final String ELEMENT_NAME = "test";
  private static final TypeProvider ACTIONABLE_TYPE = actionable.getType();
  private static final UtamArgument[] ONE_STRING_ARGS =
      new UtamArgument[] {new UtamArgument("text", "string")};
  private static final UtamArgument[] ONE_BOOLEAN_ARGS =
      new UtamArgument[] {new UtamArgument("bool", "boolean")};

  static UtamElementFilter getInnerTextFilter() {
    return new UtamElementFilter(
        "getText", new UtamMatcher(MatcherType.stringContains, ONE_STRING_ARGS));
  }

  @Test
  public void testBasicElementGetTextFilter() {
    UtamElementFilter filter =
        new UtamElementFilter(
            "getText", new UtamMatcher(MatcherType.stringContains, ONE_STRING_ARGS));
    filter.setElementFilter(UtamElement.Type.BASIC, ACTIONABLE_TYPE, ELEMENT_NAME);
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
        () -> filter.setElementFilter(UtamElement.Type.BASIC, ACTIONABLE_TYPE, ELEMENT_NAME));
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
            () -> filter.setElementFilter(UtamElement.Type.BASIC, ACTIONABLE_TYPE, ELEMENT_NAME));
    assertThat(e.getMessage(), containsString("wrongMethod"));
  }

  @Test
  public void testBasicElementIncompatibleFilterThrows() {
    UtamElementFilter filter =
        new UtamElementFilter(
            "focus", ONE_STRING_ARGS, new UtamMatcher(MatcherType.isFalse, null), false);
    UtamError e =
        expectThrows(
            UtamError.class,
            () -> filter.setElementFilter(UtamElement.Type.BASIC, ACTIONABLE_TYPE, ELEMENT_NAME));
    assertThat(
        e.getMessage(),
        is(
            equalTo(
                String.format(
                    ERR_INCORRECT_MATCHER_FOR_METHOD,
                    ELEMENT_NAME,
                    MatcherType.isFalse,
                    "Boolean"))));
  }

  @Test
  public void testCustomElementGetTextFilter() {
    UtamElementFilter filter =
        new UtamElementFilter(
            "getCustomText",
            ONE_STRING_ARGS,
            new UtamMatcher(MatcherType.stringEquals, ONE_STRING_ARGS),
            false);
    filter.setElementFilter(UtamElement.Type.CUSTOM, ACTIONABLE_TYPE, ELEMENT_NAME);
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
        () -> filter.setElementFilter(UtamElement.Type.CUSTOM, ACTIONABLE_TYPE, ELEMENT_NAME));
  }

  @Test
  public void testCustomElementGetTextFilterWithWrongArgTypeThrows() {
    UtamElementFilter filter =
        new UtamElementFilter(
            "getCustomText",
            null,
            new UtamMatcher(MatcherType.stringContains, ONE_BOOLEAN_ARGS),
            false);
    assertThrows(
        () -> filter.setElementFilter(UtamElement.Type.CUSTOM, ACTIONABLE_TYPE, ELEMENT_NAME));
  }

  @Test
  public void testBasicFilterWithoutListThrows() {
    UtamElement utamElement = new UtamElement("element");
    utamElement.selector = new UtamSelector("css");
    utamElement.filter = getInnerTextFilter();
    UtamError e = expectThrows(UtamError.class, () -> utamElement.getAbstraction());
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ELEMENT_FILTER_NEEDS_LIST, "element")));
  }

  @Test
  public void testCustomFilterWithoutListThrows() {
    UtamElement utamElement = new UtamElement("element");
    utamElement.type = TestUtilities.TEST_URI;
    utamElement.selector = new UtamSelector("css");
    utamElement.filter = getInnerTextFilter();
    UtamError e = expectThrows(UtamError.class, () -> utamElement.getAbstraction());
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
        new PageObjectValidationTestHelper.MethodInfo("getElementElement", "List<Actionable>");
    for (int i = 1; i <= 3; i++) {
      methodInfo.addParameter(
          new PageObjectValidationTestHelper.MethodParameterInfo("arg" + i, "String"));
    }
    methodInfo.addCodeLines(
        "element(this.element).buildList(Actionable.class, elm -> elm.getAttribute(arg2).contains(arg3), arg1)");
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
            "getElement", editable.getType().getSimpleName());
    methodInfo.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("arg1", "String"));
    methodInfo.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("arg2", "String"));
    methodInfo.addCodeLines(
        "element(this.element).build(Editable.class, elm -> Boolean.FALSE.equals(elm.isVisible()), arg1,arg2)");
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
        "inScope(this.getRootElement(), by(\"selector3\", Selector.Type.CSS, true), true)"
            + ".buildList(Test.class, elm -> elm.isVisible())");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testCustomNestedFilterFindFirst() {
    UtamElement object = DeserializerUtilities.getObjectFromFile("customFilterNested", UtamElement.class);
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
        "inScope(this.getElementElement(arg1), by(String.format(\"selector2 %s\", arg2), Selector.Type.CSS, false), false)"
            + ".build(Test.class, elm -> elm.customMethod(arg3))");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }
}
