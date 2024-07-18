/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.emptyIterable;
import static utam.compiler.grammar.DeserializerUtilities.expectCompilerErrorFromFile;
import static utam.compiler.helpers.PrimitiveType.NUMBER;
import static utam.compiler.helpers.PrimitiveType.STRING;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT_IMPL_CLASS;

import java.util.List;
import org.hamcrest.Matchers;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities.Result;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;

/**
 * Provides tests of UtamElementFilter for basic and custom elements
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class UtamElementFilterTests {

  private static final String ELEMENT_NAME = "test";
  private static final String ELEMENT_METHOD_NAME = "getTest";
  private static final String BASIC_ELEMENT_TYPE_IMPL = BASIC_ELEMENT_IMPL_CLASS.getFullName();
  private static final String BASIC_ELEMENT_TYPE = BASIC_ELEMENT.getFullName();
  private static final String LIST_TYPE = List.class.getName();

  @Test
  public void testBasicElementFilterWithoutMatcherThrows() {
    Exception e = expectCompilerErrorFromFile("validate/filter/basicFilterNoMatcher");
    assertThat(
        e.getMessage(),
        containsString("error 300: element \"test\" filter: incorrect format of element filter"));
  }

  @Test
  public void testBasicElementWrongMethodInFilterThrows() {
    Exception e = expectCompilerErrorFromFile("validate/filter/basicFilterWrongMethod");
    assertThat(
        e.getMessage(),
        containsString(
            "error 301: element \"test\" filter: unknown action \"wrong\" for basic element"));
  }

  @Test
  public void testBasicElementUnsupportedMethodInFilterThrows() {
    Exception e = expectCompilerErrorFromFile("validate/filter/basicFilterUnsupportedMethod");
    assertThat(
        e.getMessage(),
        containsString(
            "error 303: element \"test\" filter: unsupported action \"click\" for type"
                + " \"Touchable\""));
  }

  @Test
  public void testCustomElementGetTextFilterWithWrongArgThrows() {
    Exception e = expectCompilerErrorFromFile("validate/filter/customWrongMatcherArg");
    assertThat(
        e.getMessage(),
        containsString(
            "error 108: element \"test\" filter matcher arguments: expected number of arguments is"
                + " 0, found 1"));
  }

  @Test
  public void testCustomFilterNonListThrows() {
    Exception e = expectCompilerErrorFromFile("validate/custom_element/filterForNonList");
    assertThat(
        e.getMessage(),
        containsString("error 302: element \"test\" filter: filter can only be set for list"));
  }

  @Test
  public void testDuplicateArgsNamesThrows() {
    Exception e = expectCompilerErrorFromFile("validate/filter/basicFilterDuplicateArgs");
    assertThat(
        e.getMessage(),
        containsString(
            "error 107: element \"element\" arguments: argument with name \"arg1\" is already"
                + " declared"));
  }

  @Test
  public void testFilterBasicPublicElementUnionType() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "TestElement");
    Result result = new DeserializerUtilities().getResultFromFile("filter/basicFilterPublicUnion");
    TranslationContext context = result.getContext();
    PageObjectMethod method = context.getElement(ELEMENT_NAME).getElementMethod();
    expected.addParameter(new MethodParameterInfo("text"));
    expected.addImpliedImportedTypes(BASIC_ELEMENT_TYPE);
    assertThat(method.getDeclaration().getImports(), Matchers.is(emptyIterable()));
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine(
        "return basic(root, this.test).build(TestElement.class, "
            + "TestElementImpl.class, elm -> text.equals(elm.getText()))");
    PageObjectValidationTestHelper.validateMethod(method, expected);
    String unionClass = context.getClassUnionTypes().get(0).getDeclarationCode().get(0);
    assertThat(
        unionClass,
        Matchers.is(
            Matchers.equalTo(
                "public static class TestElementImpl extends BasePageElement implements TestElement"
                    + " {}")));
    String unionType = context.getInterfaceUnionTypes().get(0).getDeclarationCode().get(0);
    assertThat(
        unionType,
        Matchers.is(Matchers.equalTo("interface TestElement extends Editable, Clickable {}")));
  }

  @Test
  public void testFilterBasicPublicElementDefaultType() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "List<BasicElement>");
    Result result = new DeserializerUtilities().getResultFromFile("filter/basicFilterPublic");
    TranslationContext context = result.getContext();
    PageObjectMethod method = context.getElement(ELEMENT_NAME).getElementMethod();
    assertThat(
        method.getDeclaration().getCodeLine(), is("List<BasicElement> getTest(String text)"));
    expected.addParameter(new MethodParameterInfo("text"));
    expected.addImportedTypes(LIST_TYPE, BASIC_ELEMENT_TYPE);
    expected.addImpliedImportedTypes(LIST_TYPE, BASIC_ELEMENT_TYPE_IMPL, BASIC_ELEMENT_TYPE);
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine(
        "return basic(root, this.test).buildList(BasicElement.class, BasePageElement.class, elm ->"
            + " text.equals(elm.getText()))");
    PageObjectValidationTestHelper.validateMethod(method, expected);
    assertThat(context.getInterfaceUnionTypes(), is(emptyIterable()));
    assertThat(context.getClassUnionTypes(), is(emptyIterable()));
  }

  @Test
  public void testNullableListWithFilter() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "BasicElement");
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine(
        "return basic(root, this.test)"
            + ".build(BasicElement.class, BasePageElement.class, "
            + "elm -> Boolean.TRUE.equals(elm.isVisible()))");
    Result result = new DeserializerUtilities().getResultFromFile("filter/basicElementFilter");
    TranslationContext context = result.getContext();
    PageObjectMethod method = context.getElement(ELEMENT_NAME).getElementMethod();
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testFilterByGetAttribute() {
    TranslationContext context =
        new DeserializerUtilities().getContext("filter/basicFilterGetAttribute");
    PageObjectMethod method = context.getMethod(ELEMENT_METHOD_NAME);
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "List<BasicElement>");
    expected.addParameter(new MethodParameterInfo("scopeArg"));
    expected.addParameter(new MethodParameterInfo("selectorArg"));
    expected.addParameter(new MethodParameterInfo("applyArg"));
    expected.addParameter(new MethodParameterInfo("matcherArg"));
    expected.addCodeLine("BasicElement scope = this.getScopeElement(scopeArg)");
    expected.addCodeLines(
        "return basic(scope, this.test.setParameters(selectorArg)).buildList(BasicElement.class,"
            + " BasePageElement.class, elm -> (elm.getAttribute(applyArg)!= null &&"
            + " elm.getAttribute(applyArg).contains(matcherArg)))");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testFilterByGetCssPropertyValue() {
    TranslationContext context =
        new DeserializerUtilities().getContext("filter/basicFilterGetCssPropertyValue");
    PageObjectMethod method = context.getMethod(ELEMENT_METHOD_NAME);
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "List<BasicElement>");
    expected.addParameter(new MethodParameterInfo("scopeArg"));
    expected.addParameter(new MethodParameterInfo("selectorArg"));
    expected.addParameter(new MethodParameterInfo("applyArg"));
    expected.addParameter(new MethodParameterInfo("matcherArg"));
    expected.addCodeLine("BasicElement scope = this.getScopeElement(scopeArg)");
    expected.addCodeLines(
        "return basic(scope, this.test.setParameters(selectorArg)).buildList(BasicElement.class,"
            + " BasePageElement.class, elm -> (elm.getCssPropertyValue(applyArg)!= null &&"
            + " elm.getCssPropertyValue(applyArg).contains(matcherArg)))");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testFilterByIsVisibleFalseFindFirst() {
    TranslationContext context =
        new DeserializerUtilities().getContext("filter/basicFilterIsVisible");
    PageObjectMethod method = context.getMethod(ELEMENT_METHOD_NAME);
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "TestElement");
    expected.addParameter(new MethodParameterInfo("scopeArg"));
    expected.addParameter(new MethodParameterInfo("selectorArg"));
    expected.addCodeLine("BasicElement scope = this.getScopeElement(scopeArg)");
    expected.addCodeLines(
        "return basic(scope, this.test.setParameters(selectorArg)).build(TestElement.class,"
            + " TestElementImpl.class, elm -> Boolean.FALSE.equals(elm.isVisible()))");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testElementNestedInsideFilteredList() {
    TranslationContext context =
        new DeserializerUtilities().getContext("nestedlist/nestedFilteredBasicList");
    final String ELEMENT_METHOD_NAME = "getNestedBasic";
    PageObjectMethod nestedGetter = context.getMethod(ELEMENT_METHOD_NAME);
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, BASIC_ELEMENT.getSimpleName());
    expected.addParameter(new MethodParameterInfo("selectorArg", STRING.getSimpleName()));
    expected.addParameter(
        new MethodParameterInfo("_basicListWithFilterIndex", NUMBER.getSimpleName()));
    expected.addCodeLine(
        "BasicElement basicListWithFilter = this._index_getBasicListWithFilter(selectorArg,"
            + " _basicListWithFilterIndex)");
    expected.addCodeLine(
        "return basic(basicListWithFilter, this.nestedBasic).build(BasicElement.class,"
            + " BasePageElement.class)");
    PageObjectValidationTestHelper.validateMethod(nestedGetter, expected);
  }

  @Test
  public void testIndexedMethodForElementWithFilter() {
    TranslationContext context =
        new DeserializerUtilities().getContext("nestedlist/nestedFilteredBasicList");
    final String ELEMENT_METHOD_NAME = "_index_getBasicListWithFilter";
    PageObjectMethod nestedGetter = context.getMethod(ELEMENT_METHOD_NAME);
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, BASIC_ELEMENT.getSimpleName());
    expected.setNotPublic();
    expected.addParameter(new MethodParameterInfo("selectorArg", STRING.getSimpleName()));
    expected.addParameter(
        new MethodParameterInfo("_basicListWithFilterIndex", NUMBER.getSimpleName()));
    expected.addCodeLine(
        "List<BasicElement> basicListWithFilter = this.getBasicListWithFilter(selectorArg)");
    expected.addCodeLine("if (basicListWithFilter.size() < _basicListWithFilterIndex-1) {");
    expected.addCodeLine(
        "throw new RuntimeException(\"Can't find scope element 'basicListWithFilter' with given"
            + " index!\")");
    expected.addCodeLine("}");
    expected.addCodeLine("return basicListWithFilter.get(_basicListWithFilterIndex)");
    PageObjectValidationTestHelper.validateMethod(nestedGetter, expected);
  }
}
