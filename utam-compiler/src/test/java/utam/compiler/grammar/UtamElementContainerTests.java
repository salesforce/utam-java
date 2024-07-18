/*
 * Copyright (c) 2024, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.findField;
import static utam.compiler.grammar.UtamElement.DEFAULT_CONTAINER_SELECTOR_CSS;
import static utam.compiler.helpers.PrimitiveType.NUMBER;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT_IMPL_CLASS;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.representation.ContainerMethod.PAGE_OBJECT_PARAMETER;
import static utam.compiler.representation.PageObjectValidationTestHelper.validateMethod;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.List;
import org.hamcrest.CoreMatchers;
import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.FieldInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;

/**
 * Test class dedicated to testing container element
 *
 * @author elizaveta.ivanova
 * @since 252
 */
public class UtamElementContainerTests {

  private static final String VALIDATION_DATA_ROOT = "validate/container_element/";

  private static final MethodParameterInfo FIRST_CONTAINER_PARAMETER =
      new PageObjectValidationTestHelper.MethodParameterInfo(
          PAGE_OBJECT_PARAMETER.getValue(), PAGE_OBJECT_PARAMETER.getType().getSimpleName());

  private static final String ELEMENT_NAME = "test";

  private static PageObjectMethod getContainerMethod(String fileName) {
    final String methodName = getElementGetterMethodName(ELEMENT_NAME, true);
    TranslationContext context = new DeserializerUtilities().getContext("container/" + fileName);
    ElementContext element = context.getElement(ELEMENT_NAME);
    assertThat(element.getType().isSameType(CONTAINER_ELEMENT), CoreMatchers.is(true));
    return context.getMethod(methodName);
  }

  private static TranslationContext compileNestedElements() {
    return new DeserializerUtilities().getContext("container/nestedElements");
  }

  @Test
  public void testLoadContainer() {
    Exception e =
        expectThrows(
            UtamCompilationError.class,
            () ->
                new DeserializerUtilities()
                    .getContext(VALIDATION_DATA_ROOT + "loadContainer")
                    .getMethod("test"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 206: element \"container\": property \"load\" is not supported for element with"
                + " arguments, filter or for container element"));
  }

  @Test
  public void testDuplicateArgsNamesThrows() {
    UtamCompilationError e =
        expectThrows(
            UtamCompilationError.class,
            () ->
                new DeserializerUtilities().getContext(VALIDATION_DATA_ROOT + "testDuplicateArgs"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 107: element 'test' arguments: argument with name \"arg\" is already declared"));
  }

  @Test
  public void testNamesCollisionForScopeGetterThrows() {
    UtamCompilationError e =
        expectThrows(
            UtamCompilationError.class,
            () ->
                new DeserializerUtilities()
                    .getContext(VALIDATION_DATA_ROOT + "methodNameCollisionForScope"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 504: method \"getTestScopeElement\": method with the same name was already"
                + " declared"));
  }

  @Test
  public void testPublicContainerWithParameters() {
    final String methodName = getElementGetterMethodName(ELEMENT_NAME, true);
    PageObjectMethod method = getContainerMethod("containerWithParameters");
    MethodInfo expected = new MethodInfo(methodName, "T");
    expected.addParameter(new MethodParameterInfo("scopeArg"));
    expected.addParameter(new MethodParameterInfo("selectorArg"));
    expected.addParameter(FIRST_CONTAINER_PARAMETER);
    expected.addImportedTypes(PAGE_OBJECT.getFullName());
    expected.addImpliedImportedTypes(
        PAGE_OBJECT.getFullName(), BASIC_ELEMENT.getFullName(), SELECTOR.getFullName());
    expected.addCodeLine("BasicElement scope = this.getScopeElement(scopeArg)");
    expected.addCodeLine(
        "LocatorBy testLocator = LocatorBy.byCss(String.format(\".css%s\", selectorArg))");
    expected.addCodeLine("return this.container(scope, true).load(pageObjectType, testLocator)");
    PageObjectValidationTestHelper.validateMethod(method, expected);
    assertThat(
        method.getDeclaration().getCodeLine(),
        CoreMatchers.is(
            "<T extends PageObject> T getTest(String scopeArg, String selectorArg, Class<T>"
                + " pageObjectType)"));
  }

  @Test
  public void testContainerWithDefaultSelector() {
    final String methodName = getElementGetterMethodName(ELEMENT_NAME, true);
    PageObjectMethod method = getContainerMethod("containerDefaultSelector");
    MethodInfo expected = new MethodInfo(methodName, "T");
    expected.addParameter(new MethodParameterInfo("scopeArg"));
    expected.addParameter(FIRST_CONTAINER_PARAMETER);
    expected.addCodeLine("BasicElement scope = this.getScopeElement(scopeArg)");
    expected.addCodeLine(
        String.format(
            "LocatorBy testLocator = LocatorBy.byCss(\"%s\")", DEFAULT_CONTAINER_SELECTOR_CSS));
    expected.addCodeLine("return this.container(scope, false).load(pageObjectType, testLocator)");
    PageObjectValidationTestHelper.validateMethod(method, expected);
    assertThat(
        method.getDeclaration().getCodeLine(),
        CoreMatchers.is(
            "<T extends PageObject> T getTest(String scopeArg, Class<T> pageObjectType)"));
  }

  @Test
  public void testPrivateContainerList() {
    final String methodName = getElementGetterMethodName(ELEMENT_NAME, true);
    PageObjectMethod method = getContainerMethod("containerList");
    MethodInfo expected = new MethodInfo(methodName, "List<T>");
    expected.addParameter(new MethodParameterInfo("scopeArg"));
    expected.addParameter(new MethodParameterInfo("selectorArg"));
    expected.addParameter(FIRST_CONTAINER_PARAMETER);
    expected.addImportedTypes(PAGE_OBJECT.getFullName(), List.class.getName());
    expected.addImpliedImportedTypes(
        PAGE_OBJECT.getFullName(),
        List.class.getName(),
        BASIC_ELEMENT.getFullName(),
        SELECTOR.getFullName());
    expected.addCodeLine("BasicElement scope = this.getScopeElement(scopeArg)");
    expected.addCodeLine(
        "LocatorBy testLocator = LocatorBy.byCss(String.format(\".css%s\", selectorArg))");
    expected.addCodeLine(
        "return this.container(scope, false).loadList(pageObjectType, testLocator)");
    PageObjectValidationTestHelper.validateMethod(method, expected);
    assertThat(
        method.getDeclaration().getCodeLine(),
        CoreMatchers.is(
            "<T extends PageObject> List<T> getTest(String scopeArg, String selectorArg, Class<T>"
                + " pageObjectType)"));
  }

  @Test
  public void testGeneratedScopeGetterForContainer() {
    TranslationContext context = compileNestedElements();

    // check that field for container scope was added
    final String SCOPE_ELEMENT_NAME = "rootContainerScope";
    assertThat(context.getFields(), hasSize(3));
    FieldInfo createdFieldInfo = new FieldInfo(SCOPE_ELEMENT_NAME);
    createdFieldInfo.addAnnotations("@ElementMarker.Find(css = \"slot\")");
    createdFieldInfo.validateField(findField(context, SCOPE_ELEMENT_NAME));

    // check generated method
    MethodInfo expectedGetter = new MethodInfo("getRootContainerScopeElement", "BasicElement");
    expectedGetter.setNotPublic();
    expectedGetter.addCodeLine("BasicElement root = this.getRootElement()");
    expectedGetter.addCodeLine(
        "return basic(root, this.rootContainerScope).build(BasicElement.class,"
            + " BasePageElement.class)");
    expectedGetter.addImportedTypes(BASIC_ELEMENT.getFullName());
    expectedGetter.addImpliedImportedTypes(
        BASIC_ELEMENT_IMPL_CLASS.getFullName(), BASIC_ELEMENT.getFullName());
    PageObjectMethod actualMethod = context.getMethod("getRootContainerScopeElement");
    validateMethod(actualMethod, expectedGetter);
  }

  @Test
  public void testNestedBasicElement() {
    TranslationContext context = compileNestedElements();
    ElementContext nestedBasic = context.getElement("nestedBasic");
    assertThat(nestedBasic.getType().getFullName(), is(BASIC_ELEMENT.getFullName()));
    MethodInfo expectedGetter = new MethodInfo("getNestedBasic", "BasicElement");
    expectedGetter.addCodeLine(
        "BasicElement rootContainerScope = this.getRootContainerScopeElement()");
    expectedGetter.addCodeLine(
        "return basic(rootContainerScope, this.nestedBasic).build(BasicElement.class,"
            + " BasePageElement.class)");
    PageObjectMethod actualMethod = nestedBasic.getElementMethod();
    validateMethod(actualMethod, expectedGetter);
  }

  @Test
  public void testNestedCustomElement() {
    TranslationContext context = compileNestedElements();
    ElementContext nestedCustomList = context.getElement("nestedCustom");
    assertThat(nestedCustomList.getType().getSimpleName(), is(equalTo("Component")));
    MethodInfo expectedGetter = new MethodInfo("getNestedCustom", "List<Component>");
    expectedGetter.addCodeLine(
        "BasicElement rootContainerScope = this.getRootContainerScopeElement()");
    expectedGetter.addCodeLine(
        "return custom(rootContainerScope, this.nestedCustom).buildList(Component.class)");
    validateMethod(nestedCustomList.getElementMethod(), expectedGetter);
  }

  @Test
  public void testNestedContainerElement() {
    TranslationContext context = compileNestedElements();
    ElementContext nestedCustomList = context.getElement("nestedContainer");
    assertThat(nestedCustomList.getType().getSimpleName(), is(equalTo("ContainerElement")));
    MethodInfo expectedGetter = new MethodInfo("getNestedContainer", "T");
    expectedGetter.addParameter(new MethodParameterInfo("pageObjectType", "Class<T>"));
    expectedGetter.addCodeLine(
        "BasicElement rootContainerScope = this.getRootContainerScopeElement()");
    expectedGetter.addCodeLine(
        "LocatorBy nestedContainerLocator = LocatorBy.byCss(\".nestedContainer\")");
    expectedGetter.addCodeLine(
        "return this.container(rootContainerScope, false).load(pageObjectType,"
            + " nestedContainerLocator)");
    expectedGetter.addImportedTypes(PAGE_OBJECT.getFullName());
    expectedGetter.addImpliedImportedTypes(
        PAGE_OBJECT.getFullName(), SELECTOR.getFullName(), BASIC_ELEMENT.getFullName());
    validateMethod(nestedCustomList.getElementMethod(), expectedGetter);
  }

  @Test
  public void testElementNestedInsideList() {
    TranslationContext context =
        new DeserializerUtilities().getContext("nestedlist/nestedContainerList");
    final String ELEMENT_METHOD_NAME = "getNestedBasic";
    PageObjectMethod nestedGetter = context.getMethod(ELEMENT_METHOD_NAME);
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, BASIC_ELEMENT.getSimpleName());
    expected.addParameter(
        new MethodParameterInfo("_containerListScopeIndex", NUMBER.getSimpleName()));
    expected.addCodeLine(
        "BasicElement containerListScope ="
            + " this._index_getContainerListScopeElement(_containerListScopeIndex)");
    expected.addCodeLine(
        "return basic(containerListScope, this.nestedBasic).build(BasicElement.class,"
            + " BasePageElement.class)");
    PageObjectValidationTestHelper.validateMethod(nestedGetter, expected);
  }
}
