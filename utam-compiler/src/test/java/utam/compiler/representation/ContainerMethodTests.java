/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static utam.compiler.grammar.UtamElement.DEFAULT_CONTAINER_SELECTOR_CSS;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.representation.ContainerMethod.PAGE_OBJECT_PARAMETER;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;

/**
 * Test generated container methods text
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public class ContainerMethodTests {

  private static final MethodParameterInfo FIRST_CONTAINER_PARAMETER =
      new PageObjectValidationTestHelper.MethodParameterInfo(
          PAGE_OBJECT_PARAMETER.getValue(), PAGE_OBJECT_PARAMETER.getType().getSimpleName());

  private static final String ELEMENT_NAME = "test";

  private static PageObjectMethod getContainerMethod(String fileName) {
    final String methodName = getElementGetterMethodName(ELEMENT_NAME, true);
    TranslationContext context = new DeserializerUtilities().getContext("element/" + fileName);
    ElementContext element = context.getElement(ELEMENT_NAME);
    assertThat(element.getType().isSameType(CONTAINER_ELEMENT), is(true));
    return context.getMethod(methodName);
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
        is(
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
        is("<T extends PageObject> T getTest(String scopeArg, Class<T> pageObjectType)"));
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
        is(
            "<T extends PageObject> List<T> getTest(String scopeArg, String selectorArg, Class<T>"
                + " pageObjectType)"));
  }
}
