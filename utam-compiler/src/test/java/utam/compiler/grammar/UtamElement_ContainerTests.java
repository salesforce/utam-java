/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import org.openqa.selenium.By;
import utam.compiler.grammar.UtamElement.Type;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import static utam.compiler.grammar.UtamElement.ERR_CONTAINER_SHOULD_BE_PUBLIC;
import static utam.compiler.grammar.UtamSelectorTests.getUtamCssSelector;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_LIST_RETURN_TYPE;
import static utam.compiler.representation.ContainerMethodTests.FIRST_CONTAINER_PARAMETER;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamElement_ContainerTests {

  private static final String ELEMENT_NAME = "test";

  private static UtamElement getPublicContainerElement() {
    UtamElement utamElement = TestUtilities.UtamEntityCreator.createUtamElement(ELEMENT_NAME);
    utamElement.type = new String[] {"container"};
    utamElement.isPublic = true;
    return utamElement;
  }

  private static String getContainerSupportedProperties() {
    return Type.CONTAINER.getSupportedPropertiesErr(ELEMENT_NAME);
  }

  @Test
  public void testPrivateContainer() {
    UtamElement element = getPublicContainerElement();
    element.selector = getUtamCssSelector();
    element.isPublic = false;
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(
        e.getMessage(), is(equalTo(String.format(ERR_CONTAINER_SHOULD_BE_PUBLIC, ELEMENT_NAME))));
  }

  @Test
  public void testNotAllowedFilter() {
    UtamElement element = getPublicContainerElement();
    element.filter = UtamElementFilter_Tests.getInnerTextFilter();
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(getContainerSupportedProperties())));
  }

  @Test
  public void testNotAllowedShadow() {
    UtamElement element = getPublicContainerElement();
    element.shadow = new UtamShadowElement(new UtamElement[] {});
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(getContainerSupportedProperties())));
  }

  @Test
  public void testNotAllowedNestedElements() {
    UtamElement element = getPublicContainerElement();
    element.elements = new UtamElement[] {};
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(getContainerSupportedProperties())));
  }

  @Test
  public void testNotAllowedExternal() {
    UtamElement element = getPublicContainerElement();
    element.isExternal = true;
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(getContainerSupportedProperties())));
  }

  @Test
  public void testNotAllowedLoad() {
    UtamElement element = getPublicContainerElement();
    element.isNullable = false;
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(getContainerSupportedProperties())));
  }

  @Test
  public void testPublicAbstraction() {
    UtamElement element = getPublicContainerElement();
    assertThat(element.getAbstraction(), is(instanceOf(UtamElement.Container.class)));
    assertThat(
        element.selector.getLocator().getValue(),
        is(equalTo(By.cssSelector(UtamElement.Container.DEFAULT_CONTAINER_SELECTOR_CSS))));
  }

  /** The getDeclaredMethod method should return null for a non-public container */
  @Test
  public void testGetDeclaredMethodWithNoSelector() {
    TranslationContext context = new DeserializerUtilities().getContext("containerElement");
    ElementContext element = context.getElement("containerInsideRoot");
    PageObjectMethod method = element.getElementMethod();
    assertThat(element.getType().getSimpleName(), is(equalTo("ContainerElement")));
    PageObjectValidationTestHelper.MethodInfo expectedMethod =
        new PageObjectValidationTestHelper.MethodInfo(
            "getContainerInsideRoot", "<T extends PageObject> T");
    expectedMethod.addParameter(FIRST_CONTAINER_PARAMETER);
    expectedMethod.addCodeLines(
        "this.inContainer(this.root, true)"
            + ".load(pageObjectType, LocatorBy.byCss(\":scope > *:first-child\"))");
    PageObjectValidationTestHelper.validateMethod(method, expectedMethod);
  }

  /** The getDeclaredMethods method should return the proper value for a container */
  @Test
  public void testNestedContainerElement() {
    TranslationContext context = new DeserializerUtilities().getContext("containerElement");
    ElementContext element = context.getElement("nestedContainer");
    assertThat(element.isList(), is(false));
    PageObjectMethod method = element.getElementMethod();
    assertThat(element.getType().getSimpleName(), is(equalTo("ContainerElement")));
    PageObjectValidationTestHelper.MethodInfo expectedMethod =
        new PageObjectValidationTestHelper.MethodInfo(
            "getNestedContainer", "<T extends PageObject> T");
    expectedMethod.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("scopeArg", "String"));
    expectedMethod.addParameter(FIRST_CONTAINER_PARAMETER);

    expectedMethod.addCodeLines(
        "this.inContainer(this.scope.setParameters(scopeArg), true)"
            + ".load(pageObjectType, LocatorBy.byCss(\":scope > *:first-child\"))");
    PageObjectValidationTestHelper.validateMethod(method, expectedMethod);
  }

  @Test
  public void testContainerWithSelector() {
    TranslationContext context = new DeserializerUtilities().getContext("containerElement");
    ElementContext element = context.getElement("containerWithSelector");
    PageObjectMethod method = element.getElementMethod();
    assertThat(element.getType().getSimpleName(), is(equalTo("ContainerElement")));
    PageObjectValidationTestHelper.MethodInfo expectedMethod =
        new PageObjectValidationTestHelper.MethodInfo(
            "getContainerWithSelector", CONTAINER_LIST_RETURN_TYPE.getSimpleName());
    expectedMethod.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("scopeArg", "String"));
    expectedMethod.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("selectorArg", "String"));
    expectedMethod.addParameter(FIRST_CONTAINER_PARAMETER);
    expectedMethod.addCodeLines(
        "this.inContainer(this.scope.setParameters(scopeArg), false)"
            + ".loadList(pageObjectType, LocatorBy.byCss(String.format(\".css%s\", selectorArg)))");
    PageObjectValidationTestHelper.validateMethod(method, expectedMethod);
  }
}
