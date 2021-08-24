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
import static org.hamcrest.Matchers.instanceOf;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.UtamElement.Container.DEFAULT_CONTAINER_SELECTOR_CSS;
import static utam.compiler.grammar.UtamElement.Type.CONTAINER;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_ELEMENT;
import static utam.compiler.representation.ContainerMethod.PAGE_OBJECT_PARAMETER;
import static utam.compiler.representation.PageObjectValidationTestHelper.validateMethod;

import java.util.List;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamElement_ContainerTests {

  private TranslationContext context;
  private List<UtamElement> containers;
  private static final MethodParameterInfo FIRST_CONTAINER_PARAMETER =
      new PageObjectValidationTestHelper.MethodParameterInfo(
          PAGE_OBJECT_PARAMETER.getValue(), PAGE_OBJECT_PARAMETER.getType().getSimpleName());

  @BeforeClass
  private void prepareData() {
    context = new DeserializerUtilities().getContext("element/testContainer");
    containers = DeserializerUtilities
        .getDeserializedObjects(UtamElement.class, "element/containers");
  }

  @Test
  public void testNotAllowedFilter() {
    UtamElement element = containers.get(0);
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(CONTAINER.getSupportedPropertiesErr("filterThrows"))));
  }

  @Test
  public void testNotAllowedShadow() {
    UtamElement element = containers.get(1);
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), is(CONTAINER.getSupportedPropertiesErr("shadowThrows")));
  }

  @Test
  public void testNotAllowedNestedElements() {
    UtamElement element = containers.get(2);
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), is(CONTAINER.getSupportedPropertiesErr("elementsThrows")));
  }

  @Test
  public void testNotAllowedExternal() {
    UtamElement element = containers.get(3);
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), is(CONTAINER.getSupportedPropertiesErr("externalThrows")));
  }

  @Test
  public void testNotAllowedLoad() {
    UtamElement element = containers.get(4);
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), is(CONTAINER.getSupportedPropertiesErr("nullableThrows")));
  }

  @Test
  public void testPrivateWithoutSelector() {
    UtamElement element = containers.get(5);
    assertThat(element.getAbstraction(), is(instanceOf(UtamElement.Container.class)));
    assertThat(element.getLocatorString(), is(equalTo(DEFAULT_CONTAINER_SELECTOR_CSS)));
  }

  @Test
  public void testPublicWithSelector() {
    UtamElement element = containers.get(6);
    assertThat(element.getAbstraction(), is(instanceOf(UtamElement.Container.class)));
    assertThat(element.getLocatorString(), is(equalTo("injected")));
  }

  @Test
  public void testGetDeclaredMethodWithNoSelector() {
    ElementContext element = context.getElement("containerInsideRoot");
    PageObjectMethod method = element.getElementMethod();
    assertThat(element.getType().isSameType(CONTAINER_ELEMENT), is(true));
    MethodInfo expectedMethod = new MethodInfo(
        "getContainerInsideRoot", "PageObject");
    expectedMethod.addParameter(FIRST_CONTAINER_PARAMETER);
    expectedMethod.addCodeLines(
        "this.inContainer(this.root, true)"
            + ".load(pageObjectType, LocatorBy.byCss(\":scope > *:first-child\"))");
    validateMethod(method, expectedMethod);
  }

  @Test
  public void testNestedContainerElement() {
    ElementContext element = context.getElement("nestedContainer");
    assertThat(element.isList(), is(false));
    PageObjectMethod method = element.getElementMethod();
    assertThat(element.getType().isSameType(CONTAINER_ELEMENT), is(true));
    MethodInfo expectedMethod = new MethodInfo("getNestedContainer", "PageObject");
    expectedMethod.addParameter(new MethodParameterInfo("scopeArg", "String"));
    expectedMethod.addParameter(FIRST_CONTAINER_PARAMETER);
    expectedMethod.addCodeLines(
        "this.inContainer(this.scope.setParameters(scopeArg), true)"
            + ".load(pageObjectType, LocatorBy.byCss(\":scope > *:first-child\"))");
    validateMethod(method, expectedMethod);
  }

  @Test
  public void testContainerWithSelector() {
    ElementContext element = context.getElement("containerWithSelector");
    PageObjectMethod method = element.getElementMethod();
    assertThat(element.getType().isSameType(CONTAINER_ELEMENT), is(true));
    MethodInfo expectedMethod = new MethodInfo("getContainerWithSelector",
        "List<PageObject>");
    expectedMethod.addParameter(new MethodParameterInfo("scopeArg", "String"));
    expectedMethod.addParameter(new MethodParameterInfo("selectorArg", "String"));
    expectedMethod.addParameter(FIRST_CONTAINER_PARAMETER);
    expectedMethod.addCodeLines(
        "this.inContainer(this.scope.setParameters(scopeArg), false)"
            + ".loadList(pageObjectType, LocatorBy.byCss(String.format(\".css%s\", selectorArg)))");
    validateMethod(method, expectedMethod);
  }
}
