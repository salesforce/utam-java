/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.helpers.AnnotationUtils.EMPTY_ANNOTATION;
import static utam.compiler.types.BasicElementInterface.actionable;
import static utam.compiler.types.BasicElementInterface.editable;
import static utam.compiler.helpers.TranslationContext.ERR_CONTEXT_DUPLICATE_ELEMENT_NAME;
import static utam.compiler.helpers.TranslationContext.ERR_CONTEXT_DUPLICATE_FIELD;
import static utam.compiler.helpers.TranslationContext.ERR_CONTEXT_DUPLICATE_METHOD;
import static utam.compiler.helpers.TranslationContext.ERR_CONTEXT_ELEMENT_NOT_FOUND;
import static utam.compiler.helpers.TranslationContext.ERR_PROFILE_NOT_CONFIGURED;

import org.testng.annotations.Test;
import utam.compiler.representation.ElementField;
import utam.compiler.representation.ElementMethod;
import utam.compiler.representation.PageObjectValidationTestHelper.FieldInfo;
import utam.compiler.types.BasicElementInterface;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageClassField;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;

public class TranslationContextTests {

  private static final String TEST_ELEMENT_NAME = "testElement";

  private static ElementContext getElementContext() {
    return getElementContext(TEST_ELEMENT_NAME, "css");
  }

  private static ElementContext getElementContext(String elementName, String selector) {
    return new ElementContext.Basic(
        elementName, BasicElementInterface.clickable, getCssSelector(selector));
  }

  private static TranslationContext getContainerContext() {
    TranslationContext context = getTestTranslationContext();
    ElementContext elementContext = getElementContext();
    context.setElement(elementContext);
    context.setClassField(
        new ElementField(
            "testField",
            EMPTY_ANNOTATION));
    context.setMethod(new ElementMethod.Single(elementContext, true, TypeUtilities.BASIC_ELEMENT_IMPL_CLASS));
    context.setElement(new ElementContext.Container(null, "containerObject"));
    return context;
  }

  @Test
  public void testGetClassType() {
    assertThat(
        getContainerContext().getClassType().getFullName(),
        is(equalTo("utam.test.pageobjects.test.impl.TestImpl")));
  }

  @Test
  public void getDeclaredApiTest() {
    assertThat(getContainerContext().getMethods(), hasSize(1));
  }

  @Test
  public void testGetElement() {
    assertThat(
        getContainerContext().getElement(TEST_ELEMENT_NAME).getType().getSimpleName(),
        is(equalTo("Clickable")));
  }

  @Test
  public void testGetElementErr() {
    UtamError e = expectThrows(UtamError.class, () -> getContainerContext().getElement("error"));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_CONTEXT_ELEMENT_NOT_FOUND, "error"))));
  }

  @Test
  public void testGetElementWithMissingElementThrows() {
    UtamError e =
        expectThrows(UtamError.class, () -> getContainerContext().getElement("doesNotExist"));
    assertThat(e.getMessage(), containsString("referenced element 'doesNotExist' not found"));
  }

  @Test
  public void testGetFields() {
    FieldInfo fieldInfo = new FieldInfo("testField");
    TranslationContext context = getContainerContext();
    assertThat(context.getFields(), hasSize(1));
    fieldInfo.validateField(context.getFields().get(0));
  }

  @Test
  public void testGetInterfaceType() {
    TranslationContext context = getContainerContext();
    context.setImplementedType("utam-test/pageObjects/test/testInterface");
    assertThat(context.getSelfType().getFullName(),
        is(equalTo("utam.test.pageobjects.test.TestInterface")));
  }

  @Test
  public void testGetInterfaceTypeWithNullInterfaceTypeName() {
    assertThat(
        getContainerContext().getSelfType().getFullName(),
        is(equalTo("utam.test.pageobjects.test.Test")));
  }

  @Test
  public void testGetType() {
    assertThat(
        getContainerContext().getType("utam-test/pageObjects/test/testType").getFullName(),
        is(equalTo("utam.test.pageobjects.test.TestType")));
  }

  @Test
  public void testGetUtilityType() {
    assertThat(
        getContainerContext().getUtilityType("utam-test/utils/test/test").getFullName(),
        is(equalTo("utam.test.utils.test.Test")));
  }

  @Test
  public void testSetElement() {
    TranslationContext context = getContainerContext();
    context.setElement(getElementContext("newElement", "newCss"));
    assertThat(context.getElement("newElement"), is(not(nullValue())));
  }

  @Test
  public void testSetElementWithDuplicateElementThrows() {
    TranslationContext context = getContainerContext();
    RuntimeException e =
        expectThrows(RuntimeException.class, () -> context.setElement(getElementContext()));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_CONTEXT_DUPLICATE_ELEMENT_NAME, "testElement")));
  }

  @Test
  public void testDuplicateElementName() {
    final String name = "fakeElementName";
    TranslationContext context = getTestTranslationContext();
    ElementContext element =
        new ElementContext.Basic(name, actionable, getCssSelector("css"));
    context.setElement(element);
    UtamError e = expectThrows(UtamError.class, () -> context.setElement(element));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_CONTEXT_DUPLICATE_ELEMENT_NAME, name)));
  }

  @Test
  public void testMethodWithDuplicateNamesThrows() {
    PageObjectMethod method = mock(PageObjectMethod.class);
    MethodDeclaration declaration = mock(MethodDeclaration.class);
    when(declaration.getName()).thenReturn("name");
    when(method.getDeclaration()).thenReturn(declaration);
    TranslationContext context = getTestTranslationContext();
    context.setMethod(method);
    assertThat(context.getMethods(), hasSize(1));
    UtamError e = expectThrows(UtamError.class, () -> context.setMethod(method));
    assertThat(e.getMessage(), containsString(String.format(ERR_CONTEXT_DUPLICATE_METHOD, "name")));
  }

  @Test
  public void testGetRootElement() {
    TranslationContext context = getTestTranslationContext();
    ElementContext defaultRoot = new ElementContext.Root(editable, null, editable);
    context.setElement(defaultRoot);
    assertThat(context.getRootElement().getName(), is(equalTo(ElementContext.ROOT_ELEMENT_NAME)));
  }

  @Test
  public void testDuplicateNames() {
    PageClassField field = mock(PageClassField.class);
    when(field.getName()).thenReturn("name");
    TranslationContext context = getTestTranslationContext();
    context.setClassField(field);
    assertThat(context.getFields(), hasSize(1));
    UtamError e = expectThrows(UtamError.class, () -> context.setClassField(field));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_CONTEXT_DUPLICATE_FIELD, field.getName())));
  }

  @Test
  public void testGetAllElements() {
    TranslationContext context = getTestTranslationContext();
    context.setTestableElement("name", mock(ElementUnitTestHelper.class));
    assertThat(context.getTestableElements().get("name"), is(notNullValue()));
  }

  @Test
  public void nonExistingProfile() {
    TranslationContext translationInstantContext = getTestTranslationContext();
    UtamError e =
        expectThrows(
            UtamError.class, () -> translationInstantContext.getProfile("driver", "chrome"));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_PROFILE_NOT_CONFIGURED, "driver"))));
  }
}
