/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import utam.compiler.representation.ElementField;
import utam.compiler.representation.ElementMethod;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.FieldInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.declarative.representation.*;
import utam.core.declarative.translator.TranslationTypesConfig;
import utam.compiler.translator.TranslationTypesConfigJava;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.helpers.AnnotationUtils.EMPTY_ANNOTATION;
import static utam.compiler.helpers.TranslationContext.*;
import static utam.compiler.helpers.TypeUtilities.Element.editable;
import static utam.compiler.helpers.ValidationTests.ELEMENT_SELECTOR;
import static utam.compiler.helpers.ValidationTests.ELEMENT_TYPE;
import static utam.compiler.translator.AbstractTranslatorConfiguration.ERR_PROFILE_NOT_CONFIGURED;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;

public class TranslationContextTests {

  private static final String TEST_ELEMENT_NAME = "testElement";

  private static ElementContext getElementContext() {
    return getElementContext(TEST_ELEMENT_NAME, "css");
  }

  private static ElementContext getElementContext(String elementName, String selector) {
    return new ElementContext.Basic(
        elementName, TypeUtilities.Element.clickable, getCssSelector(selector));
  }

  private static TranslationContext getContainerContext() {
    TranslationContext context = getTestTranslationContext();
    ElementContext elementContext = getElementContext();
    context.setElement(elementContext);
    context.setClassField(
        new ElementField(
            "testField",
            EMPTY_ANNOTATION));
    context.setMethod(new ElementMethod.Single(elementContext, true));
    context.setElement(new ElementContext.Container("containerObject"));
    return context;
  }

  private static TranslationTypesConfig getTypesProvider() {
    return new TranslationTypesConfigJava();
  }

  private static MethodParameter getStringParameter() {
    return new ParameterUtils.Regular("text", PrimitiveType.STRING);
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
    assertThat(
        getContainerContext()
            .getInterfaceType("utam-test/pageObjects/test/testInterface")
            .getFullName(),
        is(equalTo("utam.test.pageobjects.test.TestInterface")));
  }

  @Test
  public void testGetInterfaceTypeWithNullInterfaceTypeName() {
    assertThat(
        getContainerContext().getInterfaceType(null).getFullName(),
        is(equalTo("utam.test.pageobjects.test.Test")));
  }

  @Test
  public void testGetMethods() {
    MethodInfo methodInfo = new MethodInfo("getTestElement", "Clickable");
    methodInfo.addCodeLine("element(this.testElement).build(Clickable.class)");

    PageObjectValidationTestHelper.validateMethods(
        "translator context",
        new ArrayList<>(getContainerContext().getMethods()),
        Collections.singletonList(methodInfo));
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
        new ElementContext.Basic(name, ELEMENT_TYPE, getCssSelector(ELEMENT_SELECTOR));
    context.setElement(element);
    UtamError e = expectThrows(UtamError.class, () -> context.setElement(element));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_CONTEXT_DUPLICATE_ELEMENT_NAME, name)));
  }

  @Test
  public void testSetPublicMethod() {
    TranslationContext context = getContainerContext();
    ElementContext elementContext =
        new ElementContext.Basic(
            null,
            "parameterElement",
            TypeUtilities.Element.clickable,
            getCssSelector("a[title=*'%s']"),
            false,
            Collections.singletonList(getStringParameter()));

    int currentSize = context.getMethods().size();
    context.setMethod(new ElementMethod.Single(elementContext, true));
    assertThat(context.getMethods(), hasSize(currentSize + 1));
  }

  @Test
  public void testSetPublicMethodWithMultipleParameters() {
    TranslationContext context = getContainerContext();
    ElementContext elementContext =
        new ElementContext.Basic(
            null,
            "parameterElement",
            TypeUtilities.Element.clickable,
            getCssSelector("a[title=*'%s'][alt=*'%s]"),
            false,
            Arrays.asList(
                getStringParameter(), new ParameterUtils.Regular("alt", PrimitiveType.STRING)));

    int currentSize = context.getMethods().size();
    context.setMethod(new ElementMethod.Single(elementContext, true));
    assertThat(context.getMethods(), hasSize(currentSize + 1));
  }

  @Test
  public void testSetPublicMethodWithDuplicateParametersThrows() {
    TranslationContext context = getContainerContext();
    ElementContext elementContext =
        new ElementContext.Basic(
            null,
            "parameterElement",
            TypeUtilities.Element.clickable,
            getCssSelector("a[title=*'%s']"),
            false,
            Arrays.asList(getStringParameter(), getStringParameter()));

    UtamError e =
        expectThrows(
            UtamError.class,
            () -> context.setMethod(new ElementMethod.Single(elementContext, true)));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(ERR_CONTEXT_DUPLICATE_PARAMETERS, "text", "getParameterElement")));
  }

  @Test
  public void setPublicMethodWithDuplicateMethodThrows() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                getContainerContext()
                    .setMethod(new ElementMethod.Single(getElementContext(), true)));
    assertThat(e.getMessage(), containsString("duplicate method 'getTestElement'"));
  }

  @Test
  public void nonExistingProfile() {
    TranslationContext translationInstantContext = getTestTranslationContext();
    UtamError e =
        expectThrows(
            UtamError.class, () -> translationInstantContext.getProfile("driver", "chrome"));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_PROFILE_NOT_CONFIGURED, "driver"))));
  }

  @Test
  public void setMethodWithDuplicateArguments() {
    TranslationContext context = getTestTranslationContext();
    MethodParameter parameter = getStringParameter();

    MethodDeclaration declaration = mock(MethodDeclaration.class);
    when(declaration.getName()).thenReturn("getError");
    when(declaration.getParameters())
        .thenReturn(Stream.of(parameter, parameter).collect(Collectors.toList()));

    PageObjectMethod method = mock(PageObjectMethod.class);
    when(method.getDeclaration()).thenReturn(declaration);

    UtamError e = expectThrows(UtamError.class, () -> context.setMethod(method));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_CONTEXT_DUPLICATE_PARAMETERS, "text", "getError")));
  }

  @Test
  public void testMethodWithDuplicateNames() {
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
    ElementContext defaultRoot = new ElementContext.Root(editable);
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
}
