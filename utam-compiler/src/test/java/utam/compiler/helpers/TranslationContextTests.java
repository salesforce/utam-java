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
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.helpers.TranslationContext.ERR_CONTEXT_DUPLICATE_ELEMENT_NAME;
import static utam.compiler.helpers.TranslationContext.ERR_CONTEXT_DUPLICATE_FIELD;
import static utam.compiler.helpers.TranslationContext.ERR_CONTEXT_DUPLICATE_METHOD;
import static utam.compiler.helpers.TranslationContext.ERR_CONTEXT_ELEMENT_NOT_FOUND;
import static utam.compiler.helpers.TranslationContext.ERR_PROFILE_NOT_CONFIGURED;
import static utam.compiler.types.BasicElementInterface.editable;

import org.hamcrest.CoreMatchers;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageClassField;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.LocatorBy;

public class TranslationContextTests {

  private static final String TEST_ELEMENT_NAME = "test";

  private final TranslationContext context = new DeserializerUtilities()
      .getContext("pageobjects/testContext");

  @Test
  public void testGetClassType() {
    assertThat(context.getClassType().getFullName(),
        is(equalTo("utam.test.pageobjects.test.impl.TestImpl")));
  }

  @Test
  public void testGetMethods() {
    assertThat(context.getMethods(), hasSize(1));
  }

  @Test
  public void testGetElement() {
    assertThat(context.getElement(TEST_ELEMENT_NAME), is(CoreMatchers.notNullValue()));
  }

  @Test
  public void testGetElementWithMissingElementThrows() {
    UtamError e = expectThrows(UtamError.class, () -> context.getElement("error"));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_CONTEXT_ELEMENT_NOT_FOUND, "error"))));
  }

  @Test
  public void testGetFields() {
    assertThat(context.getFields(), hasSize(0));
  }

  @Test
  public void testGetInterfaceType() {
    TranslationContext context = getTestTranslationContext();
    context.setImplementedType("utam-test/pageObjects/test/testInterface");
    assertThat(context.getSelfType().getFullName(),
        is(equalTo("utam.test.pageobjects.test.TestInterface")));
  }

  @Test
  public void testGetSelfType() {
    TranslationContext context = getTestTranslationContext();
    assertThat(context.getSelfType().getFullName(),
        is(equalTo("utam.test.pageobjects.test.Test")));
  }

  @Test
  public void testGetType() {
    TranslationContext context = getTestTranslationContext();
    assertThat(context.getType("utam-test/pageObjects/test/testType").getFullName(),
        is(equalTo("utam.test.pageobjects.test.TestType")));
  }

  @Test
  public void testGetUtilityType() {
    TranslationContext context = getTestTranslationContext();
    assertThat(context.getUtilityType("utam-test/utils/test/test").getFullName(),
        is(equalTo("utam.test.utils.test.Test")));
  }

  @Test
  public void testSetElement() {
    TranslationContext context = getTestTranslationContext();
    ElementContext elementContext = new ElementContext.Basic(TEST_ELEMENT_NAME,
        mock(TypeProvider.class), mock(LocatorBy.class));
    context.setElement(elementContext);
    assertThat(context.getElement(TEST_ELEMENT_NAME), is(not(nullValue())));
    UtamError e = expectThrows(UtamError.class, () -> context.setElement(elementContext));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_CONTEXT_DUPLICATE_ELEMENT_NAME, TEST_ELEMENT_NAME)));
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
  public void testGetTestableElements() {
    TranslationContext context = getTestTranslationContext();
    context.setTestableElement("name", mock(ElementUnitTestHelper.class));
    assertThat(context.getTestableElements().get("name"), is(notNullValue()));
  }

  @Test
  public void testNonExistingProfileThrows() {
    TranslationContext translationInstantContext = getTestTranslationContext();
    UtamError e =
        expectThrows(
            UtamError.class, () -> translationInstantContext.getProfile("driver", "chrome"));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_PROFILE_NOT_CONFIGURED, "driver"))));
  }
}
