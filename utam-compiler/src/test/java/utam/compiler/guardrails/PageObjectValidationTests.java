/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.guardrails;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.helpers.ElementContext.Self.SELF_ELEMENT;
import static utam.compiler.types.BasicElementInterface.actionable;

import java.util.ArrayList;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TypeUtilities;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.translator.GuardrailsMode;
import utam.core.element.Locator;
import utam.core.framework.consumer.UtamError;

/**
 * @author elizaveta.ivanova
 * @since 236
 */
public class PageObjectValidationTests {

  private static final String ELEMENT_NAME = "fakeElementName";
  private static final String SELECTOR_VALUE = ".fakeSelector";
  private static final String PAGE_OBJECT = "test";
  private static final TypeProvider DUMMY_TYPE = new TypeUtilities.FromString("test.FakeElementType");

  private static void validatePageObject(ElementContext... elements) {
    PageObjectValidation validation = new PageObjectValidation(GuardrailsMode.ERROR, PAGE_OBJECT,
        Stream.of(elements).collect(Collectors.toList()));
    validation.validate();
  }

  @Test
  public void testHardcodedTextInSelectorThrows() {
    ElementContext customElement = new ElementContext.Custom(
        ELEMENT_NAME, DUMMY_TYPE, getCssSelector(SELECTOR_VALUE + "[title='fakeTitle']"));
    PageObjectValidation validation = new PageObjectValidation(GuardrailsMode.ERROR, PAGE_OBJECT,
        Stream.of(customElement).collect(Collectors.toList()));
    UtamError e = expectThrows(UtamError.class, () -> validation.validateSelector(customElement));
    assertThat(e.getMessage(),
        is(equalTo(validation.getHardcodedTextInSelectorError(customElement))));

    ElementContext basicElement = new ElementContext.Basic(
        ELEMENT_NAME, DUMMY_TYPE, getCssSelector(SELECTOR_VALUE + "[title='fakeTitle']"));

    expectThrows(UtamError.class, () -> validatePageObject(basicElement));

    ElementContext rootElement = new ElementContext.Root(
        DUMMY_TYPE, getCssSelector(SELECTOR_VALUE + "[title='fakeTitle']"), actionable, mock(
        PageObjectMethod.class));

    expectThrows(UtamError.class, () -> validatePageObject(rootElement));
  }

  @Test
  public void testHardcodedTextInSelectorWarning() {
    ElementContext customElement = new ElementContext.Custom(
        ELEMENT_NAME, actionable, getCssSelector(SELECTOR_VALUE + "[title='fakeTitle']"));
    PageObjectValidation validation = new PageObjectValidation(GuardrailsMode.WARNING, PAGE_OBJECT, new ArrayList<>());
    // no error is thrown
    validation.validateSelector(customElement);
  }

  @Test
  public void testValidateNoError() {
    ElementContext elementContext = SELF_ELEMENT;
    validatePageObject(elementContext, elementContext);
  }

  @Test
  public void testValidateThrowIfConfiguredWithErrorMode() {
    Locator selector = getCssSelector("css");
    ElementContext customElement =
        new ElementContext.Custom("name1", new TypeUtilities.FromString("test.Type"), selector);
    ElementContext basicElement = new ElementContext.Basic("name2", actionable, selector);
    PageObjectValidation validation = new PageObjectValidation(GuardrailsMode.ERROR, PAGE_OBJECT,
        Stream.of(basicElement, customElement).collect(Collectors.toList()));

    UtamError e = expectThrows(UtamError.class, validation::validate);
    assertThat(e.getMessage(), startsWith(validation.getErrorPrefix(basicElement, customElement)));
  }

  @Test
  public void testValidateWarningIfConfiguredWithWarningMode() {
    Locator selector = getCssSelector("css");
    ElementContext customElement =
        new ElementContext.Custom("name1", new TypeUtilities.FromString("test.Type"), selector);
    ElementContext basicElement = new ElementContext.Basic("name2", actionable, selector);
    PageObjectValidation validation = new PageObjectValidation(GuardrailsMode.WARNING, PAGE_OBJECT,
        Stream.of(basicElement, customElement).collect(Collectors.toList()));
    // warning in console
    validation.validate();
  }
}
