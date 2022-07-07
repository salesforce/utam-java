/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.guardrails;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;
import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.guardrails.ValidationError.COMPONENTS_WITH_SAME_SELECTOR_BUT_DIFFERENT_TYPES;
import static utam.compiler.guardrails.ValidationError.COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR;
import static utam.compiler.guardrails.ValidationError.DUPLICATE_WITH_ROOT_SELECTOR;
import static utam.compiler.guardrails.ValidationUtilities.getValidationError;
import static utam.compiler.guardrails.ValidationUtilities.hasHardcodedText;
import static utam.compiler.guardrails.ValidationUtilities.isSameSelector;
import static utam.compiler.types.BasicElementInterface.actionable;

import java.util.List;
import org.hamcrest.CoreMatchers;
import org.testng.annotations.Test;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementContext.Document;
import utam.compiler.helpers.ElementContext.Root;
import utam.compiler.helpers.ElementContext.Self;
import utam.compiler.helpers.TypeUtilities;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Locator;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 236
 */
public class ValidationUtilitiesTests {

  private static final String ELEMENT_SELECTOR = ".fakeSelector";
  private static final TypeProvider ELEMENT_TYPE = new TypeUtilities.FromString("test.FakeElementType");
  private static final String ELEMENT_NAME = "fakeElementName";

  private static ElementContext.Root getRoot(String type, Locator selector) {
    return new Root(new TypeUtilities.FromString(type), selector, actionable, mock(PageObjectMethod.class));
  }

  private static ElementContext.Root getEmptyRoot(String type) {
    return new Root(new TypeUtilities.FromString(type), LocatorBy.byCss(""), actionable, mock(PageObjectMethod.class));
  }

  @Test
  public void testIsSameSelector() {
    Locator TEST = getCssSelector(ELEMENT_SELECTOR);
    assertThat(isSameSelector(TEST, TEST), is(equalTo(true)));
    assertThat(isSameSelector(getCssSelector(""), TEST), is(equalTo(false)));
    assertThat(isSameSelector(TEST, getCssSelector("")), is(equalTo(false)));
    assertThat(isSameSelector(TEST, getCssSelector("xxxx")), is(equalTo(false)));
    assertThat(isSameSelector(getCssSelector("label"), getCssSelector("label")),
        is(equalTo(true)));
  }

  /**
   * Verifies isLabelHardcoded() method returns correct value for every selector type that can be
   * potentially hardcoded.
   */
  @Test
  public void testIsLabelHardcoded() {
    final List<String> HARDCODED_SELECTORS = List.of(
        "option[value='hardcoded']",
        "button[title='hardcoded']");
    final List<String> VALID_SELECTORS = List.of(
        "option[value='%s']",
        "button[title='%s']");
    HARDCODED_SELECTORS.forEach(selector ->
        assertThat(hasHardcodedText(getCssSelector(selector)), is(equalTo(true))));
    VALID_SELECTORS.forEach(selector ->
        assertThat(hasHardcodedText(getCssSelector(selector)), is(equalTo(false))));
  }

  @Test
  public void testValidateSelfElement() {
    ElementContext element = Self.SELF_ELEMENT;
    assertThat(getValidationError(element, null), CoreMatchers.is(nullValue()));
  }

  @Test
  public void testValidateDocumentElement() {
    ElementContext element = Document.DOCUMENT_ELEMENT;
    assertThat(getValidationError(element, null), CoreMatchers.is(nullValue()));
  }

  @Test
  public void testValidateRootWithEmptySelector() {
    ElementContext.Root root = getEmptyRoot("test.Type");
    // root w same enclosing type
    assertThat(getValidationError(root, getEmptyRoot("test.Type")), is(nullValue()));
    // root with different enclosing type
    assertThat(getValidationError(root, getEmptyRoot("test.AnotherType")), is(nullValue()));
  }

  @Test
  public void testValidateRootWithNonEmptySelector() {
    Locator selector = getCssSelector(ELEMENT_SELECTOR);
    ElementContext.Root root = getRoot("test.Type", selector);
    ElementContext second = getRoot("test.Type", selector);
    assertThat(getValidationError(root, second), is(nullValue()));
    assertThat(getValidationError(root, getRoot("test.AnotherType", selector)), is(equalTo(DUPLICATE_WITH_ROOT_SELECTOR)));
    assertThat(getValidationError(root, new ElementContext.Custom(
            "name", new TypeUtilities.FromString("test.Type"), selector)),
        is(nullValue()));
    assertThat(getValidationError(root, new ElementContext.Custom(
            "name", new TypeUtilities.FromString("test.WrongType"), selector)),
        is(equalTo(DUPLICATE_WITH_ROOT_SELECTOR)));
  }

  @Test
  public void testValidateCustomElement() {
    Locator selector = getCssSelector(ELEMENT_SELECTOR);
    ElementContext.Custom customElement =
        new ElementContext.Custom("name", new TypeUtilities.FromString("test.Type"), selector);

    assertThat(
        getValidationError(customElement, new ElementContext.Basic("name", actionable, selector)),
        is(equalTo(COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR)));

    assertThat(
        getValidationError(customElement,
            new ElementContext.Basic("name", actionable, getCssSelector("other"))),
        is(nullValue()));

    assertThat(getValidationError(customElement, getRoot("test.Type", selector)), is(nullValue()));

    assertThat(getValidationError(customElement, getRoot("test.AnotherType", selector)),
        is(equalTo(DUPLICATE_WITH_ROOT_SELECTOR)));

    assertThat(getValidationError(customElement, new ElementContext.Custom(
            "name", new TypeUtilities.FromString("test.Type"), selector)),
        is(nullValue()));
    assertThat(
        getValidationError(customElement,
            new ElementContext.Custom("name", new TypeUtilities.FromString("test.AnotherType"),
                selector)),
        is(equalTo(COMPONENTS_WITH_SAME_SELECTOR_BUT_DIFFERENT_TYPES)));
  }

  @Test
  public void testValidateCustomSameTypeDifferentSelectors() {
    TypeProvider elementType = ELEMENT_TYPE;
    ElementContext element =
        new ElementContext.Custom(
            "fakeElementName", elementType, getCssSelector(ELEMENT_SELECTOR));

    ElementContext otherElement =
        new ElementContext.Custom(
            "fakeOtherElementName", elementType, getCssSelector(".fakeDifferentSelector"));

    assertThat(getValidationError(element, otherElement), CoreMatchers.is(nullValue()));
  }

  @Test
  public void testValidateBasicElementWithRootSameSelector() {
    Locator selector = getCssSelector(ELEMENT_SELECTOR);
    ElementContext basicElement = new ElementContext.Basic("name", actionable, selector);
    assertThat(getValidationError(basicElement, getRoot("test.Type", selector)),
        is(equalTo(DUPLICATE_WITH_ROOT_SELECTOR)));
  }

  @Test
  public void testValidateBasicElementWithCustomSameSelector() {
    Locator selector = getCssSelector(ELEMENT_SELECTOR);
    ElementContext basicElement = new ElementContext.Basic("name", actionable, selector);
    assertThat(getValidationError(basicElement,
        new ElementContext.Custom(
            "name", new TypeUtilities.FromString("test.Type"), selector)),
        is(equalTo(COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR)));
  }

  @Test
  public void testValidateBasicListWithCustomHavingSameSelector() {
    TypeProvider elementType = ELEMENT_TYPE;
    ElementContext list =
        new ElementContext.BasicReturnsAll(elementType, getCssSelector(ELEMENT_SELECTOR));

    ElementContext otherElement =
        new ElementContext.Custom(
            ELEMENT_NAME, elementType, getCssSelector(ELEMENT_SELECTOR));

    assertThat(
        getValidationError(list, otherElement),
        CoreMatchers.is(CoreMatchers.equalTo(COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR)));
  }

  @Test
  public void testValidateBasicListWithBasicElementHavingSameSelector() {
    TypeProvider elementType = ELEMENT_TYPE;
    ElementContext list =
        new ElementContext.BasicReturnsAll(elementType, getCssSelector(ELEMENT_SELECTOR));

    ElementContext otherElement = new ElementContext.Basic(elementType, getCssSelector(ELEMENT_SELECTOR));

    assertThat(getValidationError(list, otherElement), CoreMatchers.is(nullValue()));
  }

  @Test
  public void testValidateBasicListWithElementListHavingSameSelector() {
    TypeProvider elementType = ELEMENT_TYPE;
    ElementContext element =
        new ElementContext.BasicReturnsAll(elementType, getCssSelector(ELEMENT_SELECTOR));

    assertThat(getValidationError(element, element), is(nullValue()));
  }

  @Test
  public void testValidateBasicElementsDifferentSelectors() {
    ElementContext first =
        new ElementContext.Basic(ELEMENT_NAME, ELEMENT_TYPE, getCssSelector(ELEMENT_SELECTOR));

    ElementContext second =
        new ElementContext.Basic(
            ELEMENT_NAME, ELEMENT_TYPE, getCssSelector(".fakeOtherSelector"));

    assertThat(getValidationError(first, second), is(nullValue()));
  }

  @Test
  public void testValidateBasicElementsSameSelectors() {
    ElementContext first =
        new ElementContext.Basic(ELEMENT_NAME, ELEMENT_TYPE, getCssSelector(ELEMENT_SELECTOR));

    assertThat(getValidationError(first, first), is(nullValue()));
  }

  @Test
  public void testValidateBasicElementsAndFrameSameSelectors() {
    ElementContext first = new ElementContext.Basic(ELEMENT_NAME, ELEMENT_TYPE, getCssSelector(ELEMENT_SELECTOR));
    ElementContext frame = new ElementContext.Frame(ELEMENT_NAME, ELEMENT_SELECTOR);
    assertThat(getValidationError(first, frame), is(nullValue()));
  }
}
