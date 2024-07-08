/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.assertThrows;
import static utam.compiler.grammar.TestUtilities.TEST_PAGE_OBJECT;
import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.helpers.ElementContext.DOCUMENT_ELEMENT_NAME;
import static utam.compiler.helpers.ElementContext.EMPTY_SELECTOR;
import static utam.compiler.helpers.ElementContext.NAVIGATION_OBJECT_NAME;
import static utam.compiler.helpers.ElementContext.ROOT_ELEMENT_NAME;
import static utam.compiler.helpers.ElementContext.SELF_ELEMENT_NAME;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT;
import static utam.compiler.types.BasicElementInterface.actionable;

import java.util.Collections;
import org.testng.annotations.Test;
import utam.compiler.helpers.ElementContext.Container;
import utam.compiler.helpers.ElementContext.Custom;
import utam.compiler.helpers.ElementContext.CustomReturnsAll;
import utam.compiler.helpers.ElementContext.Document;
import utam.compiler.helpers.ElementContext.ElementType;
import utam.compiler.helpers.ElementContext.Navigation;
import utam.compiler.helpers.ElementContext.Root;
import utam.compiler.helpers.ElementContext.Self;
import utam.compiler.helpers.LocatorCodeGeneration.SelectorType;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Locator;
import utam.core.selenium.element.LocatorBy;

/**
 * Provides tests for the ElementContext class
 *
 * @author james.evans
 */
public class ElementContextTests {

  private static final String ELEMENT_NAME = "fakeElementName";
  private static final Locator SELECTOR_VALUE = getCssSelector(".fakeSelector");
  private static final TypeProvider DUMMY_TYPE =
      new TypeUtilities.FromString("test.FakeElementType");
  private static final TypeProvider BASIC_ELEMENT_TYPE = actionable;

  private static ElementContext.Basic getSingleElementContext() {
    return new ElementContext.Basic(ELEMENT_NAME, BASIC_ELEMENT_TYPE, SELECTOR_VALUE);
  }

  @Test
  public void testBasicElement() {
    ElementContext context = getSingleElementContext();
    assertThat(context.getElementNodeType(), is(ElementType.BASIC));
    assertThat(context.isNullable(), is(false));
    assertThat(context.getName(), is(equalTo(ELEMENT_NAME)));
    assertThat(context.getType(), is(equalTo(BASIC_ELEMENT_TYPE)));
    assertThat(context.getParameters(), is(hasSize(0)));
    assertThat(context.getSelector(), is(equalTo(SELECTOR_VALUE)));
  }

  @Test
  public void testCustomElement() {
    ElementContext context = new Custom(ELEMENT_NAME, DUMMY_TYPE, SELECTOR_VALUE);
    assertThat(context.getElementNodeType(), is(ElementType.CUSTOM));
    assertThat(context.isNullable(), is(false));
    assertThat(context.getName(), is(equalTo(ELEMENT_NAME)));
    assertThat(context.getType(), is(equalTo(DUMMY_TYPE)));
    assertThat(context.getParameters(), is(hasSize(0)));
    assertThat(context.getSelector(), is(equalTo(SELECTOR_VALUE)));
  }

  @Test
  public void testCustomListElementWithParameters() {
    MethodParameter parameter = new Regular("arg", PrimitiveType.STRING);
    ElementContext context =
        new CustomReturnsAll(
            null,
            ELEMENT_NAME,
            DUMMY_TYPE,
            SELECTOR_VALUE,
            Collections.singletonList(parameter),
            true);
    assertThat(context.getElementNodeType(), is(ElementType.CUSTOM));
    assertThat(context.isNullable(), is(true));
    assertThat(context.getName(), is(equalTo(ELEMENT_NAME)));
    assertThat(context.getType(), is(equalTo(DUMMY_TYPE)));
    assertThat(context.getParameters(), is(hasSize(1)));
    assertThat(context.getSelector(), is(equalTo(SELECTOR_VALUE)));
  }

  @Test
  public void testBasicElementWithoutGetterThrows() {
    ElementContext elementContext = getSingleElementContext();
    assertThrows(NullPointerException.class, elementContext::getElementMethod);
  }

  @Test
  public void testBasicElementSetDuplicateGetterThrows() {
    PageObjectMethod method = mock(PageObjectMethod.class);
    ElementContext elementContext = getSingleElementContext();
    TranslationContext context = getTestTranslationContext();
    elementContext.setElementMethod(method, context);
    assertThrows(
        NullPointerException.class, () -> elementContext.setElementMethod(method, context));
  }

  @Test
  public void testContainerElement() {
    ElementContext context = new Container(getSingleElementContext(), "container");
    assertThat(context.getElementNodeType(), is(ElementType.CONTAINER));
    assertThat(context.isNullable(), is(false));
    assertThat(context.getName(), is(equalTo("container")));
    assertThat(context.getType(), is(equalTo(CONTAINER_ELEMENT)));
    assertThat(context.getParameters(), is(hasSize(0)));
    assertThat(context.getSelector(), is(equalTo(EMPTY_SELECTOR)));
  }

  @Test
  public void testDocumentElement() {
    ElementContext context = Document.DOCUMENT_ELEMENT;
    assertThat(context.getElementNodeType(), is(ElementType.DOCUMENT));
    assertThat(context.isNullable(), is(false));
    assertThat(context.getName(), is(equalTo(DOCUMENT_ELEMENT_NAME)));
    assertThat(context.getType(), is(nullValue()));
    assertThat(context.getParameters(), is(hasSize(0)));
    assertThat(context.getSelector(), is(equalTo(EMPTY_SELECTOR)));
  }

  @Test
  public void testNavigationElement() {
    ElementContext context = Navigation.NAVIGATION_OBJECT;
    assertThat(context.getElementNodeType(), is(ElementType.NAVIGATION));
    assertThat(context.isNullable(), is(false));
    assertThat(context.getName(), is(equalTo(NAVIGATION_OBJECT_NAME)));
    assertThat(context.getType(), is(nullValue()));
    assertThat(context.getParameters(), is(hasSize(0)));
    assertThat(context.getSelector(), is(equalTo(EMPTY_SELECTOR)));
  }

  @Test
  public void testSelfElement() {
    ElementContext context = Self.SELF_ELEMENT;
    assertThat(context.getElementNodeType(), is(ElementType.SELF));
    assertThat(context.isNullable(), is(false));
    assertThat(context.getName(), is(equalTo(SELF_ELEMENT_NAME)));
    assertThat(context.getType(), is(nullValue()));
    assertThat(context.getParameters(), is(hasSize(0)));
    assertThat(context.getSelector(), is(equalTo(EMPTY_SELECTOR)));
  }

  @Test
  public void testRootElement() {
    ElementContext.Root context =
        new Root(TEST_PAGE_OBJECT, getCssSelector("css"), actionable, mock(PageObjectMethod.class));
    assertThat(context.getName(), is(equalTo(ROOT_ELEMENT_NAME)));
    assertThat(context.getElementNodeType(), is(ElementType.ROOT));
    assertThat(context.isNullable(), is(false));
    assertThat(
        context.getEnclosingPageObjectType().getFullName(),
        is(equalTo(TEST_PAGE_OBJECT.getFullName())));
    assertThat(context.getType().getFullName(), is(equalTo(actionable.getFullName())));
    assertThat(context.getParameters(), is(hasSize(0)));
    assertThat(context.getSelector(), is(equalTo(getCssSelector("css"))));
  }

  @Test
  public void testFrameElement() {
    LocatorCodeGeneration locatorHelper =
        new LocatorCodeGeneration(
            SelectorType.css,
            LocatorBy.byCss("css[%d]"),
            Collections.singletonList(new Regular("arg1", PrimitiveType.NUMBER)));
    ElementContext.Frame context =
        new ElementContext.Frame(getSingleElementContext(), ELEMENT_NAME, locatorHelper);
    assertThat(context.getName(), is(equalTo(ELEMENT_NAME)));
    assertThat(context.getElementNodeType(), is(ElementType.FRAME));
    assertThat(context.isNullable(), is(false));
    assertThat(context.getType(), is(equalTo(FRAME_ELEMENT)));
    assertThat(context.getParameters(), is(hasSize(1)));
    assertThat(context.getSelector().getStringValue(), is(equalTo("css[%d]")));
  }
}
