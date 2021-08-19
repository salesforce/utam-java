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
import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.helpers.ElementContext.DOCUMENT_ELEMENT_NAME;
import static utam.compiler.helpers.ElementContext.EMPTY_SELECTOR;
import static utam.compiler.helpers.ElementContext.ROOT_ELEMENT_NAME;
import static utam.compiler.helpers.TypeUtilities.ROOT_ELEMENT_TYPE;
import static utam.compiler.helpers.ElementContext.SELF_ELEMENT_NAME;
import static utam.compiler.helpers.TypeUtilities.BasicElementInterface.actionable;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT;

import java.util.Collections;
import org.testng.annotations.Test;
import utam.compiler.helpers.ElementContext.Basic;
import utam.compiler.helpers.ElementContext.Container;
import utam.compiler.helpers.ElementContext.Custom;
import utam.compiler.helpers.ElementContext.Document;
import utam.compiler.helpers.ElementContext.Root;
import utam.compiler.helpers.ElementContext.Self;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Locator;

/**
 * Provides tests for the ElementContext class
 *
 * @author james.evans
 */
public class ElementContextTests {

  private static final String ELEMENT_NAME = "fakeElementName";
  private static final Locator SELECTOR_VALUE = getCssSelector(".fakeSelector");
  private static final TypeProvider DUMMY_TYPE = new TypeUtilities.FromString("FakeElementType",
      "test.FakeElementType");
  private static final TypeProvider BASIC_ELEMENT_TYPE = actionable;

  private static ElementContext.Basic getSingleElementContext() {
    return new ElementContext.Basic(ELEMENT_NAME, BASIC_ELEMENT_TYPE, SELECTOR_VALUE);
  }

  @Test
  public void testBasicElement() {
    ElementContext context = getSingleElementContext();
    assertThat(context.isDocumentElement(), is(false));
    assertThat(context.isRootElement(), is(false));
    assertThat(context.isSelfElement(), is(false));
    assertThat(context.isNullable(), is(false));
    assertThat(context.isList(), is(false));
    assertThat(context.isCustomElement(), is(false));
    assertThat(context.getName(), is(equalTo(ELEMENT_NAME)));
    assertThat(context.getType(), is(equalTo(BASIC_ELEMENT_TYPE)));
    assertThat(context.getParameters(), is(hasSize(0)));
    assertThat(context.getSelector(), is(equalTo(SELECTOR_VALUE)));
  }

  @Test
  public void testBasicElementWithParameters() {
    MethodParameter parameter = new Regular("arg", PrimitiveType.STRING);
    ElementContext context = new Basic(null,
        "name",
        BASIC_ELEMENT_TYPE,
        SELECTOR_VALUE, true, Collections.singletonList(parameter), true);
    assertThat(context.isNullable(), is(true));
    assertThat(context.isList(), is(true));
    assertThat(context.getParameters(), is(hasSize(1)));
    assertThat(context.getParameters().get(0), is(equalTo(parameter)));
  }

  @Test
  public void testCustomElement() {
    ElementContext context = new Custom(ELEMENT_NAME, DUMMY_TYPE, SELECTOR_VALUE);
    assertThat(context.isDocumentElement(), is(false));
    assertThat(context.isRootElement(), is(false));
    assertThat(context.isSelfElement(), is(false));
    assertThat(context.isNullable(), is(false));
    assertThat(context.isList(), is(false));
    assertThat(context.isCustomElement(), is(true));
    assertThat(context.getName(), is(equalTo(ELEMENT_NAME)));
    assertThat(context.getType(), is(equalTo(DUMMY_TYPE)));
    assertThat(context.getParameters(), is(hasSize(0)));
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
    elementContext.setElementMethod(method);
    assertThrows(NullPointerException.class, () -> elementContext.setElementMethod(method));
  }

  @Test
  public void testContainerElement() {
    ElementContext context = new Container(getSingleElementContext(), "container");
    assertThat(context.isDocumentElement(), is(false));
    assertThat(context.isRootElement(), is(false));
    assertThat(context.isSelfElement(), is(false));
    assertThat(context.isNullable(), is(false));
    assertThat(context.isList(), is(false));
    assertThat(context.isCustomElement(), is(false));
    assertThat(context.getName(), is(equalTo("container")));
    assertThat(context.getType(), is(equalTo(CONTAINER_ELEMENT)));
    assertThat(context.getParameters(), is(hasSize(0)));
    assertThat(context.getSelector(), is(equalTo(EMPTY_SELECTOR)));
  }

  @Test
  public void testDocumentElement() {
    ElementContext context = Document.DOCUMENT_ELEMENT;
    assertThat(context.isDocumentElement(), is(true));
    assertThat(context.isRootElement(), is(false));
    assertThat(context.isSelfElement(), is(false));
    assertThat(context.isNullable(), is(false));
    assertThat(context.isList(), is(false));
    assertThat(context.isCustomElement(), is(false));
    assertThat(context.getName(), is(equalTo(DOCUMENT_ELEMENT_NAME)));
    assertThat(context.getType(), is(nullValue()));
    assertThat(context.getParameters(), is(hasSize(0)));
    assertThat(context.getSelector(), is(equalTo(EMPTY_SELECTOR)));
  }

  @Test
  public void testSelfElement() {
    ElementContext context = Self.SELF_ELEMENT;
    assertThat(context.isDocumentElement(), is(false));
    assertThat(context.isRootElement(), is(false));
    assertThat(context.isSelfElement(), is(true));
    assertThat(context.isNullable(), is(false));
    assertThat(context.getName(), is(equalTo(SELF_ELEMENT_NAME)));
    assertThat(context.getType(), is(nullValue()));
    assertThat(context.isList(), is(false));
    assertThat(context.isCustomElement(), is(false));
    assertThat(context.getParameters(), is(hasSize(0)));
    assertThat(context.getSelector(), is(equalTo(EMPTY_SELECTOR)));
  }

  @Test
  public void testRootElement() {
    ElementContext.Root context = new Root(DUMMY_TYPE);
    assertThat(context.getName(), is(equalTo(ROOT_ELEMENT_NAME)));
    assertThat(context.isDocumentElement(), is(false));
    assertThat(context.isSelfElement(), is(false));
    assertThat(context.isNullable(), is(false));
    assertThat(context.isRootElement(), is(true));
    assertThat(context.getEnclosingPageObjectType(), is(equalTo(DUMMY_TYPE)));
    assertThat(context.getType(), is(equalTo(ROOT_ELEMENT_TYPE)));
    assertThat(context.isList(), is(false));
    assertThat(context.isCustomElement(), is(false));
    assertThat(context.getParameters(), is(hasSize(0)));
    assertThat(context.getSelector(), is(equalTo(EMPTY_SELECTOR)));
  }

  @Test
  public void testRootElementWithSelector() {
    Locator selector = SELECTOR_VALUE;
    ElementContext.Root context = new Root(DUMMY_TYPE, selector);
    assertThat(context.getSelector(), is(equalTo(selector)));
  }

  @Test
  public void testFrameElement() {
    ElementContext.Frame context = new ElementContext.Frame(getSingleElementContext(), ELEMENT_NAME, SELECTOR_VALUE);
    assertThat(context.getName(), is(equalTo(ELEMENT_NAME)));
    assertThat(context.isDocumentElement(), is(false));
    assertThat(context.isSelfElement(), is(false));
    assertThat(context.isNullable(), is(false));
    assertThat(context.isRootElement(), is(false));
    assertThat(context.getType(), is(equalTo(FRAME_ELEMENT)));
    assertThat(context.isList(), is(false));
    assertThat(context.isCustomElement(), is(false));
    assertThat(context.getParameters(), is(hasSize(0)));
    assertThat(context.getSelector(), is(equalTo(SELECTOR_VALUE)));
  }
}
