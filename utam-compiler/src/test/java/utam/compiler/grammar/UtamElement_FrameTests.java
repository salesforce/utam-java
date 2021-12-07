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
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_MISSING_SELECTOR_PROPERTY;
import static utam.compiler.grammar.UtamElement.ERR_FRAME_LIST_SELECTOR_NOT_ALLOWED;
import static utam.compiler.grammar.UtamElement.Type;
import static utam.compiler.representation.FrameMethod.FRAME_ELEMENT;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.List;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.element.FrameElement;
import utam.core.framework.consumer.UtamError;

/**
 * test reads JSON file with declared frames
 *
 * @since 236
 */
public class UtamElement_FrameTests {

  private List<UtamElement> frames;

  @BeforeClass
  public void prepareData() {
    frames = DeserializerUtilities.getDeserializedObjects(UtamElement.class, "element/frames");
  }

  @Test
  public void testValidFrameElementNode() {
    UtamElement utamElement = frames.get(0);
    UtamElement.Traversal abstraction = utamElement.getAbstraction();
    TranslationContext context = getTestTranslationContext();
    assertThat(abstraction.getClass(), is(equalTo(UtamElement.Frame.class)));
    ElementContext elementContext = abstraction.testRootTraverse(context);
    assertThat(elementContext.getType().isSameType(new TypeUtilities.FromClass(FrameElement.class)),
        is(equalTo(true)));

    final String methodName = getElementGetterMethodName("simpleFrameElement", false);
    PageObjectMethod method = elementContext.getElementMethod();
    assertThat(method.getDeclaration().getName(), is(equalTo(methodName)));

    method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, FRAME_ELEMENT).setNotPublic();
    expected.addCodeLine("return element(this.simpleFrameElement).build(FrameElement.class, FrameElementImpl.class)");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testPublicFrameElementGetterWithParameters() {
    UtamElement utamElement = frames.get(8);
    TranslationContext context = getTestTranslationContext();
    UtamElement.Traversal abstraction = utamElement.getAbstraction();
    abstraction.testRootTraverse(context);
    final String methodName = getElementGetterMethodName("myPublicFrame", true);
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, FRAME_ELEMENT);
    expected.addParameter(new MethodParameterInfo("frameStr"));
    expected.addCodeLine(
        "return element(this.myPublicFrame).build(FrameElement.class, FrameElementImpl.class, frameStr)");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testFrameElementWithReturnAllSelectorThrows() {
    UtamElement utamElement = frames.get(1);
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_FRAME_LIST_SELECTOR_NOT_ALLOWED, "returnAllThrows"))));
  }

  @Test
  public void testFrameElementWithNoSelectorThrows() {
    UtamElement utamElement = frames.get(2);
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_ELEMENT_MISSING_SELECTOR_PROPERTY, "noSelectorThrows"))));
  }

  @Test
  public void testFrameElementWithNullableThrows() {
    UtamElement utamElement = frames.get(3);
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(Type.FRAME.getSupportedPropertiesErr("nullableThrows"))));
  }

  @Test
  public void testFrameElementWithExternalThrows() {
    UtamElement utamElement = frames.get(4);
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(Type.FRAME.getSupportedPropertiesErr("externalThrows"))));
  }

  @Test
  public void testFrameElementWithElementsThrows() {
    UtamElement utamElement = frames.get(5);
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(Type.FRAME.getSupportedPropertiesErr("elementsThrows"))));
  }

  @Test
  public void testFrameElementWithFilterThrows() {
    UtamElement utamElement = frames.get(7);
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(Type.FRAME.getSupportedPropertiesErr("filterThrows"))));
  }

  @Test
  public void testFrameElementWithShadowThrows() {
    UtamElement utamElement = frames.get(6);
    UtamError e = expectThrows(UtamError.class, utamElement::getAbstraction);
    assertThat(e.getMessage(), is(equalTo(Type.FRAME.getSupportedPropertiesErr("shadowThrows"))));
  }
}
