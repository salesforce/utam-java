/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import org.hamcrest.Matchers;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;

public class FrameMethodTests {

  private static final String ELEMENT_NAME = "test";

  private static PageObjectMethod getFrameGetter(String fileName) {
    TranslationContext context = new DeserializerUtilities().getContext("frame/" + fileName);
    ElementContext element = context.getElement(ELEMENT_NAME);
    PageObjectMethod method = element.getElementMethod();
    assertThat(element.getType().getFullName(), is(Matchers.equalTo(FRAME_ELEMENT.getFullName())));
    return method;
  }

  @Test
  public void testPublicFrame() {
    final String methodName = getElementGetterMethodName(ELEMENT_NAME, true);
    PageObjectMethod method = getFrameGetter("framePublic");
    MethodInfo expected = new MethodInfo(methodName, FRAME_ELEMENT);
    expected.addParameter(new MethodParameterInfo("scopeStr"));
    expected.addParameter(new MethodParameterInfo("frameStr"));
    expected.addImportedTypes(FRAME_ELEMENT.getFullName());
    expected.addImpliedImportedTypes(FRAME_ELEMENT.getFullName(), BASIC_ELEMENT.getFullName());
    expected.addCodeLine("BasicElement scope = this.getScopeElement(scopeStr)");
    expected.addCodeLine("return basic(scope, this.test.setParameters(frameStr)).buildFrame()");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testPrivateFrame() {
    final String methodName = getElementGetterMethodName(ELEMENT_NAME, false);
    PageObjectMethod method = getFrameGetter("framePrivate");
    MethodInfo expected = new MethodInfo(methodName, FRAME_ELEMENT).setNotPublic();
    expected.addImportedTypes(FRAME_ELEMENT.getFullName());
    expected.addImpliedImportedTypes(FRAME_ELEMENT.getFullName(), BASIC_ELEMENT.getFullName());
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine("return basic(root, this.test).buildFrame()");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }
}
