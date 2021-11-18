/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.representation.FrameMethod.FRAME_ELEMENT;
import static utam.compiler.representation.FrameMethod.FRAME_IMPL_CLASS;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.Collections;
import org.testng.annotations.Test;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.LocatorCodeGeneration.SelectorType;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.selenium.element.LocatorBy;

public class FrameMethodTests {

  private static final String ELEMENT_NAME = "testFrame";

  private static MethodInfo getExpectedMethod(boolean isPublic) {
    String methodName = getElementGetterMethodName(ELEMENT_NAME, isPublic);
    MethodInfo info = new MethodInfo(methodName, FRAME_ELEMENT);
    info.addImportedTypes(FRAME_ELEMENT.getFullName());
    info.addImpliedImportedTypes(FRAME_IMPL_CLASS.getFullName());
    info.setIsPublic(isPublic);
    return info;
  }

  @Test
  public void testFrameElementGetterPublic() {
    ElementContext element = new ElementContext.Frame(ELEMENT_NAME, "css");
    PageObjectMethod method = new FrameMethod(element, true);
    MethodInfo expected = getExpectedMethod(true);
    expected.addCodeLine(String
        .format("element(this.%s).build(FrameElement.class, FrameElementImpl.class)", ELEMENT_NAME));
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testFrameElementGetterPrivate() {
    ElementContext scope = new ElementContext.Basic("scope");
    LocatorCodeGeneration locatorHelper = new LocatorCodeGeneration("css");
    ElementContext element = new ElementContext.Frame(scope, ELEMENT_NAME, locatorHelper);
    PageObjectMethod method = new FrameMethod(element, false);
    MethodInfo expected = getExpectedMethod(false);
    expected.addCodeLine(String
        .format("element(this.%s).build(FrameElement.class, FrameElementImpl.class)", ELEMENT_NAME));
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testFrameElementGetterWithArgs() {
    ElementContext scope = new ElementContext.Basic("scope");
    LocatorCodeGeneration locatorHelper = new LocatorCodeGeneration(SelectorType.css,
        LocatorBy.byCss("css[%d]"),
        Collections.singletonList(new Regular("arg1", PrimitiveType.NUMBER)));
    ElementContext element = new ElementContext.Frame(scope, ELEMENT_NAME, locatorHelper);
    PageObjectMethod method = new FrameMethod(element, false);
    MethodInfo expected = getExpectedMethod(false);
    expected.addParameter(new MethodParameterInfo("arg1", PrimitiveType.NUMBER));
    expected.addCodeLine("element(this.testFrame).build(FrameElement.class, FrameElementImpl.class, arg1)");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }
}
