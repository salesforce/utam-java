/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.compose;

import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT;

import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;

/**
 * test JSON files for document object
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class FrameElementComposeTests {

  private TranslationContext context;

  @BeforeClass
  public void prepareData() {
    context = new DeserializerUtilities().getContext("compose/frameActions");
  }

  @Test
  public void testFrameInvocationsInCompose() {
    final String methodName = "iframeTest";
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName);
    methodInfo.addParameter(new MethodParameterInfo("frameStr"));
    methodInfo.addCodeLine("this.getDocument().enterFrame(this.getMyFrameElement())");
    methodInfo.addCodeLine("this.getDocument().enterFrame(this.getMyPublicFrame(frameStr))");
    methodInfo.addCodeLine("this.getDocument().exitFrame()");
    methodInfo.addCodeLine("this.getDocument().exitToParentFrame()");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testPageObjectTypeLiteralParameter() {
    final String methodName = "testLiteralArgs";
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName);
    String importPageObjectType = "my.lightning.Button";
    methodInfo.addImpliedImportedTypes(importPageObjectType);
    methodInfo.addCodeLine("this.getDocument().enterFrameAndLoad(this.getMyFrameElement(), Button.class)");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testPageObjectTypeNonLiteralParameter() {
    final String methodName = "testNonLiteralArgs";
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName);
    methodInfo.addParameter(new MethodParameterInfo("myFrame", "FrameElement"));
    methodInfo.addParameter(new MethodParameterInfo("pageObject", "Class<? extends RootPageObject>"));
    methodInfo.addImportedTypes(ROOT_PAGE_OBJECT.getFullName(), Class.class.getName(), FRAME_ELEMENT.getFullName());
    methodInfo.addImpliedImportedTypes(ROOT_PAGE_OBJECT.getFullName(), Class.class.getName(), FRAME_ELEMENT.getFullName());
    methodInfo.addCodeLine("this.getDocument().enterFrameAndLoad(myFrame, pageObject)");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }
}
