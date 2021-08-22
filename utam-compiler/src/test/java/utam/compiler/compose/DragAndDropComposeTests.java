/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.compose;

import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;

import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;

/**
 * @author elizaveta.ivanova
 * @since 236
 */
public class DragAndDropComposeTests {

  private TranslationContext context;

  @BeforeClass
  public void prepareData() {
    context = new DeserializerUtilities().getContext("compose/dragAndDrop");
  }

  @Test
  public void testDragAndDropWithElementLiteralDuration() {
    final String methodName = "composeDragAndDropElementLiteralWithDuration";
    MethodInfo methodInfo = new MethodInfo(methodName);
    methodInfo.addParameter(new MethodParameterInfo("selectorArg1"));
    methodInfo.addParameter(new MethodParameterInfo("selectorArg2"));
    methodInfo.addCodeLine("this.getFirstElement(selectorArg1).dragAndDrop(this.getSecond(selectorArg2), 2)");
    PageObjectMethod method = context.getMethod(methodName);
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testDragAndDropWithOffset() {
    final String methodName = "composeDragAndDropWithOffset";
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName);
    methodInfo.addParameter(new MethodParameterInfo("x", PrimitiveType.NUMBER));
    methodInfo.addParameter(new MethodParameterInfo("y", PrimitiveType.NUMBER));
    methodInfo.addCodeLine("this.getSimplePublic().dragAndDropByOffset(x, y, 0)");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testDragAndDropWithOffsetDuration() {
    final String methodName = "composeDragAndDropWithOffsetDuration";
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName);
    methodInfo.addParameter(new MethodParameterInfo("duration", PrimitiveType.NUMBER));
    methodInfo.addCodeLine("this.getSimplePublic().dragAndDropByOffset(1, 2, duration)");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testDragAndDropWithElementNonLiteral() {
    final String methodName = "composeDragAndDropElement";
    MethodInfo methodInfo = new MethodInfo(methodName);
    methodInfo.addImportedTypes(BASIC_ELEMENT.getFullName());
    methodInfo.addImpliedImportedTypes(BASIC_ELEMENT.getFullName());
    methodInfo.addParameter(new MethodParameterInfo("selectorArg1"));
    methodInfo.addParameter(new MethodParameterInfo("elementArg", BASIC_ELEMENT));
    methodInfo.addCodeLine("this.getFirstElement(selectorArg1).dragAndDrop(elementArg, 0)");
    PageObjectMethod method = context.getMethod(methodName);
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }
}
