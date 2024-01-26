/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.core.StringContains.containsString;
import static utam.compiler.grammar.DeserializerUtilities.expectCompilerErrorFromFile;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.COLLECTOR_IMPORT;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT;
import static utam.compiler.representation.PageObjectValidationTestHelper.validateMethod;

import java.util.*;
import org.testng.annotations.Test;
import utam.compiler.JsonBuilderTestUtility;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;

/**
 * test composed methods
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamMethodActionApplyTests {

  private static final String methodName = "test";
  private static final String LIST_IMPORT = List.class.getName();
  private static final String COLLECTORS_IMPORT = COLLECTOR_IMPORT.getFullName();
  private static final String CUSTOM_TYPE_IMPORT = "org.lwr.pageobjects.Custom";

  private static TranslationContext getContext(String json) {
    return new DeserializerUtilities().getContext("compose/apply/" + json);
  }

  @Test
  public void testComposeCustomElement() {
    TranslationContext context = getContext("customElement");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "List<String>");
    expected.addParameter(new MethodParameterInfo("strArg"));
    expected.addImpliedImportedTypes(LIST_IMPORT, CUSTOM_TYPE_IMPORT);
    expected.addImportedTypes(LIST_IMPORT);
    expected.addCodeLine("Custom custom0 = this.getCustomElement()");
    expected.addCodeLine("List<String> statement0 = custom0.someMethod(strArg, true)");
    expected.addCodeLine("return statement0");
    validateMethod(method, expected);
  }

  @Test
  public void testComposeWithDocumentElement() {
    TranslationContext context = getContext("documentElement");
    PageObjectMethod actualMethod = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName, "Boolean");
    methodInfo.addCodeLine("String statement0 = this.getDocument().getUrl()");
    methodInfo.addCodeLine("Boolean matcher0 = \"url\".equals(statement0)");
    methodInfo.addCodeLine("return matcher0");
    validateMethod(actualMethod, methodInfo);
  }

  @Test
  public void testComposeWithNavigation() {
    TranslationContext context = getContext("navigationElement");
    PageObjectMethod actualMethod = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName);
    methodInfo.addCodeLine("this.getNavigation().back()");
    methodInfo.addCodeLine("this.getNavigation().forward()");
    validateMethod(actualMethod, methodInfo);
  }

  @Test
  public void testComposeWithBasicElement() {
    TranslationContext context = getContext("basicElement");
    PageObjectMethod actualMethod = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName);
    methodInfo.addCodeLine("TestElement test0 = this.getTestElement()");
    methodInfo.addCodeLine("test0.click()");
    validateMethod(actualMethod, methodInfo);
  }

  @Test
  public void testComposeWithBasicElementClickVariations() {
    TranslationContext context = getContext("clickableElement");
    PageObjectMethod actualMethod = context.getMethod("testDoubleClick");
    MethodInfo methodInfo = new MethodInfo("testDoubleClick");
    methodInfo.addCodeLine("TestElement test0 = this.getTestElement()");
    methodInfo.addCodeLine("test0.doubleClick()");
    validateMethod(actualMethod, methodInfo);

    actualMethod = context.getMethod("testRightClick");
    methodInfo = new MethodInfo("testRightClick");
    methodInfo.addCodeLine("TestElement test0 = this.getTestElement()");
    methodInfo.addCodeLine("test0.rightClick()");
    validateMethod(actualMethod, methodInfo);

    actualMethod = context.getMethod("testClickAndHold");
    methodInfo = new MethodInfo("testClickAndHold");
    methodInfo.addParameter(new MethodParameterInfo("duration", "Integer"));
    methodInfo.addCodeLine("TestElement test0 = this.getTestElement()");
    methodInfo.addCodeLine("test0.clickAndHold(duration)");
    validateMethod(actualMethod, methodInfo);
  }

  @Test
  public void testComposeWithNestedBasicElementWithSelectorArgs() {
    TranslationContext context = getContext("nestedArgElement");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "String");
    expected.addParameter(new MethodParameterInfo("row", "Integer"));
    expected.addParameter(new MethodParameterInfo("column", "Integer"));
    expected.addCodeLine("BasicElement nestedTarget0 = this.getNestedTargetElement(row, column)");
    expected.addCodeLine("String statement0 = nestedTarget0.getText()");
    expected.addCodeLine("return statement0");
    validateMethod(method, expected);
  }

  @Test
  public void testComposeBasicElementWithFilter() {
    TranslationContext context = getContext("basicElementWithFilter");
    PageObjectMethod actualMethod = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName, "Test");
    methodInfo.addParameter(new MethodParameterInfo("buttonText"));
    methodInfo.addCodeLine("TestElement test0 = this.getTest(buttonText)");
    methodInfo.addCodeLine("test0.click()");
    methodInfo.addCodeLine("return this");
    validateMethod(actualMethod, methodInfo);
  }

  @Test
  public void testSelfComposeMethodWithoutReturn() {
    TranslationContext context = getContext("selfReturnBoolean");
    PageObjectMethod actualMethod = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "Boolean");
    expected.addCodeLine("Boolean statement0 = this.isPresent()");
    expected.addCodeLine("return statement0");
    validateMethod(actualMethod, expected);
  }

  @Test
  public void testSelfComposeStatementWithReturn() {
    TranslationContext context = getContext("selfReturnList");
    PageObjectMethod actualMethod = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "List<Boolean>");
    expected.addImpliedImportedTypes(LIST_IMPORT);
    expected.addImportedTypes(LIST_IMPORT);
    expected.addCodeLine("List<Boolean> statement0 = this.isPresent()");
    expected.addCodeLine("return statement0");
    validateMethod(actualMethod, expected);
  }

  @Test
  public void testListComposeMethodNodeForListElement() {
    TranslationContext context = getContext("basicListVoid");
    PageObjectMethod actualMethod = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName);
    expected.addImpliedImportedTypes(LIST_IMPORT);
    expected.addCodeLine("List<TestElement> test0 = this.getTestElement()");
    expected.addCodeLine("if (test0 == null) { return; }");
    expected.addCodeLine("test0.forEach(element -> element.focus())");
    validateMethod(actualMethod, expected);
  }

  @Test
  public void testListComposeMethodNodeWithActionReturningValueForListElement() {
    TranslationContext context = getContext("basicListReturns");
    PageObjectMethod actualMethod = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "List<Boolean>");
    expected.addImpliedImportedTypes(COLLECTORS_IMPORT, LIST_IMPORT, BASIC_ELEMENT.getFullName());
    expected.addImportedTypes(LIST_IMPORT);
    expected.addCodeLine("List<BasicElement> test0 = this.getTestElement()");
    expected.addCodeLine("if (test0 == null) { return null; }");
    expected.addCodeLine(
        "List<Boolean> statement0 = test0.stream().map(element ->"
            + " element.isVisible()).collect(Collectors.toList())");
    expected.addCodeLine("return statement0");
    validateMethod(actualMethod, expected);
  }

  @Test
  public void testSameNullableBasicElementReused() {
    TranslationContext context = getContext("basicElementReused");
    PageObjectMethod actualMethod = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "Integer");
    expected.addCodeLine("List<BasicElement> basic0 = this.getBasicElement()");
    expected.addCodeLine("if (basic0 == null) { return null; }");
    expected.addCodeLine(
        "basic0.stream().map(element -> element.getText()).collect(Collectors.toList())");
    expected.addCodeLine("Integer statement1 = basic0.size()");
    expected.addCodeLine("return statement1");
    validateMethod(actualMethod, expected);
  }

  @Test
  public void testCustomWithReferenceArgs() {
    TranslationContext context = getContext("argsReference");
    PageObjectMethod actualMethod = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "Custom");
    expected.addParameter(new MethodParameterInfo("strArg"));
    expected.addImpliedImportedTypes(CUSTOM_TYPE_IMPORT);
    expected.addImportedTypes(CUSTOM_TYPE_IMPORT);
    expected.addCodeLine("Custom custom0 = this.getCustomElement()");
    expected.addCodeLine("Custom statement0 = custom0.someMethod(strArg)");
    expected.addCodeLine("return statement0");
    validateMethod(actualMethod, expected);
  }

  @Test
  public void testCustomElementListWithChain() {
    TranslationContext context = getContext("customListWithChain");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "Boolean");
    expected.addImpliedImportedTypes(LIST_IMPORT, CUSTOM_TYPE_IMPORT, COLLECTORS_IMPORT);
    expected.addCodeLine("List<Custom> custom0 = this.getCustomElement()");
    expected.addCodeLine(
        "List<Custom> statement0 = custom0.stream().map(element ->"
            + " element.method1()).collect(Collectors.toList())");
    expected.addCodeLine(
        "Object statement1 = statement0.stream().map(element ->"
            + " element.method2()).collect(Collectors.toList())");
    expected.addCodeLine("Boolean matcher1 = statement1 != null");
    expected.addCodeLine("return matcher1");
    validateMethod(method, expected);
  }

  @Test
  public void testContainerElementWithChain() {
    TranslationContext context = getContext("containerChain");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "List<Item>");
    expected.addParameter(new MethodParameterInfo("rowIndex", PrimitiveType.NUMBER));
    expected.addImpliedImportedTypes(
        LIST_IMPORT, "my.pageobject.Section", "my.pageobject.Row", "my.pageobject.Item");
    expected.addImportedTypes(LIST_IMPORT, "my.pageobject.Item");
    expected.addCodeLine("Section statement0 = this.getContent(Section.class)");
    expected.addCodeLine("Row statement1 = statement0.getRow(rowIndex)");
    expected.addCodeLine("List<Item> statement2 = statement1.getItems()");
    expected.addCodeLine("return statement2");
    assertThat(method.getDeclaration().getCodeLine(), is("List<Item> test(Integer rowIndex)"));
    validateMethod(method, expected);
  }

  @Test
  public void testWrongArgsBasicActionThrows() {
    Exception e = expectCompilerErrorFromFile("validate/apply/wrongArgType");
    assertThat(
        e.getMessage(),
        containsString(
            "error 109: method \"test\" arguments: argument \"str\" has incorrect type, expected"
                + " \"BasicElement\", found \"String\""));
  }

  @Test
  public void testChainWithElementThrows() {
    Exception e = expectCompilerErrorFromFile("validate/apply/redundantElementForChain");
    assertThat(
        e.getMessage(),
        containsString(
            "error 606: method \"test\" statement: "
                + "\"element\" property is redundant because statement is marked as a chain"));
  }

  @Test
  public void testDragAndDropWithElementLiteralDuration() {
    TranslationContext context = getContext("dragAndDropElementLiteralWithDuration");
    MethodInfo methodInfo = new MethodInfo(methodName);
    methodInfo.addParameter(new MethodParameterInfo("selectorArg1"));
    methodInfo.addParameter(new MethodParameterInfo("selectorArg2"));
    methodInfo.addCodeLine("FirstElement first0 = this.getFirstElement(selectorArg1)");
    methodInfo.addCodeLine("first0.dragAndDrop(this.getSecond(selectorArg2), 2)");
    PageObjectMethod method = context.getMethod(methodName);
    validateMethod(method, methodInfo);
  }

  @Test
  public void testDragAndDropWithOffset() {
    TranslationContext context = getContext("dragAndDropWithOffset");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName);
    methodInfo.addParameter(new MethodParameterInfo("x", PrimitiveType.NUMBER));
    methodInfo.addParameter(new MethodParameterInfo("y", PrimitiveType.NUMBER));
    methodInfo.addCodeLine("SimplePublicElement simplePublic0 = this.getSimplePublic()");
    methodInfo.addCodeLine("simplePublic0.dragAndDropByOffset(x, y)");
    validateMethod(method, methodInfo);
  }

  @Test
  public void testDragAndDropWithOffsetDuration() {
    TranslationContext context = getContext("dragAndDropWithOffsetDuration");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName);
    methodInfo.addParameter(new MethodParameterInfo("duration", PrimitiveType.NUMBER));
    methodInfo.addCodeLine("SimplePublicElement simplePublic0 = this.getSimplePublic()");
    methodInfo.addCodeLine("simplePublic0.dragAndDropByOffset(1, 2, duration)");
    validateMethod(method, methodInfo);
  }

  @Test
  public void testFrameInvocationsInCompose() {
    TranslationContext context = getContext("frameActions");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName);
    methodInfo.addParameter(new MethodParameterInfo("frameStr"));
    methodInfo.addCodeLine("this.getDocument().enterFrame(this.getMyFrameElement())");
    methodInfo.addCodeLine("this.getDocument().enterFrame(this.getMyPublicFrame(frameStr))");
    methodInfo.addCodeLine("this.getDocument().exitFrame()");
    methodInfo.addCodeLine("this.getDocument().exitToParentFrame()");
    validateMethod(method, methodInfo);
  }

  @Test
  public void testRootPageObjectTypeLiteralParameter() {
    TranslationContext context = getContext("frameLiteralArgsVoid");
    PageObjectMethod method = context.getMethod(methodName);
    assertThat(method.getDeclaration().getCodeLine(), is("void test()"));
    MethodInfo methodInfo = new MethodInfo(methodName);
    String importPageObjectType = "my.lightning.Button";
    methodInfo.addImpliedImportedTypes(importPageObjectType);
    methodInfo.addCodeLine(
        "this.getDocument().enterFrameAndLoad(this.getMyFrameElement(), Button.class)");
    validateMethod(method, methodInfo);
  }

  @Test
  public void testRootPageObjectTypeLiteralParameterWithReturn() {
    TranslationContext context = getContext("frameLiteralArgs");
    PageObjectMethod method = context.getMethod(methodName);
    assertThat(method.getDeclaration().getCodeLine(), is("Button test()"));
    MethodInfo methodInfo = new MethodInfo(methodName, "Button");
    String importPageObjectType = "my.lightning.Button";
    methodInfo.addImpliedImportedTypes(importPageObjectType);
    methodInfo.addCodeLine(
        "Button statement0 = this.getDocument().enterFrameAndLoad(this.getMyFrameElement(),"
            + " Button.class)");
    methodInfo.addCodeLine("return statement0");
    validateMethod(method, methodInfo);
  }

  @Test
  public void testPageObjectTypeNonLiteralParameter() {
    TranslationContext context = getContext("frameNonLiteralArgs");
    PageObjectMethod method = context.getMethod(methodName);
    assertThat(
        method.getDeclaration().getCodeLine(),
        is("<T extends RootPageObject> T test(FrameElement myFrame, Class<T> pageObject)"));
    MethodInfo methodInfo = new MethodInfo(methodName, "T");
    methodInfo.addParameter(new MethodParameterInfo("myFrame", "FrameElement"));
    methodInfo.addParameter(new MethodParameterInfo("pageObject", "Class<T>"));
    methodInfo.addImportedTypes(ROOT_PAGE_OBJECT.getFullName(), FRAME_ELEMENT.getFullName());
    methodInfo.addImpliedImportedTypes(ROOT_PAGE_OBJECT.getFullName(), FRAME_ELEMENT.getFullName());
    methodInfo.addCodeLine(
        "T statement0 = this.getDocument().enterFrameAndLoad(myFrame, pageObject)");
    methodInfo.addCodeLine("return statement0");
    validateMethod(method, methodInfo);
  }

  @Test
  public void testPageObjectTypeNonLiteralParameterReturnsVoid() {
    TranslationContext context = getContext("frameNonLiteralArgsVoid");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName);
    methodInfo.addParameter(new MethodParameterInfo("myFrame", "FrameElement"));
    assertThat(
        method.getDeclaration().getCodeLine(),
        is("void test(FrameElement myFrame, Class<? extends RootPageObject> pageObject)"));
    methodInfo.addParameter(
        new MethodParameterInfo("pageObject", "Class<? extends RootPageObject>"));
    methodInfo.addImportedTypes(ROOT_PAGE_OBJECT.getFullName(), FRAME_ELEMENT.getFullName());
    methodInfo.addImpliedImportedTypes(ROOT_PAGE_OBJECT.getFullName(), FRAME_ELEMENT.getFullName());
    methodInfo.addCodeLine("this.getDocument().enterFrameAndLoad(myFrame, pageObject)");
    validateMethod(method, methodInfo);
  }

  @Test
  public void testApplyBasicActionToFrame() {
    TranslationContext context = getContext("frameAsBasic");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName, "String");
    methodInfo.addImpliedImportedTypes(FRAME_ELEMENT.getFullName());
    methodInfo.addCodeLine("FrameElement myFrame0 = this.getMyFrameElement()");
    methodInfo.addCodeLine("String statement0 = myFrame0.getText()");
    methodInfo.addCodeLine("return statement0");
    validateMethod(method, methodInfo);
  }

  @Test
  public void testCustomElementWithFilterFindFirst() {
    String importStr = "my.pageobject.Foo";
    TranslationContext context = getContext("customWithFilterFindFirst");
    PageObjectMethod method = context.getMethod(methodName);
    assertThat(
        method.getDeclaration().getCodeLine(), is("Foo test(String filterArg, String applyArg)"));
    MethodInfo expected = new MethodInfo(methodName, "Foo");
    expected.addParameter(new MethodParameterInfo("filterArg"));
    expected.addParameter(new MethodParameterInfo("applyArg"));
    expected.addImportedTypes(importStr);
    expected.addImpliedImportedTypes(importStr);
    expected.addCodeLine("Foo findFirst0 = this.getFindFirstElement(filterArg)");
    expected.addCodeLine("Foo statement0 = findFirst0.publicMethod(applyArg)");
    expected.addCodeLine("return statement0");
    validateMethod(method, expected);
  }

  /** custom element has scope with parameters */
  @Test
  public void testCustomElementWithParametrizedScope() {
    TranslationContext context = getContext("customElementNestedWithArg");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName);
    expected.addParameter(new MethodParameterInfo("row", "Integer"));
    expected.addParameter(new MethodParameterInfo("column", "Integer"));
    expected.addParameter(new MethodParameterInfo("applyArg"));
    expected.addCodeLine("Element element0 = this.getElementElement(row, column)");
    expected.addCodeLine("element0.apply(applyArg)");
    validateMethod(method, expected);
  }

  /** basic element has scope with parameters and filter "findAll" */
  @Test
  public void testBasicFilteredElementWithParametrizedScope() {
    TranslationContext context = getContext("basicElementScopeWithParameters");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "List<String>");
    expected.addParameter(new MethodParameterInfo("row", "Integer"));
    expected.addParameter(new MethodParameterInfo("column"));
    expected.addCodeLine("List<BasicElement> element0 = this.getElementElement(row, column)");
    expected.addCodeLine(
        "List<String> statement0 = element0.stream().map(element ->"
            + " element.getText()).collect(Collectors.toList())");
    expected.addCodeLine("return statement0");
    validateMethod(method, expected);
  }

  @Test
  public void incorrectComposeFormatMissingElementThrows() {
    JsonBuilderTestUtility test = new JsonBuilderTestUtility();
    test.addRawString(
        "methods",
        "[{ \"name\" : \"test\", \"compose\" : [ {\"element\": \"error\", \"apply\": \"action\"}"
            + " ]}]");
    Exception e = test.expectCompilerError();
    assertThat(
        e.getMessage(),
        containsString(
            "error 601: method \"test\" statement: "
                + "unknown element with name \"error\" is referenced in a compose statement"));
  }

  @Test
  public void testLiteralGetterArgument() {
    PageObjectMethod method =
        new DeserializerUtilities()
            .getContext("generated/args/literalGetterArg.utam")
            .getMethod("testApply");
    MethodInfo expected = new MethodInfo("testApply", "void");
    expected.addCodeLine("SettingsPanelElement settingsPanel0 = this.getSettingsPanel()");
    expected.addCodeLine("settingsPanel0.click()");
    validateMethod(method, expected);
  }
}
