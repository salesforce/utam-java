/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities.FromString;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

public class CustomElementMethodTests {

  private static final String METHOD_NAME = "getTest";
  private static final TypeProvider EXPECTED_TYPE =
      new FromString("utam.test.pageobjects.Component");
  private static final String IMPORTED_LIST_TYPE = List.class.getName();

  @Test
  public void testNestedCustomListWithFilterFindFirst() {
    PageObjectMethod method =
        new DeserializerUtilities()
            .getContext("filter/customNestedWithFilter")
            .getMethod(METHOD_NAME);
    MethodInfo expected = new MethodInfo(METHOD_NAME, "Component");
    expected.addParameter(new MethodParameterInfo("arg1"));
    expected.addParameter(new MethodParameterInfo("arg2"));
    expected.addCodeLine("BasicElement element = this.getElementElement(arg1)");
    expected.addCodeLines(
        "return custom(element, this.test).build(Component.class, elm -> (elm.getText()!= null &&"
            + " elm.getText().contains(arg2)))");
    expected.addImportedTypes(EXPECTED_TYPE.getFullName());
    expected.addImpliedImportedTypes(EXPECTED_TYPE.getFullName(), BASIC_ELEMENT.getFullName());
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testCustomNullableListWithFilter() {
    MethodInfo expected = new MethodInfo(METHOD_NAME, "List<Component>");
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine(
        "return custom(root, this.test).buildList(Component.class, elm ->"
            + " Boolean.TRUE.equals(elm.isVisible()))");
    PageObjectMethod method =
        new DeserializerUtilities().getContext("filter/customWithFilter").getMethod(METHOD_NAME);
    expected.addImportedTypes(EXPECTED_TYPE.getFullName(), IMPORTED_LIST_TYPE);
    expected.addImpliedImportedTypes(
        EXPECTED_TYPE.getFullName(), BASIC_ELEMENT.getFullName(), IMPORTED_LIST_TYPE);
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testCustomFilterBooleanMatcherFindAllNullable() {
    TranslationContext context =
        new DeserializerUtilities().getContext("filter/customFilterBoolean");
    PageObjectMethod method = context.getMethod(METHOD_NAME);
    MethodInfo expected = new MethodInfo(METHOD_NAME, "List<Test>");
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLines(
        "return custom(root, this.test)"
            + ".buildList(Test.class, elm -> Boolean.TRUE.equals(elm.isVisible()))");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testCustomNestedFilterFindFirst() {
    TranslationContext context =
        new DeserializerUtilities().getContext("filter/customFilterNested");
    PageObjectMethod method = context.getMethod(METHOD_NAME);
    MethodInfo expected = new MethodInfo(METHOD_NAME, "Test");
    expected.addParameter(new MethodParameterInfo("scopeArg"));
    expected.addParameter(new MethodParameterInfo("selectorArg"));
    expected.addParameter(new MethodParameterInfo("applyArg"));
    expected.addParameter(new MethodParameterInfo("matcherArg"));
    expected.addCodeLine("BasicElement scope = this.getScopeElement(scopeArg)");
    expected.addCodeLines(
        "return custom(scope, this.test.setParameters(selectorArg))"
            + ".build(Test.class, elm -> matcherArg.equals(elm.customMethod(applyArg)))");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }
}
