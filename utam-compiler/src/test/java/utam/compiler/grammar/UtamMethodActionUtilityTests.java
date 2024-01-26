/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.TypeUtilities.COLLECTOR_IMPORT;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.base.UtamUtilitiesContext;

/**
 * test composed utility statements
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamMethodActionUtilityTests {

  private static final String UTILITY_IMPORT = UtamUtilitiesContext.class.getName();
  private static final String methodName = "test";
  private static final String UTILS_IMPORT = "utam.extension.utils.CustomExtensionUtils";
  private static final String LIST_IMPORT = List.class.getName();
  private static final String COLLECTOR_IMPORT_STR = COLLECTOR_IMPORT.getFullName();

  private static TranslationContext getContext(String fileName) {
    return new DeserializerUtilities().getContext("compose/utility/" + fileName);
  }

  @Test
  public void testComposeImperativeExtensionReturningCustomType() {
    TranslationContext context = getContext("customReturnType");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName, "CustomReturnType");
    methodInfo.addImportedTypes("utam.extension.pageobjects.CustomReturnType");
    methodInfo.addImpliedImportedTypes(
        UTILS_IMPORT, UTILITY_IMPORT, "utam.extension.pageobjects.CustomReturnType");
    methodInfo.addCodeLine(
        "CustomReturnType statement0 = CustomExtensionUtils.getFieldValue(new"
            + " UtamUtilitiesContext(this), fieldType)");
    methodInfo.addCodeLine("return statement0");
    methodInfo.addParameter(new MethodParameterInfo("fieldType", "String"));
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testComposeImperativeExtensionUsedInChain() {
    TranslationContext context = getContext("chain");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "List<String>");
    expected.addImpliedImportedTypes(UTILS_IMPORT, UTILITY_IMPORT);
    expected.addImpliedImportedTypes(
        "utam.extension.pageobjects.CustomReturnType", LIST_IMPORT, COLLECTOR_IMPORT_STR);
    expected.addImportedTypes(LIST_IMPORT);
    expected.addCodeLine(
        "CustomReturnType statement0 = CustomExtensionUtils.utilityMethod1(new"
            + " UtamUtilitiesContext(this))");
    expected.addCodeLine("List<CustomReturnType> statement1 = statement0.method1()");
    expected.addCodeLine(
        "List<String> statement2 = statement1.stream().flatMap(element ->"
            + " element.method2().stream()).collect(Collectors.toList())");
    expected.addCodeLine("return statement2");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testImperativeExtensionWithVoidReturn() {
    TranslationContext context = getContext("noReturn");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName);
    expected.addParameter(new MethodParameterInfo("index", PrimitiveType.NUMBER));
    expected.addImpliedImportedTypes(UTILS_IMPORT, UTILITY_IMPORT);
    expected.addCodeLine("CustomExtensionUtils.utils(new UtamUtilitiesContext(this), index)");
    expected.addCodeLine("CustomExtensionUtils.utils(new UtamUtilitiesContext(this), index)");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }
}
