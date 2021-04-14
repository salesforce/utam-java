/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.UtilityMethod;
import org.testng.annotations.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamMethod_UtilityTests {

  /** The getMethod method should return a valid value for a utility method */
  @Test
  public void testGetMethodForUtilityMethod() {
    final String METHOD_NAME = "testMethod";
    final String APPLY_METHOD_NAME = "utilityMethod";
    final String UTILITY_TYPE = "utam-test/utils/test/utilityMethodClass";
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod utamMethod =
        new UtamMethod(
            METHOD_NAME,
            "string",
            new UtamMethodUtil(UTILITY_TYPE, APPLY_METHOD_NAME, new UtamArgument[] {}),
            null);
    PageObjectValidationTestHelper.MethodInfo methodInfo =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "String");
    methodInfo.addCodeLine(String.format("this.utilUtilityMethodClass.%s()", APPLY_METHOD_NAME));
    PageObjectMethod methodObject = utamMethod.getMethod(context);
    assertThat(methodObject, is(instanceOf(UtilityMethod.class)));
    PageObjectValidationTestHelper.validateMethod(methodObject, methodInfo);
  }

  /**
   * The getMethod method should return a valid value for a utility method explicitly not
   * returning a list
   */
  @Test
  public void testGetMethodForUtilityMethodNotReturningList() {
    final String METHOD_NAME = "testMethod";
    final String APPLY_METHOD_NAME = "utilityMethod";
    final String UTILITY_TYPE = "utam-test/utils/test/utilityMethodClass";
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod utamMethod =
        new UtamMethod(
            METHOD_NAME,
            "string",
            new UtamMethodUtil(UTILITY_TYPE, APPLY_METHOD_NAME, new UtamArgument[] {}),
            false);
    PageObjectValidationTestHelper.MethodInfo methodInfo =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "String");
    methodInfo.addCodeLine(String.format("this.utilUtilityMethodClass.%s()", APPLY_METHOD_NAME));
    PageObjectMethod methodObject = utamMethod.getMethod(context);
    assertThat(methodObject, is(instanceOf(UtilityMethod.class)));
    PageObjectValidationTestHelper.validateMethod(methodObject, methodInfo);
  }

  /** The getMethod method should return a valid value for a utility method returning a list */
  @Test
  public void testGetMethodForUtilityMethodReturningList() {
    final String METHOD_NAME = "testMethod";
    final String APPLY_METHOD_NAME = "utilityMethod";
    final String UTILITY_TYPE = "utam-test/utils/test/utilityMethodClass";
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod utamMethod =
        new UtamMethod(
            METHOD_NAME,
            "string",
            new UtamMethodUtil(UTILITY_TYPE, APPLY_METHOD_NAME, new UtamArgument[] {}),
            true);
    PageObjectValidationTestHelper.MethodInfo methodInfo =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "List<String>");
    methodInfo.addCodeLine(String.format("this.utilUtilityMethodClass.%s()", APPLY_METHOD_NAME));
    PageObjectMethod methodObject = utamMethod.getMethod(context);
    assertThat(methodObject, is(instanceOf(UtilityMethod.class)));
    PageObjectValidationTestHelper.validateMethod(methodObject, methodInfo);
  }

  /** The getMethod method should return a valid value for a utility method returning a list */
  @Test
  public void testGetMethodForUtilityMethodWithDeclaredChainMethodThrows() {
    final String METHOD_NAME = "testMethod";
    final String APPLY_METHOD_NAME = "utilityMethod";
    final String UTILITY_TYPE = "utam-test/utils/test/utilityMethodClass";
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod utamMethod =
        new UtamMethod(
            METHOD_NAME,
            null,
            new UtamMethodChainLink[] {},
            new UtamMethodUtil(UTILITY_TYPE, APPLY_METHOD_NAME, new UtamArgument[] {}),
            null,
            "string",
            null);
    UtamError e = expectThrows(
        UtamError.class,
        () -> utamMethod.getMethod(context)
    );
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(UtamMethod.ERR_METHOD_REDUNDANT_TYPE, METHOD_NAME))));
  }

  /** The getMethod method should return a valid value for a utility method returning a list */
  @Test
  public void testGetMethodForUtilityMethodWithDeclaredComposeMethodThrows() {
    final String METHOD_NAME = "testMethod";
    final String APPLY_METHOD_NAME = "utilityMethod";
    final String UTILITY_TYPE = "utam-test/utils/test/utilityMethodClass";
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod utamMethod =
        new UtamMethod(
            METHOD_NAME,
            new UtamMethodAction[] {},
            null,
            new UtamMethodUtil(UTILITY_TYPE, APPLY_METHOD_NAME, new UtamArgument[] {}),
            null,
            "string",
            null);
    UtamError e = expectThrows(
        UtamError.class,
        () -> utamMethod.getMethod(context)
    );
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(UtamMethod.ERR_METHOD_REDUNDANT_TYPE, METHOD_NAME))));
  }

}
