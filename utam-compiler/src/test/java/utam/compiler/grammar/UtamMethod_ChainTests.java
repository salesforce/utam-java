/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ChainMethod;
import utam.core.declarative.representation.PageObjectMethod;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import static utam.compiler.grammar.TestUtilities.*;
import static utam.compiler.grammar.UtamMethod.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

/**
 * @author james.evans
 * @since 228
 */
public class UtamMethod_ChainTests {

  private static final String METHOD_NAME = "testMethod";

  private static UtamMethodChainLink getChainLink(
      String elementName, String typeName, boolean isList) {
    return new UtamMethodChainLink(
        elementName, isList, String.format("utam-test/pageObjects/test/%s", typeName));
  }

  private static UtamMethodChainLink getFirstTestChainLink(String elementName, boolean isList) {
    return new UtamMethodChainLink(elementName, isList, TEST_URI);
  }

  private static String getErr(String message) {
    return String.format(message, METHOD_NAME);
  }

  /** The getChainMethod method should return the proper value */
  @Test
  public void testGetChainMethod() {
    UtamPageObject object = new UtamPageObject();
    TranslationContext context = getTestTranslationContext();
    object.elements =
        new UtamElement[] {new UtamElement("firstElement", TEST_URI, new UtamSelector("css"))};
    object.compile(context);
    UtamMethod method =
        new UtamMethod(
            METHOD_NAME,
            null,
            new UtamMethodChainLink[] {
              getFirstTestChainLink("firstElement", false),
              getChainLink("secondElement", "SecondWrapper", false),
            });
    PageObjectValidationTestHelper.MethodInfo methodInfo =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "SecondWrapper");
    methodInfo.addCodeLine(getElementPrivateMethodCalled("firstElement") + "().getSecondElement()");
    PageObjectMethod methodObject = method.getMethod(context);
    assertThat(methodObject, is(instanceOf(ChainMethod.class)));
    PageObjectValidationTestHelper.validateMethod(methodObject, methodInfo);
    PageObjectValidationTestHelper.validateMethod(method.getChainMethod(context), methodInfo);
  }

  @Test
  public void testChainMethodWithList() {
    UtamPageObject object = new UtamPageObject();
    TranslationContext context = getTestTranslationContext();
    object.elements =
        new UtamElement[] {
          new UtamElement("firstElement", TEST_URI, new UtamSelector("css", true))
        };
    object.compile(context);
    UtamMethod method =
        new UtamMethod(
            METHOD_NAME,
            null,
            new UtamMethodChainLink[] {
              getFirstTestChainLink("firstElement", true),
              getChainLink("secondElement", "SecondWrapper", false),
            });
    PageObjectValidationTestHelper.MethodInfo methodInfo =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "List<SecondWrapper>");
    methodInfo.addCodeLine(
        getElementPrivateMethodCalled("firstElement")
            + "().stream().map(element -> element.getSecondElement()).collect(Collectors.toList())");
    PageObjectMethod methodObject = method.getMethod(context);
    PageObjectValidationTestHelper.validateMethod(methodObject, methodInfo);
    PageObjectValidationTestHelper.validateMethod(method.getChainMethod(context), methodInfo);
    assertThat(methodObject.getClassImports(), hasSize(3));
    assertThat(
        methodObject.getDeclaration().getCodeLine(),
        is(equalTo("List<SecondWrapper> testMethod()")));
  }

  @Test
  public void testChainMethodWithListOfLists() {
    UtamPageObject object = new UtamPageObject();
    TranslationContext context = getTestTranslationContext();
    object.elements =
        new UtamElement[] {
          new UtamElement("firstElement", TEST_URI, new UtamSelector("css", true))
        };
    object.compile(context);
    UtamMethod method =
        new UtamMethod(
            METHOD_NAME,
            null,
            new UtamMethodChainLink[] {
              getFirstTestChainLink("firstElement", true),
              getChainLink("secondElement", "SecondWrapper", true),
            });
    PageObjectValidationTestHelper.MethodInfo methodInfo =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "List<SecondWrapper>");
    methodInfo.addCodeLine(
        getElementPrivateMethodCalled("firstElement")
            + "().stream().flatMap(element -> element.getSecondElement().stream()).collect(Collectors.toList())");
    PageObjectMethod methodObject = method.getMethod(context);
    PageObjectValidationTestHelper.validateMethod(methodObject, methodInfo);
    PageObjectValidationTestHelper.validateMethod(method.getChainMethod(context), methodInfo);
  }

  /** The getChainMethod method should throw the proper exception if a return type is specified */
  @Test
  public void testGetChainMethodWithReturnTypeThrows() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = new UtamMethod(METHOD_NAME, null, new UtamMethodChainLink[] {});
    method.returnStr = "invalid";
    UtamError e = expectThrows(UtamError.class, () -> method.getChainMethod(context));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_METHOD_RETURN_TYPE_REDUNDANT, METHOD_NAME)));
  }

  /** The getChainMethod method should throw the proper exception if arguments specified */
  @Test
  public void testGetChainMethodWithArgumentsThrows() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = new UtamMethod(METHOD_NAME, null, new UtamMethodChainLink[] {});
    method.args = new UtamArgument[0];
    UtamError e = expectThrows(UtamError.class, () -> method.getChainMethod(context));
    assertThat(e.getMessage(), containsString(getErr(ERR_ARGS_NOT_ALLOWED)));
  }

  /** The getChainMethod method should throw the proper exception if arguments specified */
  @Test
  public void testGetChainMethodWithEmptyStatementListThrows() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = new UtamMethod(METHOD_NAME, null, new UtamMethodChainLink[] {});
    UtamError e = expectThrows(UtamError.class, () -> method.getChainMethod(context));
    assertThat(e.getMessage(), containsString(getErr(ERR_METHOD_EMPTY_STATEMENTS)));
  }

  /**
   * The getMethod method should throw the appropriate exception for a method type that cannot be
   * determined
   */
  @Test
  public void testGetMethodForUnknownMethodTypeThrows() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = new UtamMethod(METHOD_NAME, null);
    UtamError e = expectThrows(UtamError.class, () -> method.getMethod(context));
    assertThat(e.getMessage(), containsString(getErr(ERR_METHOD_UNKNOWN_TYPE)));
  }

  @Test
  public void testChainWithListThrows() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = new UtamMethod(METHOD_NAME, null, new UtamMethodChainLink[] {});
    method.isReturnList = true;
    UtamError e = expectThrows(UtamError.class, () -> method.getMethod(context));
    assertThat(e.getMessage(), containsString(getErr(ERR_METHOD_RETURN_ALL_REDUNDANT)));
  }

  @Test
  public void testChainRedundantCompose() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = new UtamMethod(METHOD_NAME, null, new UtamMethodChainLink[] {});
    method.compose = new UtamMethodAction[0];
    UtamError e = expectThrows(UtamError.class, () -> method.getMethod(context));
    assertThat(e.getMessage(), containsString(getErr(ERR_METHOD_REDUNDANT_TYPE)));
  }
}
