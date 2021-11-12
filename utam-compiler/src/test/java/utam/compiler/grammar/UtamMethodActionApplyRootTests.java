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
import static org.testng.Assert.expectThrows;
import static utam.compiler.helpers.ActionableActionType.ERR_UNKNOWN_ACTION;
import static utam.compiler.helpers.BasicElementInterface.clickable;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;

import org.testng.annotations.Test;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.element.BasicElement;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.element.BasePageElement;

/**
 * Tests for composing root element actions and defining root element type
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamMethodActionApplyRootTests {

  private static final String TEST_METHOD_NAME = "test";
  private static final String ROOT_METHOD_NAME = "getRoot";

  private static TranslationContext getContext(String json) {
    return new DeserializerUtilities()
        .getContext("compose/root/" + json);
  }

  @Test
  public void testPrivateRootNoTypeBasicAction() {
    TranslationContext context = getContext("rootContains");
    PageObjectMethod actualMethod = context.getMethod(TEST_METHOD_NAME);
    MethodInfo expected = new MethodInfo(TEST_METHOD_NAME, "Boolean");
    expected.addImpliedImportedTypes(SELECTOR.getFullName());
    expected.addImpliedImportedTypes(BasePageElement.class.getName());
    expected.addCodeLine("BasePageElement root0 = this.getRootElement()");
    expected.addCodeLine(
        "Boolean statement0 = root0.containsElement(LocatorBy.byCss(String.format(\".foo[title='%s']\", title)), false)");
    expected.addCodeLine("return statement0");
    expected.addParameter(new MethodParameterInfo("title"));
    PageObjectValidationTestHelper.validateMethod(actualMethod, expected);
  }

  @Test
  public void testPublicRootNoTypeGetText() {
    TranslationContext context = getContext("publicRootNoType");
    PageObjectMethod actualMethod = context.getMethod(TEST_METHOD_NAME);
    MethodInfo expected = new MethodInfo(TEST_METHOD_NAME, "String");
    Class returnType = BasicElement.class;
    expected.addImpliedImportedTypes(returnType.getName());
    expected.addCodeLine("BasicElement root0 = this.getRoot()");
    expected.addCodeLine("String statement0 = root0.getText()");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(actualMethod, expected);

    PageObjectMethod rootMethod = context.getMethod(ROOT_METHOD_NAME);
    MethodInfo expectedRootMethod = new MethodInfo(ROOT_METHOD_NAME, returnType.getSimpleName());
    expectedRootMethod.addImpliedImportedTypes(returnType.getName());
    expectedRootMethod.addCodeLine("return this.getRootElement()");
    PageObjectValidationTestHelper.validateMethod(rootMethod, expectedRootMethod);
  }

  @Test
  public void testPublicRootNoTypeClickThrows() {
    String expectedError = String.format(ERR_UNKNOWN_ACTION, "click", "root", "basic type");
    UtamError e = expectThrows(UtamError.class,
        () -> getContext("publicRootWrongAction"));
    assertThat(e.getMessage(), containsString(expectedError));
  }

  @Test
  public void testPrivateRootWrongTypeClickThrows() {
    String expectedError = String
        .format(ERR_UNKNOWN_ACTION, "click", "root", "declared interfaces [ editable ]");
    UtamError e = expectThrows(UtamError.class,
        () -> getContext("privateRootWrongAction"));
    assertThat(e.getMessage(), containsString(expectedError));
  }

  @Test
  public void testPublicRootWithTypeClick() {
    TranslationContext context = getContext("publicRootWithType");
    PageObjectMethod actualMethod = context.getMethod(TEST_METHOD_NAME);
    MethodInfo expected = new MethodInfo(TEST_METHOD_NAME);
    expected.addCodeLine("RootElement root0 = this.getRoot()");
    expected.addCodeLine("root0.click()");
    PageObjectValidationTestHelper.validateMethod(actualMethod, expected);

    PageObjectMethod rootMethod = context.getMethod(ROOT_METHOD_NAME);
    MethodInfo expectedRootMethod = new MethodInfo(ROOT_METHOD_NAME, "RootElement");
    expectedRootMethod.addCodeLine("return getProxy(this.getRootElement(), RootElement.class)");
    PageObjectValidationTestHelper.validateMethod(rootMethod, expectedRootMethod);
  }

  @Test
  public void testPrivateRootWithTypeClick() {
    TranslationContext context = getContext("privateRootWithType");
    PageObjectMethod actualMethod = context.getMethod(TEST_METHOD_NAME);
    MethodInfo expected = new MethodInfo(TEST_METHOD_NAME);
    expected.addCodeLine("RootElement root0 = this.getRoot()");
    expected.addCodeLine("root0.click()");
    PageObjectValidationTestHelper.validateMethod(actualMethod, expected);

    PageObjectMethod rootMethod = context.getMethod(ROOT_METHOD_NAME);
    assertThat(rootMethod.isPublic(), is(false));
    MethodInfo expectedRootMethod = new MethodInfo(ROOT_METHOD_NAME, "RootElement");
    expectedRootMethod.setNotPublic();
    expectedRootMethod.addImpliedImportedTypes(clickable.getFullName());
    expectedRootMethod.addCodeLine("return getProxy(this.getRootElement(), RootElement.class)");
    PageObjectValidationTestHelper.validateMethod(rootMethod, expectedRootMethod);
  }
}
