/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import static utam.compiler.grammar.UtamMethod.ERR_METHOD_SHOULD_BE_ABSTRACT;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;

/**
 * @author james.evans
 * @since 228
 */
public class UtamMethod_AbstractTests {

  private static final String METHOD_NAME = "testMethod";

  /** The getAbstractMethod method should return the proper value */
  @Test
  public void testGetAbstractMethod() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = TestUtilities.UtamEntityCreator.createUtamMethod(
        METHOD_NAME, "string", (UtamArgument[]) null);
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "String");
    PageObjectValidationTestHelper.validateMethod(method.getAbstractMethod(context), info);
  }

  @Test
  public void testGetAbstractListMethod() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = TestUtilities.UtamEntityCreator.createUtamMethod(
        METHOD_NAME, "string", (UtamArgument[]) null);
    method.isReturnList = true;
    PageObjectValidationTestHelper.MethodInfo info =
            new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "List<String>");
    PageObjectValidationTestHelper.validateMethod(method.getAbstractMethod(context), info);
  }

  /** The getAbstractMethod method with null return type should return the proper value */
  @Test
  public void testGetAbstractMethodWithNullReturnType() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = TestUtilities.UtamEntityCreator.createUtamMethod(
        METHOD_NAME, (String)null, (UtamArgument[]) null);
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "void");
    PageObjectValidationTestHelper.validateMethod(method.getAbstractMethod(context), info);
  }

  /** The getAbstractMethod method with an element return type should return the proper value */
  @Test
  public void testGetAbstractMethodWithElementReturnType() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = TestUtilities.UtamEntityCreator.createUtamMethod(
        METHOD_NAME, new String[] {"clickable"}, (UtamArgument[]) null);
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "TestMethodElement");
    PageObjectValidationTestHelper.validateMethod(method.getAbstractMethod(context), info);
  }

  /** The getAbstractMethod method with a component return type should return the proper value */
  @Test
  public void testGetAbstractMethodWithComponentReturnType() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = TestUtilities.UtamEntityCreator.createUtamMethod(
        METHOD_NAME, "utam-test/pageObjects/test/componentType", (UtamArgument[]) null);
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "ComponentType");
    PageObjectValidationTestHelper.validateMethod(method.getAbstractMethod(context), info);
  }

  /**
   * The getAbstractMethod method should throw the proper exception if a compose element is
   * specified
   */
  @Test
  public void testGetAbstractMethodWithComposeThrows() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = TestUtilities.UtamEntityCreator.createUtamMethod(
        METHOD_NAME, new UtamMethodAction[] {});
    UtamError e = expectThrows(UtamError.class, () -> method.getAbstractMethod(context));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_METHOD_SHOULD_BE_ABSTRACT, METHOD_NAME)));
  }

  /**
   * The getAbstractMethod method should throw the proper exception if a chain element is specified
   */
  @Test
  public void testGetAbstractMethodWithChainThrows() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = TestUtilities.UtamEntityCreator.createUtamMethod(
        METHOD_NAME, (String)null, (UtamArgument[]) null);
    method.chain = new UtamMethodChainLink[]{};
    UtamError e = expectThrows(UtamError.class, () -> method.getAbstractMethod(context));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_METHOD_SHOULD_BE_ABSTRACT, METHOD_NAME)));
  }

  @Test
  public void testAbstractMethod() {
    TranslationContext context = new DeserializerUtilities().getContext("abstractMethod");
    // return type that is not primitive nor a base element type
    MethodInfo methodInfo1 = new MethodInfo("returnsCustomType", "FakeType");
    PageObjectValidationTestHelper.validateMethod(
        context.getMethod("returnsCustomType"), methodInfo1);
    // return actionable
    MethodInfo methodInfo2 = new MethodInfo("returnsActionable", "ReturnsActionableElement");
    PageObjectValidationTestHelper.validateMethod(
        context.getMethod("returnsActionable"), methodInfo2);
    // return list of strings
    MethodInfo methodInfo3 = new MethodInfo("returnsListString", "List<String>");
    PageObjectValidationTestHelper.validateMethod(
        context.getMethod("returnsListString"), methodInfo3);
    // return void
    MethodInfo methodInfo4 = new MethodInfo("returnsVoid", "void");
    PageObjectValidationTestHelper.validateMethod(context.getMethod("returnsVoid"), methodInfo4);
  }

  @Test
  public void testCustomBasicElementTypes() {
    TranslationContext context = new DeserializerUtilities().getContext("customInterface");
    assertThat(
        context.getMethod("getPublicElement").getDeclaration().getReturnType().getSimpleName(),
        is(equalTo("GetPublicElementElement")));

    context = new DeserializerUtilities().getContext("customImplementation");
    assertThat(
        context.getMethod("getPublicElement").getDeclaration().getReturnType().getSimpleName(),
        is(equalTo("GetPublicElementElement")));

    assertThat(
        context.getElement("privateElement").getType().getSimpleName(),
        is(equalTo("PrivateElementElement")));
  }
}
