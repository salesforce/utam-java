/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.UtamMethod.ERR_METHOD_SHOULD_BE_ABSTRACT;

import java.util.function.Function;
import org.testng.annotations.Test;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;

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
    UtamMethod method = TestUtilities.UtamEntityCreator.createUtamMethod(METHOD_NAME, "string", null);
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "String");
    PageObjectValidationTestHelper.validateMethod(method.getAbstractMethod(context), info);
  }

  /** The getAbstractMethod method with null return type should return the proper value */
  @Test
  public void testGetAbstractMethodWithNullReturnType() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = TestUtilities.UtamEntityCreator.createUtamMethod(METHOD_NAME, null, null);
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "void");
    PageObjectValidationTestHelper.validateMethod(method.getAbstractMethod(context), info);
  }

  /** The getAbstractMethod method with a component return type should return the proper value */
  @Test
  public void testGetAbstractMethodWithComponentReturnType() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = TestUtilities.UtamEntityCreator.createUtamMethod(
        METHOD_NAME, "utam-test/pageObjects/test/componentType", null);
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

  @Test
  public void testAbstractMethodReturnTypes() {
    TranslationContext context = new DeserializerUtilities().getContext("interface/abstract");

    // return type that is not primitive nor a base element type
    String methodName = "returnsCustomType";
    PageObjectMethod actualMethod = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "FakeType");
    expected.addImportedTypes("utam.fake.pageobjects.test.FakeType");
    PageObjectValidationTestHelper.validateMethod(actualMethod, expected);

    // return actionable
    methodName = "returnsActionable";
    actualMethod = context.getMethod(methodName);
    expected = new MethodInfo(methodName, "ReturnsActionableElement");
    PageObjectValidationTestHelper.validateMethod(actualMethod, expected);

    // return list of strings
    methodName = "returnsListString";
    actualMethod = context.getMethod(methodName);
    expected = new MethodInfo(methodName, "List<String>");
    expected.addImportedTypes(TypeUtilities.LIST_IMPORT.getFullName());
    PageObjectValidationTestHelper.validateMethod(actualMethod, expected);

    // return void
    methodName = "returnsVoid";
    actualMethod = context.getMethod(methodName);
    expected = new MethodInfo(methodName, "void");
    PageObjectValidationTestHelper.validateMethod(actualMethod, expected);
  }

  @Test
  public void testCustomBasicElementTypes() {
    Function<PageObjectMethod, String> getReturnTypeName = method -> method.getDeclaration()
        .getReturnType().getSimpleName();
    TranslationContext context = new DeserializerUtilities().getContext("interface/customInterface");
    assertThat(
        getReturnTypeName.apply(context.getMethod("getPublicElement")),
        is(equalTo("GetPublicElementElement")));

    context = new DeserializerUtilities().getContext("interface/customImplementation");
    assertThat(
        getReturnTypeName.apply(context.getMethod("getPublicElement")),
        is(equalTo("GetPublicElementElement")));

    assertThat(
        context.getElement("privateElement").getType().getSimpleName(),
        is(equalTo("PrivateElementElement")));
  }
}
