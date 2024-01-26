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
import static org.hamcrest.Matchers.hasSize;
import static org.testng.Assert.expectThrows;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.JsonBuilderTestUtility;
import utam.compiler.UtamCompilationError;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.framework.consumer.UtamError;

/**
 * tests for method comments in a page object
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public class UtamMethodDescriptionTests {

  private static List<String> getMethodDescription(String jsonFile, String methodName) {
    return getMethodDeclaration(jsonFile, methodName).getDescription();
  }

  private static MethodDeclaration getMethodDeclaration(String jsonFile, String methodName) {
    return new DeserializerUtilities().getContext(jsonFile).getMethod(methodName).getDeclaration();
  }

  @Test
  public void testDescriptionStringForComposeMethod() {
    List<String> description =
        getMethodDescription("generated/comments/verboseString.utam", "myPublicMethod");
    assertThat(description, hasSize(3));
    assertThat(description.get(0), containsString("public method description"));
    assertThat(description.get(1), containsString("@return String"));
    assertThat(description.get(2), containsString("@param attrName attribute name to get"));
  }

  @Test
  public void testDescriptionStringForInterfaceComposeMethod() {
    List<String> description =
        getMethodDescription("generated/comments/verboseInterface.utam", "myPublicMethod");
    assertThat(description, hasSize(3));
    assertThat(description.get(0), containsString("public method description"));
    assertThat(description.get(1), containsString("@return String"));
    assertThat(description.get(2), containsString("@param attrName attribute name to get"));
  }

  @Test
  public void testDescriptionStringForCustomElementMethod() {
    List<String> description =
        getMethodDescription("generated/comments/verboseString.utam", "getCustomPublic");
    assertThat(description, hasSize(3));
    assertThat(description.get(0), containsString("description"));
    assertThat(description.get(1), containsString("@return VerboseObject"));
    assertThat(
        description.get(2), containsString("@param selectorArg selector parameter description"));
  }

  @Test
  public void testDescriptionStringForBasicElementMethod() {
    List<String> description =
        getMethodDescription("generated/comments/verboseString.utam", "getBasicPrivateElement");
    assertThat(description, hasSize(3));
    assertThat(description.get(0), containsString("description"));
    assertThat(description.get(1), containsString("@return BasicElement"));
    assertThat(description.get(2), containsString("@param selectorArg String"));
  }

  @Test
  public void testDescriptionStringForContainerMethod() {
    List<String> description =
        getMethodDescription("generated/comments/verboseString.utam", "getContainerElement");
    assertThat(description, hasSize(3));
    assertThat(description.get(0), containsString("description"));
    assertThat(description.get(1), containsString("@return T"));
    assertThat(description.get(2), containsString("@param pageObjectType Class<T>"));
  }

  @Test
  public void testDescriptionObjectForComposeMethod() {
    List<String> description =
        getMethodDescription("generated/comments/verboseObject.utam", "myPublicMethod");
    assertThat(description, hasSize(5));
    assertThat(description.get(0), containsString("one"));
    assertThat(description.get(1), containsString("two"));
    assertThat(description.get(2), containsString("@return return something"));
    assertThat(description.get(3), containsString("@param attrName attribute name to get"));
    assertThat(description.get(4), containsString("@throws NullPointerException when"));
  }

  @Test
  public void testDescriptionObjectForCustomGetter() {
    MethodDeclaration declaration =
        getMethodDeclaration("generated/comments/verboseObject.utam", "getCustomPublic");
    assertThat(declaration.isDeprecated(), equalTo(true));
    List<String> description = declaration.getDescription();
    assertThat(description, hasSize(5));
    assertThat(description.get(0), containsString("description"));
    assertThat(description.get(1), containsString("@return return something"));
    assertThat(
        description.get(2), containsString("@param selectorArg selector parameter description"));
    assertThat(description.get(3), containsString("@throws NullPointerException when"));
    assertThat(description.get(4), containsString("@deprecated this element is outdated"));
  }

  @Test
  public void testDescriptionObjectForContainer() {
    List<String> description =
        getMethodDescription("generated/comments/verboseObject.utam", "getContainer");
    assertThat(description, hasSize(5));
    assertThat(description.get(0), containsString("description"));
    assertThat(description.get(1), containsString("@return return something"));
    assertThat(
        description.get(2), containsString("@param selectorArg selector parameter description"));
    assertThat(description.get(3), containsString("@param pageObjectType Class<T>"));
    assertThat(description.get(4), containsString("@throws NullPointerException when"));
  }

  @Test
  public void testDescriptionForArgsReference() {
    List<String> description =
        getMethodDescription("generated/comments/verboseObject.utam", "myArgsReference");
    assertThat(description, hasSize(3));
    assertThat(description.get(0), containsString("method myArgsReference"));
    assertThat(description.get(1), containsString("@return String"));
    assertThat(description.get(2), containsString("@param attrName attribute name to get"));
  }

  @Test
  public void testFormatIsNotStringOrObjectForMethodThrows() {
    JsonBuilderTestUtility testUtility = new JsonBuilderTestUtility();
    testUtility.addRawString("interface", "true");
    testUtility.addRawString("methods", "[{\"name\": \"test\", \"description\": true}]");
    UtamError e = expectThrows(UtamError.class, testUtility::getDeserializedJson);
    assertThat(
        e.getMessage(),
        containsString(
            "error 700: method \"test\" description: format of the description is incorrect"));
  }

  @Test
  public void testFormatIsNotStringOrObjectForElementThrows() {
    JsonBuilderTestUtility test = new JsonBuilderTestUtility();
    test.addRawString(
        "elements",
        "[{\"name\": \"test\", \"description\": true, \"selector\": {\"css\": \"css\"}}]");
    Exception e = test.expectCompilerError();
    assertThat(
        e.getMessage(),
        containsString(
            "error 700: element \"test\" description: format of the description is incorrect"));
  }

  @Test
  public void testDescriptionObjectForInterface() {
    MethodDeclaration declaration =
        getMethodDeclaration("generated/comments/verboseInterface.utam", "getCustomPublic");
    assertThat(declaration.isDeprecated(), equalTo(true));
    List<String> description = declaration.getDescription();
    assertThat(description, hasSize(5));
    assertThat(description.get(0), containsString("description"));
    assertThat(description.get(1), containsString("@return return something"));
    assertThat(
        description.get(2), containsString("@param selectorArg selector parameter description"));
    assertThat(description.get(3), containsString("@throws NullPointerException when"));
    assertThat(description.get(4), containsString("@deprecated this element is outdated"));
  }

  @Test
  public void testDescriptionTextEmptyStringThrows() {
    Exception e =
        expectThrows(
            UtamCompilationError.class,
            () -> getMethodDeclaration("validate/method_comments/empty_text", "test"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 10: method \"test\" description: property \"text\" should be a non empty string,"
                + " instead found empty"));
  }

  @Test
  public void testDescriptionEmptyTextArrayThrows() {
    Exception e =
        expectThrows(
            UtamCompilationError.class,
            () -> getMethodDeclaration("validate/method_comments/empty_text_array", "test"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 12: method \"test\" description: property \"text\" should be a non-empty"
                + " array"));
  }

  @Test
  public void testDescriptionEmptyStringThrows() {
    Exception e =
        expectThrows(
            UtamCompilationError.class,
            () -> getMethodDeclaration("validate/method_comments/empty_string", "test"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 10: element \"test\" description: property \"text\" should be a non empty"
                + " string, instead found empty"));
  }

  @Test
  public void testEmptyReturnThrows() {
    Exception e =
        expectThrows(
            UtamCompilationError.class,
            () -> getMethodDeclaration("validate/method_comments/empty_return", "test"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 10: method \"test\" description: property \"return\" should be a non empty"
                + " string, instead found empty"));
  }

  @Test
  public void testEmptyThrowsThrows() {
    Exception e =
        expectThrows(
            UtamCompilationError.class,
            () -> getMethodDeclaration("validate/method_comments/empty_throws", "test"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 10: element \"test\" description: property \"throws\" should be a non empty"
                + " string, instead found number"));
  }
}
