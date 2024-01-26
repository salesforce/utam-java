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
import static org.hamcrest.Matchers.emptyIterable;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;

import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.DeserializerUtilities.Result;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageObjectMethod;

/**
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamPageObjectInterfaceTests {

  private static final String methodName = "getRoot";

  private static String getErrUnsupportedProperties(String property) {
    return String.format(
        "error 8: interface: property \"%s\" is not supported, supported are: root, interface,"
            + " methods, type, exposeRootElement",
        property);
  }

  private static Result getPageObject(String fileName) {
    return new DeserializerUtilities().getResultFromFile("interface/" + fileName);
  }

  private static void expectError(String partialJson) {
    new DeserializerUtilities()
        .getResultFromString(String.format("{ %s \"interface\" : true }", partialJson));
  }

  @Test
  public void testAbstractWithNonNullElementsThrows() {
    String partialJson =
        "\"elements\" : [{ \"name\" : \"name\", \"selector\" : {\"css\": \"css\"}}], ";
    Exception e = expectThrows(UtamCompilationError.class, () -> expectError(partialJson));
    assertThat(e.getMessage(), containsString(getErrUnsupportedProperties("elements")));
  }

  @Test
  public void testAbstractWithPlatformThrows() {
    String partialJson = "\"platform\" : \"web\", ";
    Exception e = expectThrows(UtamCompilationError.class, () -> expectError(partialJson));
    assertThat(e.getMessage(), containsString(getErrUnsupportedProperties("platform")));
  }

  @Test
  public void testAbstractWithProfileThrows() {
    String partialJson =
        "\"profile\": [\n" + "    {\n" + "      \"name\": \"value\"\n" + "    }\n" + "  ],";
    Exception e = expectThrows(UtamCompilationError.class, () -> expectError(partialJson));
    assertThat(
        e.getMessage(),
        containsString(
            "error 805: \"profile\" can only be set for a page object that implements an interface,"
                + " please add \"implements\" property"));
  }

  @Test
  public void testAbstractWithSelectorThrows() {
    String partialJson = "\"selector\" : { \"css\" : \"css\" }, ";
    Exception e = expectThrows(UtamCompilationError.class, () -> expectError(partialJson));
    assertThat(e.getMessage(), containsString(getErrUnsupportedProperties("selector")));
  }

  @Test
  public void testAbstractWithShadowThrows() {
    String partialJson =
        "\"shadow\" : { \"elements\" : [{ \"name\" : \"name\", \"selector\" : {\"css\": \"css\"}}]"
            + " }, ";
    Exception e = expectThrows(UtamCompilationError.class, () -> expectError(partialJson));
    assertThat(e.getMessage(), containsString(getErrUnsupportedProperties("shadow")));
  }

  @Test
  public void testAbstractWithBeforeLoadThrows() {
    String partialJson = "\"beforeLoad\" : [{ \"element\": \"root\" }], ";
    Exception e = expectThrows(UtamCompilationError.class, () -> expectError(partialJson));
    assertThat(e.getMessage(), containsString(getErrUnsupportedProperties("beforeLoad")));
  }

  @Test
  public void testInterfaceExposesRootWithoutTypes() {
    Result result = getPageObject("exposeRootNoTypes");
    PageObjectMethod method = result.getContext().getMethod(methodName);
    MethodDeclaration declaration = method.getDeclaration();
    assertThat(declaration.getName(), is(equalTo(methodName)));
    assertThat(declaration.getParameters(), is(emptyIterable()));
    assertThat(declaration.getReturnType().getSimpleName(), is(equalTo("BasicElement")));
  }

  @Test
  public void testExposedRootNoTypesImplemented() {
    Result result = getPageObject("exposeRootNoTypesImpl");
    PageObjectMethod actualMethod = result.getContext().getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName, "BasicElement");
    methodInfo.addCodeLine("return this.getRootElement()");
    PageObjectValidationTestHelper.validateMethod(actualMethod, methodInfo);
  }

  @Test
  public void testInterfaceExposeRootWithTypes() {
    Result result = getPageObject("exposeRoot");
    PageObjectMethod method = result.getContext().getMethod(methodName);
    MethodDeclaration declaration = method.getDeclaration();
    assertThat(declaration.getName(), is(equalTo(methodName)));
    assertThat(declaration.getParameters(), is(emptyIterable()));
    assertThat(declaration.getReturnType().getSimpleName(), is(equalTo("RootElement")));
  }

  @Test
  public void testExposedRootWithTypesImplemented() {
    Result result = getPageObject("exposeRootImpl");
    PageObjectMethod actualMethod = result.getContext().getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName, "RootElement");
    methodInfo.addCodeLine("return getProxy(this.getRootElement(), RootElement.class)");
    PageObjectValidationTestHelper.validateMethod(actualMethod, methodInfo);
  }
}
