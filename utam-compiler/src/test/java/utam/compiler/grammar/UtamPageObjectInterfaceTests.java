/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyIterable;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

import org.testng.annotations.Test;
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

  private static Result getPageObject(String fileName) {
    return new DeserializerUtilities().getResultFromFile("interface/" + fileName);
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
