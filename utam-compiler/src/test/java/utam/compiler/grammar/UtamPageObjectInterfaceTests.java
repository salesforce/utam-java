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
import utam.compiler.helpers.TranslationContext;
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

  @Test
  public void testInterfaceExposesRootWithTypes() {
    TranslationContext context = new DeserializerUtilities()
        .getContext("interface/exposeRootWithTypes");
    PageObjectMethod method = context.getMethod(methodName);
    MethodDeclaration declaration = method.getDeclaration();
    assertThat(method.isPublic(), is(true));
    assertThat(declaration.getName(), is(equalTo(methodName)));
    assertThat(declaration.getParameters(), is(emptyIterable()));
    assertThat(declaration.getReturnType().getFullName(), is(equalTo("RootElement")));
  }

  @Test
  public void testInterfaceExposesRoot() {
    TranslationContext context = new DeserializerUtilities().getContext("interface/exposeRoot");
    PageObjectMethod method = context.getMethod(methodName);
    MethodDeclaration declaration = method.getDeclaration();
    assertThat(method.isPublic(), is(true));
    assertThat(declaration.getName(), is(equalTo(methodName)));
    assertThat(declaration.getParameters(), is(emptyIterable()));
    assertThat(declaration.getReturnType().getSimpleName(), is(equalTo("BasicElement")));
  }

  @Test
  public void testExposedRootImplemented() {
    TranslationContext context = new DeserializerUtilities().getContext("interface/exposeRootImplemented");
    PageObjectMethod actualMethod = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName, "RootElement");
    methodInfo.addCodeLine("return proxy(this.getRootElement(), RootElement.class)");
    PageObjectValidationTestHelper.validateMethod(actualMethod, methodInfo);
  }
}
