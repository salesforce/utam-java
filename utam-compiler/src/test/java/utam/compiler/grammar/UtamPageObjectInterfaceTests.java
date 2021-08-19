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
import static utam.compiler.helpers.TypeUtilities.ROOT_ELEMENT_TYPE;

import org.testng.annotations.Test;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageObjectMethod;

/**
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamPageObjectInterfaceTests {

  @Test
  public void testNotNullMatcherForPrivateGetter() {
    TranslationContext context = new DeserializerUtilities().getContext("interfaceExposeRoot");
    String methodName = "getRoot";
    PageObjectMethod method = context.getMethod(methodName);
    MethodDeclaration declaration = method.getDeclaration();
    assertThat(method.isPublic(), is(true));
    assertThat(declaration.getName(), is(equalTo(methodName)));
    assertThat(declaration.getParameters(), is(emptyIterable()));
    assertThat(declaration.getReturnType().isSameType(ROOT_ELEMENT_TYPE), is(true));
  }
}
