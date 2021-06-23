/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static utam.compiler.translator.TranslationUtilities.EMPTY_COMMENTS;

import java.util.Collections;
import org.testng.annotations.Test;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Actionable;

/**
 * tests for the InterfaceMethod representation class
 *
 * @author james.evans
 */
public class InterfaceMethodTests {

  private static MethodContext getMethodContext(TypeProvider typeProvider, boolean isReturnList) {
    return new MethodContext("testMethod", typeProvider, isReturnList);
  }

  /**
   * An InterfaceMethod object should be able to be be created
   */
  @Test
  public void testInterfaceMethodReturnsActionableElement() {
    MethodInfo info = new MethodInfo("testMethod", "Actionable");
    TypeProvider returnType = new TypeUtilities.FromClass(Actionable.class);
    InterfaceMethod method =
        new InterfaceMethod(getMethodContext(returnType, false), Collections.emptyList(), "", false);
    PageObjectValidationTestHelper.validateMethod(method, info);
    assertThat(method.getClassImports(), hasSize(1));
  }

  @Test
  public void testInterfaceMethodReturnsList() {
    TypeProvider returnType = new TypeUtilities.FromString("SomeReturnType");
    InterfaceMethod method = new InterfaceMethod(getMethodContext(returnType, true),
        Collections.emptyList(), "", false);
    assertThat(method.getCodeLines().isEmpty(), is(true));
    assertThat(method.getClassImports(), hasSize(2));
    assertThat(method.getClassImports().get(0).getSimpleName(), is(equalTo("List")));
    assertThat(method.getClassImports().get(1).getSimpleName(), is(equalTo("SomeReturnType")));
    assertThat(method.getDeclaration().getCodeLine(),
        is(equalTo("List<SomeReturnType> testMethod()")));
  }

  @Test
  public void testInterfaceMethodWithParameters() {
    TypeProvider returnType = PrimitiveType.STRING;
    InterfaceMethod method = new InterfaceMethod(getMethodContext(returnType, false),
        Collections.singletonList(
            new ParameterUtils.Regular("name", new TypeUtilities.FromString("Type"))),
        EMPTY_COMMENTS, false);
    assertThat(method.getCodeLines().isEmpty(), is(true));
    assertThat(method.getClassImports(), hasSize(2));
    assertThat(method.getClassImports().get(0).getSimpleName(), is(equalTo("Type")));
    assertThat(method.getClassImports().get(1).getSimpleName(), is(equalTo("String")));
    assertThat(method.getDeclaration().getCodeLine(), is(equalTo("String testMethod(Type name)")));
  }
}
