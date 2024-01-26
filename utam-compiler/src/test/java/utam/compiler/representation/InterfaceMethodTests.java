/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyIterable;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.hasSize;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageObjectClass;
import utam.core.declarative.representation.PageObjectInterface;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * tests for interface methods
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class InterfaceMethodTests {

  private static PageObjectInterface getInterface(String fileName) {
    return new DeserializerUtilities()
        .getResultFromFile("interface/" + fileName)
        .getPageObject()
        .getInterface();
  }

  private static MethodDeclaration getMethod(PageObjectInterface result) {
    assertThat(result.getDeclaredApi(), hasSize(1));
    return result.getDeclaredApi().get(0);
  }

  private static void assertListImport(TypeProvider typeProvider) {
    assertThat(typeProvider.getFullName(), is(List.class.getName()));
  }

  @Test
  public void testMethodReturnsListOfBasicElements() {
    PageObjectInterface result = getInterface("returnListOfBasicElements");
    MethodDeclaration method = getMethod(result);
    assertThat(method.getCodeLine(), is(equalTo("List<GetTestElement> getTest()")));
    assertThat(method.getImports(), hasSize(1));
    assertListImport(method.getImports().get(0));
    assertThat(result.getUnionTypes(), hasSize(1));
    assertThat(
        result.getUnionTypes().get(0).getDeclarationCode().get(0),
        is("interface GetTestElement extends Actionable {}"));
  }

  @Test
  public void testMethodReturnsListOfBasicElementsImplOnly() {
    PageObjectClass result =
        new DeserializerUtilities()
            .getResultFromFile("interface/returnListOfBasicElementsImpl")
            .getPageObject()
            .getImplementation();
    PageObjectMethod method = result.getMethods().get(0);
    assertThat(
        method.getDeclaration().getCodeLine(), is(equalTo("List<GetTestElement> getTest()")));
    assertThat(method.getClassImports(), hasSize(greaterThanOrEqualTo(1)));
    assertListImport(method.getClassImports().get(0));
    assertThat(result.getUnionTypes(), hasSize(1));
    assertThat(
        result.getUnionTypes().get(0).getDeclarationCode().get(0),
        is(
            "public static class GetTestElementImpl extends BasePageElement implements"
                + " GetTestElement {}"));
  }

  @Test
  public void testMethodReturnsBasicElement() {
    PageObjectInterface result = getInterface("returnBasicElement");
    MethodDeclaration method = getMethod(result);
    assertThat(method.getCodeLine(), is(equalTo("GetTestElement getTest()")));
    assertThat(method.getImports(), is(emptyIterable()));
    assertThat(result.getUnionTypes(), hasSize(1));
    assertThat(
        result.getUnionTypes().get(0).getDeclarationCode().get(0),
        is("interface GetTestElement extends Actionable {}"));
  }

  @Test
  public void testMethodReturnsListOfCustomType() {
    PageObjectInterface result = getInterface("returnListOfCustomElements");
    MethodDeclaration method = getMethod(result);
    assertThat(method.getCodeLine(), is(equalTo("List<Custom> getTest()")));
    assertThat(method.getImports(), hasSize(2));
    assertThat(method.getImports().get(0).getFullName(), is("my.pageobjects.Custom"));
    assertListImport(method.getImports().get(1));
  }

  @Test
  public void testMethodReturnsString() {
    PageObjectInterface result = getInterface("returnString");
    MethodDeclaration method = getMethod(result);
    assertThat(method.getCodeLine(), is(equalTo("String getTest()")));
    assertThat(method.getImports(), is(emptyIterable()));
  }

  @Test
  public void testMethodReturnVoid() {
    PageObjectInterface result = getInterface("returnVoid");
    MethodDeclaration method = getMethod(result);
    assertThat(method.getCodeLine(), is(equalTo("void getTest()")));
    assertThat(method.getImports(), is(emptyIterable()));
  }
}
