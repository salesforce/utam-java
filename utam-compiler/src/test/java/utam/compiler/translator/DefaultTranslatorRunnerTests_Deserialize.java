/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import utam.compiler.grammar.DeserializerUtilities;
import utam.compiler.grammar.TestUtilities;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.translator.DefaultTranslatorRunner.DUPLICATE_IMPL_ERR;
import static utam.compiler.translator.DefaultTranslatorRunner.DUPLICATE_PAGE_OBJECT_NAME;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;
import static utam.compiler.translator.DefaultTranslatorRunnerTests_Profiles.getRunnerMock;

public class DefaultTranslatorRunnerTests_Deserialize {

  @Test
  public void testSetGeneratedObjects() {
    String json = "{  \"implements\": \"utam-test/pageObjects/test/testInterface\" }";
    PageObjectDeclaration declaration = TestUtilities.getPageObject(json);
    DefaultTranslatorRunner runner = getRunnerMock();
    runner.setPageObject("initial", declaration);
  }

  @Test
  public void testGetGeneratedObjects() {
    String json = "{}";
    PageObjectDeclaration declaration = TestUtilities.getPageObject(json);
    DefaultTranslatorRunner runner = getRunnerMock();
    runner.setPageObject("initial", declaration);
    assertThat(
        runner.getGeneratedPageObjectsNames(),
        is(equalTo(Stream.of("initial").collect(Collectors.toSet()))));
  }

  @Test
  public void testSetPageObject() {
    PageObjectDeclaration declaration = TestUtilities.getPageObject("{}");
    DefaultTranslatorRunner runner = getRunnerMock();
    runner.setPageObject("initial", declaration);
    assertThat(runner.getGeneratedObject("initial"), is(sameInstance(declaration)));
  }

  @Test
  public void testSetPageObjectWithDuplicateNameThrows() {
    PageObjectDeclaration declaration = TestUtilities.getPageObject("{}");
    DefaultTranslatorRunner runner = getRunnerMock();
    runner.setPageObject("initial", declaration);
    UtamError e = expectThrows(UtamError.class, () -> runner.setPageObject("initial", declaration));
    assertThat(
        e.getMessage(), containsString(String.format(DUPLICATE_PAGE_OBJECT_NAME, "initial")));
  }

  @Test
  public void testSetPageObjectWithInterface() {
    String json = "{  \"interface\": true  }";
    PageObjectDeclaration declaration = TestUtilities.getPageObject(json);
    DefaultTranslatorRunner runner = getRunnerMock();
    runner.setPageObject("initial", declaration);
    assertThat(runner.getGeneratedObject("initial"), is(sameInstance(declaration)));
  }

  @Test
  public void testSetPageObjectWithInterfaceWithDuplicateTypeThrows() {
    String json = "{  \"interface\": true  }";
    PageObjectDeclaration declaration = TestUtilities.getPageObject(json);
    DefaultTranslatorRunner runner = getRunnerMock();
    String type = "type";
    runner.setPageObject(type, declaration);
    UtamError e = expectThrows(UtamError.class, () -> runner.setPageObject(type, declaration));
    assertThat(e.getMessage(), containsString(String.format(DUPLICATE_PAGE_OBJECT_NAME, type)));
  }

  @Test
  public void testSetPageObjectWithImplementation() {
    String json = "{ \"implements\": \"utam-test/pageObjects/test/testInterface\"}";
    PageObjectDeclaration declaration = TestUtilities.getPageObject(json);
    DefaultTranslatorRunner runner = getRunnerMock();
    runner.setPageObject("initial", declaration);
    assertThat(runner.getGeneratedObject("initial"), is(sameInstance(declaration)));
  }

  @Test
  public void testSetPageObjectWithImplementationOfDeclaredInterface() {
    String interfaceJson = "{ \"interface\": true }";
    String implementationJson = "{\"implements\": \"utam-test/pageObjects/test/test\"}";

    PageObjectDeclaration interfaceDeclaration = TestUtilities.getPageObject(interfaceJson);
    String interfaceTypeName = interfaceDeclaration.getInterface().getInterfaceType().getFullName();
    PageObjectDeclaration implDeclaration = TestUtilities.getPageObject(implementationJson);
    String implementationTypeName =
        implDeclaration.getImplementation().getClassType().getFullName();
    DefaultTranslatorRunner runner = getRunnerMock();
    runner.setPageObject(interfaceTypeName, interfaceDeclaration);
    runner.setPageObject(implementationTypeName, implDeclaration);
    assertThat(
        runner.getGeneratedObject(interfaceTypeName), is(sameInstance(interfaceDeclaration)));
    assertThat(
        runner.getGeneratedObject(implementationTypeName), is(sameInstance(implDeclaration)));
  }

  @Test
  public void testSetPageObjectWithImplementationWithDuplicateTypeThrows() {
    PageObjectDeclaration declaration = new DeserializerUtilities().getResultFromFile("pageobjects/implements").getPageObject();
    String interfaceTypeName = declaration.getInterface().getInterfaceType().getFullName();
    DefaultTranslatorRunner runner = getRunnerMock();
    runner.setPageObject("initial", declaration);
    UtamError e =
        expectThrows(UtamError.class, () -> runner.setPageObject(interfaceTypeName, declaration));
    assertThat(
        e.getMessage(),
        is(
            equalTo(
                String.format(
                    DUPLICATE_IMPL_ERR,
                    "utam.test.pageobjects.CustomInterface",
                    "utam.test.pageobjects.test.impl.TestImpl"))));
  }
}
