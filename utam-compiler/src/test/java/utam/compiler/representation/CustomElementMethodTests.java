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
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.BasicElementInterface.actionable;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.representation.CustomElementMethod.Filtered;
import static utam.compiler.representation.CustomElementMethod.Multiple;
import static utam.compiler.representation.CustomElementMethod.Root;
import static utam.compiler.representation.CustomElementMethod.Single;

import java.util.Collections;
import org.testng.annotations.Test;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * Provides tests for the ComponentMethod class
 *
 * @author james.evans
 */
public class CustomElementMethodTests {

  private static final String ELEMENT_NAME = "test";
  private static final String ELEMENT_METHOD_NAME = "getTest";
  private static final String IMPORT_TYPE_SELECTOR = SELECTOR.getFullName();
  private static final String TYPE_SHORT_NAME = "Type";
  private static final String TYPE_FULL_NAME = "my.package.Type";
  private static final TypeProvider TYPE =
      new TypeUtilities.FromString(TYPE_SHORT_NAME, TYPE_FULL_NAME);
  private static final Root INJECTED_ROOT =
      new Root(new LocatorCodeGeneration(getCssSelector("css")));

  private static ElementContext getBasicScope() {
    final ElementContext scope =
        new ElementContext.Basic(ELEMENT_NAME, actionable, getCssSelector("css"));
    MethodDeclaration declaration = mock(MethodDeclaration.class);
    PageObjectMethod mock = mock(PageObjectMethod.class);
    when(declaration.getName()).thenReturn(ELEMENT_METHOD_NAME);
    when(mock.getDeclaration()).thenReturn(declaration);
    scope.setElementMethod(mock);
    return scope;
  }

  @Test
  public void testExternalComponentMethodCreation() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, TYPE_SHORT_NAME);
    info.addImpliedImportedTypes(IMPORT_TYPE_SELECTOR);
    info.addCodeLines(
        "Type instance = inScope(this.test, LocatorBy.byCss(\"css\"), true)."
            + "build(Type.class)",
        "instance");
    info.addImportedTypes(TYPE_FULL_NAME);
    PageObjectMethod method =
        new Single(
            true, ELEMENT_NAME, INJECTED_ROOT, getBasicScope(), true, TYPE, true, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testComponentMethodWithParametrizedSelector() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, TYPE_SHORT_NAME);
    info.addCodeLines(
        "Type instance = inScope(this.test, LocatorBy.byCss(String.format(\".fakeSelector[title='%s']\", name)), false, true)."
            + "build(Type.class)",
        "instance.load()",
        "instance");
    info.addImportedTypes(TYPE_FULL_NAME);
    info.addImpliedImportedTypes(IMPORT_TYPE_SELECTOR);
    info.addParameter(new PageObjectValidationTestHelper.MethodParameterInfo("name", "String"));
    Single method =
        new Single(
            true,
            ELEMENT_NAME,
            new Root(new LocatorCodeGeneration(
                getCssSelector(".fakeSelector[title='%s']"),
                Collections.singletonList(
                    new ParameterUtils.Regular("name", PrimitiveType.STRING)))),
            getBasicScope(),
            false,
            TYPE,
            false, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  /**
   * A ComponentMethod object that returns a list should be able to be created
   */
  @Test
  public void testComponentMethodReturningList() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, "List<Type>");
    info.addCodeLines(
        "inScope(this.test, LocatorBy.byCss(\"css\"), true, true).buildList(Type.class)");
    info.addImportedTypes("java.util.List");
    info.addImportedTypes(TYPE_FULL_NAME);
    info.addImpliedImportedTypes(IMPORT_TYPE_SELECTOR);
    PageObjectMethod method =
        new Multiple(
            true, ELEMENT_NAME, INJECTED_ROOT, getBasicScope(), TYPE, true, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testComponentMethodWithFilterNullableFindFirst() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, TYPE_SHORT_NAME);
    info.addCodeLines(
        "inScope(this.test, LocatorBy.byCss(\"css\"), false, true).build(Type.class, elm -> elm.applyMethod())");
    info.addImportedTypes(TYPE_FULL_NAME);
    info.addImpliedImportedTypes(IMPORT_TYPE_SELECTOR);
    PageObjectMethod method =
        new Filtered(
            true,
            ELEMENT_NAME,
            INJECTED_ROOT,
            getBasicScope(),
            TYPE,
            false, true,
            "applyMethod",
            EMPTY_PARAMETERS,
            MatcherType.isTrue,
            EMPTY_PARAMETERS,
            true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testComponentMethodWithFilterNotNullableFindAll() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, "List<Type>");
    info.addCodeLines(
        "inScope(this.test, LocatorBy.byCss(\"css\"), true, false).buildList(Type.class, elm -> elm.applyMethod())");
    info.addImportedTypes(TYPE_FULL_NAME, "java.util.List");
    info.addImpliedImportedTypes(IMPORT_TYPE_SELECTOR);
    PageObjectMethod method =
        new Filtered(
            true,
            ELEMENT_NAME,
            INJECTED_ROOT,
            getBasicScope(),
            TYPE,
            true, false,
            "applyMethod",
            EMPTY_PARAMETERS,
            MatcherType.isTrue,
            EMPTY_PARAMETERS,
            false);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  /**
   * An InjectedSelector object returns the proper string representation
   */
  @Test
  public void testInjectedSelector() {
    Root plain = new Root(new LocatorCodeGeneration(getCssSelector("css")));
    assertThat(plain.getCodeString(), is(equalTo("LocatorBy.byCss(\"css\")")));
    Root wParams =
        new Root(
            new LocatorCodeGeneration(getCssSelector(".fakeSelector[title='%s']"),
                Collections
                    .singletonList(new ParameterUtils.Regular("name", PrimitiveType.STRING))));
    assertThat(
        wParams.getCodeString(),
        is(
            equalTo(
                "LocatorBy.byCss(String.format(\".fakeSelector[title='%s']\", name))")));
  }
}
