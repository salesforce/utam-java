/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import utam.compiler.helpers.*;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import org.testng.annotations.Test;

import java.util.Collections;

import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.Element.actionable;
import static utam.compiler.helpers.TypeUtilities.Element.clickable;

/**
 * Provides tests for the ElementMethod class
 *
 * @author james.evans
 */
public class ElementMethodTests {

  private static final String ELEMENT_NAME = "test";
  private static final String ELEMENT_METHOD_NAME = "getTest";
  private static final TypeProvider ACTIONABLE_TYPE = actionable;
  private static final TypeProvider CLICKABLE_TYPE = clickable;

  @Test
  public void testSingleElementGetterMethodCreated() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, ACTIONABLE_TYPE.getSimpleName());
    info.addCodeLine("element(this.test).build(Actionable.class)");
    info.addImportedTypes(ACTIONABLE_TYPE.getFullName());

    ElementContext element =
        new ElementContext.Basic(ELEMENT_NAME, ACTIONABLE_TYPE, getCssSelector(".css"));
    PageObjectMethod method = new ElementMethod.Single(element, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testSingleElementWithParametersGetterMethodCreated() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, CLICKABLE_TYPE.getSimpleName());
    info.addCodeLine("element(this.test).build(Clickable.class, selectorArg)");
    info.addImportedTypes(CLICKABLE_TYPE.getFullName());
    info.addParameter(new MethodParameterInfo("selectorArg", "String"));
    ElementContext element =
        new ElementContext.Basic(
            null,
            ELEMENT_NAME,
            CLICKABLE_TYPE,
            getCssSelector(".css[%s]"),
            false,
            Collections.singletonList(
                new ParameterUtils.Regular("selectorArg", PrimitiveType.STRING)));
    PageObjectMethod method = new ElementMethod.Single(element, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testListElementMethodCreation() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, "List<Actionable>");
    info.addCodeLine("element(this.test).buildList(Actionable.class)");
    info.addImportedTypes("java.util.List", ACTIONABLE_TYPE.getFullName());
    ElementContext element =
        new ElementContext.Basic(ELEMENT_NAME, ACTIONABLE_TYPE, getCssSelector("css"), true);
    PageObjectMethod method = new ElementMethod.Multiple(element, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }


  @Test
  public void testListElementMethodWithParametersGetterMethodCreated() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME,"List<Clickable>");
    info.addCodeLine("element(this.test).buildList(Clickable.class, selectorArg)");
    info.addImportedTypes("java.util.List", CLICKABLE_TYPE.getFullName());
    info.addParameter(new MethodParameterInfo("selectorArg", "String"));
    ElementContext element =
        new ElementContext.Basic(
            null,
            ELEMENT_NAME,
            CLICKABLE_TYPE,
            getCssSelector(".css[%s]"),
            false,
            Collections.singletonList(
                new ParameterUtils.Regular("selectorArg", PrimitiveType.STRING)));
    PageObjectMethod method = new ElementMethod.Multiple(element, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testFilteredElementMethodCreation() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, "Actionable");
    info.addParameter(new MethodParameterInfo("test", "String"));
    info.addCodeLine("element(this.test).build(Actionable.class, elm -> elm.getText().contains(test))");
    info.addImportedTypes(ACTIONABLE_TYPE.getFullName());
    PageObjectMethod method = new ElementMethod.Filtered(
        ELEMENT_NAME,
        ACTIONABLE_TYPE,
        EMPTY_PARAMETERS,
        true,
        "getText",
        EMPTY_PARAMETERS,
        MatcherType.stringContains,
        Collections.singletonList(new ParameterUtils.Primitive("test", PrimitiveType.STRING)),
        true, false);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }
}
