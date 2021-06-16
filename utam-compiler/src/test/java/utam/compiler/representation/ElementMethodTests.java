/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.BasicElementInterface.actionable;
import static utam.compiler.helpers.TypeUtilities.BasicElementInterface.clickable;
import static utam.compiler.representation.ElementMethod.DOCUMENT_GETTER;

import java.util.Collections;
import org.testng.annotations.Test;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

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
  private static final String BASE_ELEMENT_TYPE = "utam.core.framework.element.BasePageElement";

  @Test
  public void testSingleElementGetterMethodCreated() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, ACTIONABLE_TYPE.getSimpleName());
    info.addCodeLine("element(this.test).build(Actionable.class, ActionableImpl.class)");
    info.addImportedTypes(ACTIONABLE_TYPE.getFullName(), BASE_ELEMENT_TYPE);

    ElementContext element =
        new ElementContext.Basic(ELEMENT_NAME, ACTIONABLE_TYPE, getCssSelector(".css"));
    PageObjectMethod method = new ElementMethod.Single(element, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testSingleElementWithParametersGetterMethodCreated() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, CLICKABLE_TYPE.getSimpleName());
    info.addCodeLine("element(this.test).build(Clickable.class, ClickableImpl.class, selectorArg)");
    info.addImportedTypes(CLICKABLE_TYPE.getFullName(), BASE_ELEMENT_TYPE);
    info.addParameter(new MethodParameterInfo("selectorArg", "String"));
    ElementContext element =
        new ElementContext.Basic(
            null,
            ELEMENT_NAME,
            CLICKABLE_TYPE,
            getCssSelector(".css[%s]"),
            false,
            Collections.singletonList(
                new ParameterUtils.Regular("selectorArg", PrimitiveType.STRING)), false);
    PageObjectMethod method = new ElementMethod.Single(element, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testListElementMethodCreation() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, "List<Actionable>");
    info.addCodeLine("element(this.test).buildList(Actionable.class, ActionableImpl.class)");
    info.addImportedTypes("java.util.List", ACTIONABLE_TYPE.getFullName(), BASE_ELEMENT_TYPE);
    ElementContext element =
        new ElementContext.Basic(ELEMENT_NAME, ACTIONABLE_TYPE, getCssSelector("css"), true);
    PageObjectMethod method = new ElementMethod.Multiple(element, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }


  @Test
  public void testListElementMethodWithParametersGetterMethodCreated() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME,"List<Clickable>");
    info.addCodeLine("element(this.test).buildList(Clickable.class, ClickableImpl.class, selectorArg)");
    info.addImportedTypes("java.util.List", CLICKABLE_TYPE.getFullName(), BASE_ELEMENT_TYPE);
    info.addParameter(new MethodParameterInfo("selectorArg", "String"));
    ElementContext element =
        new ElementContext.Basic(
            null,
            ELEMENT_NAME,
            CLICKABLE_TYPE,
            getCssSelector(".css[%s]"),
            false,
            Collections.singletonList(
                new ParameterUtils.Regular("selectorArg", PrimitiveType.STRING)), false);
    PageObjectMethod method = new ElementMethod.Multiple(element, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testFilteredElementMethodCreation() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, "Actionable");
    info.addParameter(new MethodParameterInfo("test", "String"));
    info.addCodeLine("element(this.test).build(Actionable.class, ActionableImpl.class, elm -> elm.getText().contains(test))");
    info.addImportedTypes(ACTIONABLE_TYPE.getFullName(), BASE_ELEMENT_TYPE);
    PageObjectMethod method = new ElementMethod.Filtered(
        ELEMENT_NAME,
        ACTIONABLE_TYPE,
        EMPTY_PARAMETERS,
        true,
        "getText",
        EMPTY_PARAMETERS,
        MatcherType.stringContains,
        Collections.singletonList(new ParameterUtils.Primitive("test", PrimitiveType.STRING)),
        true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testDocumentGetter() {
    PageObjectMethod method = DOCUMENT_GETTER;
    assertThat(method.isPublic(), is(false));
    assertThat(method.getClassImports(), is(empty()));
    assertThat(method.getCodeLines().get(0), is(equalTo("this.getDocument()")));
  }
}
