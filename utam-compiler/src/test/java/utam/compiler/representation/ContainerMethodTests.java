/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_LIST_RETURN_TYPE;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_RETURN_TYPE;
import static utam.compiler.helpers.TypeUtilities.BasicElementInterface.actionable;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.representation.ContainerMethod.PAGE_OBJECT_PARAMETER;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.grammar.UtamSelector;
import utam.compiler.helpers.ElementContext;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.base.PageObject;

/**
 * ContainerMethod representation class tests
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public class ContainerMethodTests {

  public static final MethodParameterInfo FIRST_CONTAINER_PARAMETER =
      new PageObjectValidationTestHelper.MethodParameterInfo(
          PAGE_OBJECT_PARAMETER.getValue(), PAGE_OBJECT_PARAMETER.getType().getSimpleName());
  private static final String RETURN_TYPE = CONTAINER_RETURN_TYPE.getSimpleName();
  private static final String LIST_RETURN_TYPE = CONTAINER_LIST_RETURN_TYPE.getSimpleName();
  private static final String METHOD_NAME = "getContainer";
  private static final String ELEMENT_NAME = "container";

  private static ElementContext getScope() {
    final ElementContext scope =
        new ElementContext.Basic("scope", actionable, getCssSelector("css"));
    MethodDeclaration declaration = mock(MethodDeclaration.class);
    PageObjectMethod mock = mock(PageObjectMethod.class);
    when(declaration.getName()).thenReturn("getScope");
    when(mock.getDeclaration()).thenReturn(declaration);
    scope.setElementMethod(mock);
    return scope;
  }

  @Test
  public void testContainerMethodWithSelector() {
    MethodInfo info = new MethodInfo(METHOD_NAME, RETURN_TYPE);
    info.addCodeLine(
        "this.inContainer(this.scope, false).load(pageObjectType, LocatorBy.byCss(\".fakeSelector\"))");
    info.addImportedTypes(PageObject.class.getName());
    info.addImpliedImportedTypes(PageObject.class.getName(), SELECTOR.getFullName());
    info.addParameter(FIRST_CONTAINER_PARAMETER);
    ContainerMethod method = new ContainerMethod.WithSelector(
        getScope(), false, ELEMENT_NAME,
        new UtamSelector(".fakeSelector").getCodeGenerationHelper(getTestTranslationContext()));
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testContainerMethodWithSelectorReturnsList() {
    MethodInfo info = new MethodInfo(METHOD_NAME, LIST_RETURN_TYPE);
    info.addCodeLine(
        "this.inContainer(this.scope, false).loadList(pageObjectType, LocatorBy.byCss(\".fakeSelector\"))");
    info.addImportedTypes(PageObject.class.getName(), List.class.getName());
    info.addImpliedImportedTypes(PageObject.class.getName(), SELECTOR.getFullName());
    info.addParameter(FIRST_CONTAINER_PARAMETER);
    ContainerMethod method = new ContainerMethod.WithSelectorReturnsList(
        getScope(), false, ELEMENT_NAME,
        new UtamSelector(".fakeSelector").getCodeGenerationHelper(getTestTranslationContext()));
    PageObjectValidationTestHelper.validateMethod(method, info);
  }
}
