/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import utam.core.declarative.representation.PageObjectMethod;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import org.testng.annotations.Test;
import utam.core.element.Clickable;
import utam.core.element.Editable;
import utam.core.element.RootElement;

/**
 * Provides tests for the ElementMethod class
 *
 * @author james.evans
 */
public class RootElementMethodTests {

  /** An ElementMethod object describing a root elements should be able to be created */
  @Test
  public void testPublicRootElementMethodCreation() {
    MethodInfo info = new MethodInfo("getRoot", Clickable.class.getSimpleName());
    info.addCodeLine("this.getRootElement()");
    info.addImportedTypes(Clickable.class.getName());
    PageObjectMethod method =
        new RootElementMethod.Public(new TypeUtilities.FromClass(Clickable.class));
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testProtectedRootElementMethodCreation() {
    MethodInfo info = new MethodInfo("getRootElement", RootElement.class.getSimpleName());
    info.addCodeLine("this.getRootElement()");
    info.addImportedTypes(RootElement.class.getName());
    info.setIsPublic(false);
    PageObjectMethod method = new RootElementMethod.Protected();
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testPrivateRootElementMethodCreation() {
    MethodInfo info = new MethodInfo("getRoot", Editable.class.getSimpleName());
    info.addCodeLine("this.getRootElement()");
    info.addImportedTypes(Editable.class.getName());
    info.setIsPublic(false);
    PageObjectMethod method =
        new RootElementMethod.Private(TypeUtilities.BasicElementInterface.editable);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }
}
