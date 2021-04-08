/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import utam.core.declarative.representation.TypeProvider;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.declarative.translator.TranslationTypesConfig;
import utam.compiler.translator.TranslationTypesConfigJava;
import org.testng.annotations.Test;
import utam.core.selenium.element.Actionable;

import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;

public class UtilityMethodTests {

  private static final TranslationTypesConfig TYPES_CONFIG = new TranslationTypesConfigJava();
  private static final String UTILITY_FULL_TYPE = "utam-test/utils/test/utilityClass";

  static TypeProvider getUtilityType() {
    return TYPES_CONFIG.getUtilityType(UTILITY_FULL_TYPE);
  }

  /** A basic UtilityMethod object should be able to be created */
  @Test
  public void testUtilityMethodCreation() {
    TypeProvider returnType = new TypeUtilities.FromClass(Actionable.class);
    TypeProvider utilityClassType = getUtilityType();
    MethodInfo info = new MethodInfo("testMethod", returnType.getSimpleName());
    info.addCodeLine("this.utilUtilityClass.appliedMethod()");
    info.addImportedTypes(returnType.getFullName());
    info.addImpliedImportedTypes(utilityClassType.getFullName());

    UtilityMethod method =
        new UtilityMethod(
                new TypeUtilities.FromClass(Actionable.class),
            false,
            new UtilityMethod.Utility(
                "appliedMethod", EMPTY_PARAMETERS, new UtilityReferenceField(utilityClassType)));
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  /** A basic UtilityMethod object should be able to be created with a list return value */
  @Test
  public void testUtilityMethodCreationWithList() {
    TypeProvider returnType =
        new TypeUtilities.ListOf(new TypeUtilities.FromClass(Actionable.class));
    TypeProvider utilityClassType = getUtilityType();
    MethodInfo info = new MethodInfo("testMethod", returnType.getSimpleName());
    info.addCodeLine("this.utilUtilityClass.appliedMethod()");
    info.addImpliedImportedTypes(
        new TypeUtilities.FromClass(Actionable.class).getFullName(),
        "java.util.List",
        utilityClassType.getFullName());

    UtilityMethod method =
        new UtilityMethod(
                new TypeUtilities.FromClass(Actionable.class),
            true,
            new UtilityMethod.Utility(
                "appliedMethod", EMPTY_PARAMETERS, new UtilityReferenceField(utilityClassType)));
    PageObjectValidationTestHelper.validateMethod(method, info);
  }
}
