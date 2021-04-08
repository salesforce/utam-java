/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.UtamMethodUtil.ERR_METHOD_WRONG_UTILS_TYPE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.UtilityMethod.Utility;

public class UtamMethodUtil_Tests {
  
  /**
   * The getMethodReference method should return the proper value
   */
  @Test
  public void testGetMethodReference() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethodUtil util = new UtamMethodUtil(
        "utam-test/utils/test/testUtilClass", 
        "testUtilityMethod",
        new UtamArgument[] {});
    
    Utility utilObject = util.getMethodReference("testMethod", context);
    assertThat(utilObject, is(not(nullValue())));
  }
  
  /**
   * The getMethodReference method should throw the proper exception when
   * the utility method and the page object do not share the same namespace
   */
  @Test
  public void testMethodReferenceWithMismatchedNamespacesThrows() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethodUtil util = new UtamMethodUtil(
        "utam-invalid/utils/test/testUtilClass", 
        "testUtilityMethod",
        new UtamArgument[] {});
    
    UtamError e = expectThrows(
        UtamError.class, 
        () -> util.getMethodReference("testMethod", context));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_METHOD_WRONG_UTILS_TYPE, "testMethod",
                "utam-invalid/utils/test/testUtilClass", "test", "invalid")));
  }
}
