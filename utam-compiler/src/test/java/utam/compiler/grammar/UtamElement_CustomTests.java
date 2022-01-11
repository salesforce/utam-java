/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_FILTER_NEEDS_LIST;
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_MISSING_SELECTOR_PROPERTY;
import static utam.compiler.grammar.UtamElement.Type;
import static utam.compiler.grammar.UtamElementFilter_Tests.getInnerTextFilter;
import static utam.compiler.grammar.UtamSelectorTests.getUtamCssSelector;

import org.testng.annotations.Test;
import utam.core.framework.consumer.UtamError;

public class UtamElement_CustomTests {

  private static final String ELEMENT_NAME = "test";
  private static final String METHOD_NAME = "getTest";
  private static final String COMPONENT_TYPE_URI = "utam-test/pageObjects/test/componentName";

  private static UtamElement getPublicComponentElement(UtamSelector selector) {
    UtamElement utamElement = TestUtilities.UtamEntityCreator
        .createUtamElement(ELEMENT_NAME, COMPONENT_TYPE_URI, selector);
    utamElement.isPublic = true;
    return utamElement;
  }

  private static String getCustomSupportedProperties() {
    return Type.CUSTOM.getSupportedPropertiesErr(ELEMENT_NAME);
  }

  /**
   * The validateComponentElement method with a component and a null selector should throw the
   * appropriate exception
   */
  @Test
  public void testValidateComponentElementWithNullSelectorThrows() {
    UtamElement element = getPublicComponentElement(null);
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(
        e.getMessage(), is(String.format(ERR_ELEMENT_MISSING_SELECTOR_PROPERTY, ELEMENT_NAME)));
  }

  /**
   * The validateComponentElement method with a filter and a non-list selector should throw the
   * appropriate exception
   */
  @Test
  public void testValidateComponentWithFilterAndNonListSelectorThrows() {
    UtamElement element = getPublicComponentElement(getUtamCssSelector());
    element.filter = getInnerTextFilter();
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ELEMENT_FILTER_NEEDS_LIST, ELEMENT_NAME)));
  }

  /**
   * The validateComponentElement method with a component and nested elements should throw the
   * appropriate exception
   */
  @Test
  public void testValidateComponentElementWithInnerElementsThrows() {
    UtamElement element = getPublicComponentElement(getUtamCssSelector());
    element.elements = new UtamElement[]{};
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), containsString(getCustomSupportedProperties()));
  }

  /**
   * The validateComponentElement method with a component and nested shadow elements should throw
   * the appropriate exception
   */
  @Test
  public void testValidateComponentElementWithInnerShadowElementsThrows() {
    UtamElement element = getPublicComponentElement(getUtamCssSelector());
    element.shadow = new UtamShadowElement(new UtamElement[]{});
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), containsString(getCustomSupportedProperties()));
  }

  @Test
  public void testDuplicateArgsNamesThrows() {
    UtamError e =
        expectThrows(UtamError.class,
            () -> new DeserializerUtilities().getContext("element/customDuplicateArgs")
                .getMethod(METHOD_NAME));
    assertThat(e.getMessage(), containsString("duplicate parameters"));
  }
}
