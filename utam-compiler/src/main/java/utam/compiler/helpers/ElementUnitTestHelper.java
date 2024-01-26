/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.ElementContext.ROOT_ELEMENT_NAME;

import java.util.ArrayList;
import java.util.List;

/**
 * helper class with information for unit test serializer
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class ElementUnitTestHelper {

  private static final String ELEMENT_REGISTRATION_COMMENT_TEMPLATE =
      "//TODO: register element '%s' as a child %sof the %s%s:";
  private static final String ELEMENT_REGISTRATION_TEMPLATE =
      "//%s(simulator.registerElement(\"%s\", \"%s\"));";

  private final String selector;
  private final String parentElementName;
  private final boolean isExpandsParentShadow;
  private final boolean isList;

  /**
   * Initializes a new instance of the ElementUnitTestHelper class
   *
   * @param selector the selector to use
   * @param parentElementName the name of the parent element
   * @param isExpandsParentShadow a value indicating whether to look in the parent element's shadow
   *     root
   * @param isList a value indicating whether the element is a list
   */
  public ElementUnitTestHelper(
      String selector, String parentElementName, boolean isExpandsParentShadow, boolean isList) {
    this.selector = selector;
    this.parentElementName = parentElementName;
    this.isExpandsParentShadow = isExpandsParentShadow;
    this.isList = isList;
  }

  /**
   * Gets the element registration
   *
   * @param elementName the name of the element
   * @return a list of the element registration for logging purposes
   */
  public List<String> getElementRegistration(String elementName) {
    List<String> elementRegistration = new ArrayList<>();
    String parentDescription = "root element";
    if (!this.parentElementName.equals(ROOT_ELEMENT_NAME)) {
      parentDescription = String.format("element named '%s'", parentElementName);
    }
    String shadow = "";
    if (isExpandsParentShadow) {
      shadow = "in the Shadow DOM ";
    }
    String list = "";
    if (isList) {
      list = " (selector returns list)";
    }
    String withChild = ".withChild";
    if (isExpandsParentShadow) {
      withChild = ".withChildInShadowDOM";
    }
    elementRegistration.add(
        String.format(
            ELEMENT_REGISTRATION_COMMENT_TEMPLATE, elementName, shadow, parentDescription, list));
    elementRegistration.add(
        String.format(ELEMENT_REGISTRATION_TEMPLATE, withChild, elementName, selector));
    return elementRegistration;
  }
}
