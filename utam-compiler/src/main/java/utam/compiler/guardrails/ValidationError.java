/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.guardrails;

/**
 * types of validation errors produced by the Guardrails
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public enum ValidationError {

  /**
   * Error for components with the same selector but different types
   */
  COMPONENTS_WITH_SAME_SELECTOR_BUT_DIFFERENT_TYPES(
      "components have same selector but different types"),

  /**
   * Error for components and elements having a duplicate selector
   */
  COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR("component and element can't have same selector"),

  /**
   * Error for a selector duplicating the root selector
   */
  DUPLICATE_WITH_ROOT_SELECTOR("selector is duplicate of the root selector");

  private final String errMessage;

  ValidationError(String errMessage) {
    this.errMessage = errMessage;
  }

  String getErrMessage() {
    return errMessage;
  }
}
