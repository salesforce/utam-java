/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import utam.core.framework.consumer.UtamError;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
class UtamShadowElement {

  static final String ERR_SHADOW_EMPTY_ELEMENTS = "shadow object should have elements";

  final UtamElement[] elements;

  @JsonCreator
  UtamShadowElement(@JsonProperty(value = "elements", required = true) UtamElement[] elements) {
    this.elements = elements;
    if (elements == null) {
      throw new UtamError(ERR_SHADOW_EMPTY_ELEMENTS);
    }
  }
}
