/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import java.util.List;
import utam.core.declarative.representation.TypeProvider;

/**
 * action applied to an element
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface ActionType {

  /**
   * type returned by the action
   *
   * @return return type
   */
  TypeProvider getReturnType();

  /**
   * list of expected parameters types
   *
   * @param parserContext parser context to validate parameters
   * @param parameterCount number of expected parameters
   * @return list of expected types
   */
  List<TypeProvider> getParametersTypes(String parserContext, int parameterCount);

  /**
   * value of "apply" property in JSON, ex. "click"
   *
   * @return string with apply property
   */
  String getApplyString();
}
