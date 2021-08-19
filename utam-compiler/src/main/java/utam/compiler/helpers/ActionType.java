/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import java.util.Collections;
import java.util.List;
import utam.compiler.grammar.UtamArgument;
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
   * @return list
   */
  List<TypeProvider> getParametersTypes();

  /**
   * some actions might have more than one possible set of parameters
   *
   * @return all possible combinations of parameter types
   */
  default List<List<TypeProvider>> getParametersTypesOptions() {
    return Collections.singletonList(getParametersTypes());
  }

  /**
   * value of "apply" property in JSON, ex. "click"
   *
   * @return string with apply property
   */
  String getApplyString();

  /**
   * because "getClass()" is reserved method in Java, for this method invoked method will be
   * getClassAttribute(); in other cases method name is same as "apply" property value
   *
   * @return string with method name to invoke
   */
  default String getInvokeMethodName() {
    return this.getApplyString();
  }

  /**
   * some actions can add default value to parameters, hence transforming originally passed args
   *
   * @param args declared args
   * @return array or args
   */
  default UtamArgument[] getTransformedArgs(UtamArgument[] args) {
    return args;
  }
}
