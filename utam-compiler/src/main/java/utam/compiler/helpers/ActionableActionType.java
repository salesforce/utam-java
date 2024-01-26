/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.ArrayList;
import java.util.List;
import utam.compiler.UtamCompilationError;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Actionable;

/**
 * this enum links element actions with translator code <br>
 *
 * @see Actionable as of 228 every method has enum here to use in translator
 * @author elizaveta.ivanova
 * @since 226
 */
public enum ActionableActionType implements ActionType {
  /**
   * blur the element <br>
   * executes javascript `return arguments[0].blur();` <br>
   * Throws exception if element not found within timeout
   */
  blur,
  /**
   * focus on the value <br>
   * throws exception if fails
   */
  focus,
  /**
   * performs Actions.moveToElement from Selenium, <br>
   * which "Moves the mouse to the middle of the element. The element is scrolled into view". <br>
   * Throws exception if element not found within timeout or element could not be moved to
   */
  moveTo,
  /**
   * scroll to the element <br>
   * executes javascript `return arguments[0].scrollIntoView(true);` <br>
   * Throws exception if element not found within timeout
   */
  scrollToTop,
  /**
   * scrolls current element to the center of the screen <br>
   * executes javascript `arguments[0].scrollIntoView({block:'center'})` <br>
   * Throws exception if element not found within timeout or element could not be scrolled to center
   */
  scrollToCenter;

  // used in unit tests
  Class getElementClass() {
    return Actionable.class;
  }

  // used in unit tests
  Class[] getParameterClasses() {
    return new Class[0];
  }

  @Override
  public TypeProvider getReturnType() {
    return VOID;
  }

  @Override
  public List<TypeProvider> getParametersTypes(String parserContext, int parameterCount) {
    int expected = 0;
    if (parameterCount != expected) {
      String contextStr = String.format("%s action \"%s\"", parserContext, this.name());
      throw new UtamCompilationError(
          VALIDATION.getErrorMessage(
              108, contextStr, String.valueOf(expected), String.valueOf(parameterCount)));
    }
    return new ArrayList<>();
  }

  @Override
  public String getApplyString() {
    return this.name();
  }
}
