/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.Objects;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Actionable;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
  blur(null),
  /**
   * focus on the value <br>
   * throws exception if fails
   */
  focus(null),
  /**
   * performs Actions.moveToElement from Selenium, <br>
   * which "Moves the mouse to the middle of the element. The element is scrolled into view". <br>
   * Throws exception if element not found within timeout or element could not be moved to
   */
  moveTo(null),
  /**
   * scroll to the element <br>
   * executes javascript `return arguments[0].scrollIntoView(true);` <br>
   * Throws exception if element not found within timeout
   */
  scrollToTop(null),
  /**
   * scrolls current element to the center of the screen <br>
   * executes javascript `arguments[0].scrollIntoView({block:'center'})` <br>
   * Throws exception if element not found within timeout or element could not be scrolled to center
   */
  scrollToCenter(null);

  static final String ERR_NOT_HTML_ELEMENT = "element '%s' is not HTML element, its type is '%s'";
  static final String ERR_UNKNOWN_ACTION = "unknown action '%s' for element '%s' with %s";
  // return type of the action
  private final TypeProvider returnType;
  // parameters accepted by the action
  private final TypeProvider[] actionParameters;

  ActionableActionType(TypeProvider returnType, TypeProvider... parameters) {
    if (parameters.length == 0) {
      this.actionParameters = PrimitiveType.EMPTY_ARRAY;
    } else {
      this.actionParameters = parameters;
    }
    this.returnType = Objects.requireNonNullElse(returnType, VOID);
  }

  // used in unit tests
  Class getElementClass() {
    return Actionable.class;
  }

  // used in unit tests
  Class[] getParameterClasses() {
    return Stream.of(actionParameters).map(TypeProvider::getClassType).toArray(Class[]::new);
  }

  @Override
  public TypeProvider getReturnType() {
    return returnType;
  }

  @Override
  public List<TypeProvider> getParametersTypes() {
    return Stream.of(actionParameters).collect(Collectors.toList());
  }

  @Override
  public String getApplyString() {
    return this.name();
  }

  @Override
  public String getInvokeMethodName() {
    return name();
  }
}
