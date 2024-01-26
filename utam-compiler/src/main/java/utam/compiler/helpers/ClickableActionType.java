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

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.UtamCompilationError;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Clickable;

/**
 * this enum links element actions with translator code <br>
 *
 * @see Clickable as of 228 every method has enum here to use in translator
 * @author elizaveta.ivanova
 * @since 226
 */
public enum ClickableActionType implements ActionType {
  /**
   * click on the element using WebElement.click <br>
   * throws exception if fails
   */
  click(),

  /**
   * double-click on the element using Actions class <br>
   * throws exception if fails
   */
  doubleClick(),

  /**
   * right-click on the element using Actions class <br>
   * throws exception if fails
   */
  rightClick(),

  /**
   * click and hold on the element for the specified number of seconds using Actions class <br>
   * throws exception if fails
   */
  clickAndHold(PrimitiveType.NUMBER);

  private final PrimitiveType[] actionParameters;

  ClickableActionType(PrimitiveType... parameters) {
    if (parameters.length == 0) {
      this.actionParameters = PrimitiveType.EMPTY_ARRAY;
    } else {
      this.actionParameters = parameters;
    }
  }

  // used in unit tests
  Class[] getParameterClasses() {
    if (this == clickAndHold) {
      // Using TypeUtilities.getClassFromFullName will return "java.lang.Integer"
      // as the class name, which is not what we want. We want the primitive,
      // not the boxed Integer class, so we will hand-code that here.
      return new Class[] {int.class};
    }
    return new Class[0];
  }

  // used in unit tests
  Class getElementClass() {
    return Clickable.class;
  }

  @Override
  public TypeProvider getReturnType() {
    return VOID;
  }

  @Override
  public List<TypeProvider> getParametersTypes(String parserContext, int parameterCount) {
    int expected = actionParameters.length;
    String contextStr = String.format("%s action \"%s\"", parserContext, this.name());
    if (actionParameters.length != parameterCount) {
      throw new UtamCompilationError(
          VALIDATION.getErrorMessage(
              108, contextStr, String.valueOf(expected), String.valueOf(parameterCount)));
    }
    return Stream.of(actionParameters).collect(Collectors.toList());
  }

  @Override
  public String getApplyString() {
    return this.name();
  }
}
