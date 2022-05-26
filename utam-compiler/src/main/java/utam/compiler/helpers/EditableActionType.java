/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.BasicElementActionType.ERROR_CODE_FOR_PARAMETERS;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.UtamCompilerIntermediateError;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Editable;

/**
 * this enum links element actions with translator code <br>
 *
 * @see Editable as of 228 every method has enum here to use in translator
 * @author elizaveta.ivanova
 * @since 226
 */
public enum EditableActionType implements ActionType {
  /**
   * clear element content by entering empty string <br>
   * throws exception if fails
   */
  clear(),
  /**
   * clear element content and enter string <br>
   * throws exception if fails
   */
  clearAndType(PrimitiveType.STRING),
  /**
   * press keyboard key
   */
  press(PrimitiveType.STRING),
  /**
   * Apply WebElement.sendKeys from Selenium - "simulate typing into an element, which may set its *
   * value". <br>
   * Method is wrapped in fluent wait to find the element. Throws exception if nothing found within
   * timeout.
   */
  setText(PrimitiveType.STRING);

  // parameters accepted by the action
  private final PrimitiveType[] actionParameters;

  EditableActionType(PrimitiveType... parameters) {
    if (parameters.length == 0) {
      this.actionParameters = PrimitiveType.EMPTY_ARRAY;
    } else {
      this.actionParameters = parameters;
    }
  }

  Class[] getParameterClasses() { // used in unit tests
    return Stream.of(actionParameters).map(TypeUtilities::getClassFromFullName).toArray(Class[]::new);
  }

  Class getElementClass() {
    return Editable.class;
  }

  @Override
  public TypeProvider getReturnType() {
    return TypeUtilities.VOID;
  }

  @Override
  public List<TypeProvider> getParametersTypes(String parserContext, int parameterCount) {
    int expected = actionParameters.length;
    if (actionParameters.length != parameterCount) {
      throw new UtamCompilerIntermediateError(ERROR_CODE_FOR_PARAMETERS, parserContext, this.name(),
          String.valueOf(expected), String.valueOf(parameterCount));
    }
    return Stream.of(actionParameters).collect(Collectors.toList());
  }

  @Override
  public String getApplyString() {
    return this.name();
  }
}
