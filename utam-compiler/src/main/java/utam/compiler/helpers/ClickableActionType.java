/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.helpers.BasicElementActionType.ERROR_CODE_FOR_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.sql.Array;
import java.time.Duration;
import java.util.ArrayList;
import utam.compiler.UtamCompilationError;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Clickable;

import java.util.List;

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
  click,
  doubleClick,
  rightClick,
  clickAndHold;

  // used in unit tests
  Class[] getParameterClasses() {
    if (this == clickAndHold) {
      return new Class[] { Duration.class };
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
    int expected = 0;
    List<TypeProvider> returned = new ArrayList<>();
    if (this == clickAndHold) {
      expected = 1;
      returned.add(PrimitiveType.NUMBER);
    }
    if (expected != parameterCount) {
      throw new UtamCompilationError(VALIDATION.getErrorMessage(ERROR_CODE_FOR_PARAMETERS, parserContext, this.name(),
          String.valueOf(expected), String.valueOf(parameterCount)));
    }
    return returned;
  }

  @Override
  public String getApplyString() {
    return this.name();
  }
}
