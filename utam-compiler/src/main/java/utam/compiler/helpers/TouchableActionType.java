/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.UtamCompilationError;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Touchable;

/**
 * this enum links mobile touch actions with translator code <br>
 * every method from Touchable interface has enum here to use in translator
 *
 * @author r.rajasekaran
 * @since 232
 */
public enum TouchableActionType implements ActionType {
  /**
   * Flick an element in the horizontal and vertical direction, based on the x-offset and y-offset
   * from the element.<br>
   * throws exception if fails
   */
  flick(TypeUtilities.VOID, PrimitiveType.NUMBER, PrimitiveType.NUMBER);

  // parameters accepted by the action
  private final PrimitiveType[] actionParameters;
  // return type of the action
  private final TypeProvider returnType;

  TouchableActionType(TypeProvider returnType, PrimitiveType... parameters) {
    this.actionParameters = parameters;
    this.returnType = returnType;
  }

  @Override
  public TypeProvider getReturnType() {
    return returnType;
  }

  @Override
  public String getApplyString() {
    return this.name();
  }

  Class[] getParameterClasses() { // used in unit tests
    List<Class> paramTypeList = new ArrayList<>();
    Stream.of(actionParameters)
        .map(TypeUtilities::getClassFromFullName)
        .forEach(
            clazz -> {
              // Map to argument type in Touchable interface
              // flick method takes int primitives as arguments
              if (clazz.getTypeName().contains("Integer")) {
                paramTypeList.add(int.class);
              }
            });

    return paramTypeList.toArray(new Class[0]);
  }

  @Override
  public List<TypeProvider> getParametersTypes(String parserContext, int parameterCount) {
    int expected = actionParameters.length;
    if (actionParameters.length != parameterCount) {
      String contextStr = String.format("%s action \"%s\"", parserContext, this.name());
      throw new UtamCompilationError(
          VALIDATION.getErrorMessage(
              108, contextStr, String.valueOf(expected), String.valueOf(parameterCount)));
    }
    return Stream.of(actionParameters).collect(Collectors.toList());
  }

  Class getElementClass() {
    return Touchable.class;
  }
}
