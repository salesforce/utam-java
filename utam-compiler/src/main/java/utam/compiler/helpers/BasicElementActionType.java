/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.UtamCompilerIntermediateError;
import utam.compiler.types.BasicElementInterface;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;
import utam.core.element.BasicElement;

/**
 * this enum links element actions with translator code <br>
 *
 * @see BasicElement as of 234 every method has enum here to use in translator
 * @author james.evans
 * @since 234
 */
public enum BasicElementActionType implements ActionType {
  /**
   * contains element <br>
   * returns whether an element contains the element specified by the selector <br>
   * Throws exception if element not found within timeout
   */
  containsElement(PrimitiveType.BOOLEAN, SELECTOR, PrimitiveType.BOOLEAN),
  /**
   * get value of the given attribute <br>
   * returns value of the attribute with the given name
   */
  getAttribute(PrimitiveType.STRING, PrimitiveType.STRING),
  /**
   * get value of the "class" attribute <br>
   */
  getClassAttribute(PrimitiveType.STRING),
  /**
   * get inner text of the element <br>
   */
  getText(PrimitiveType.STRING),
  /**
   * get value of the title attribute <br>
   * returns value of the "title" attribute
   */
  getTitle(PrimitiveType.STRING),
  /**
   * get value of the value attribute <br>
   * returns value of the "value" attribute
   */
  getValue(PrimitiveType.STRING),
  /**
   * returns true if element is found AND enabled <br>
   * it's an immediate check, no waiting is involved. Never throws any exceptions, just returns
   * true/false
   *
   * <p>return true if element is present and enabled
   */
  isEnabled(PrimitiveType.BOOLEAN),
  /**
   * focus on the element <br>
   * executes javascript `arguments[0].focus();` <br>
   * Throws exception if element not found within timeout
   */
  isFocused(PrimitiveType.BOOLEAN),
  /**
   * check if element is present immediately <br>
   * same as "present" but does not throw exception if false returned
   */
  isPresent(PrimitiveType.BOOLEAN),
  /**
   * check if element is displayed <br>
   * same as "displayed" but does not throw exception if false returned
   */
  isVisible(PrimitiveType.BOOLEAN),
  /**
   * Only applicable to the element marked as a list <br>
   * Throws exception if element not found within timeout
   *
   * <p>return number of found elements if element is marked as a list
   */
  size(PrimitiveType.NUMBER),
  /**
   * wait for element absence <br>
   * throws exception if fails
   */
  waitForAbsence(null),
  /**
   * wait for element absence <br>
   * throws TimeoutException if fails
   */
  waitForInvisible(null),
  /**
   * wait for element visibility <br>
   * throws TimeoutException if fails
   */
  waitForVisible(null);

  // return type of the action
  private final TypeProvider returnType;
  // parameters accepted by the action
  private final TypeProvider[] actionParameters;

  BasicElementActionType(TypeProvider returnType, TypeProvider... parameters) {
    if (parameters.length == 0) {
      this.actionParameters = PrimitiveType.EMPTY_ARRAY;
    } else {
      this.actionParameters = parameters;
    }
    this.returnType = Objects.requireNonNullElse(returnType, VOID);
  }

  static final Integer ERROR_CODE_FOR_PARAMETERS = 110;

  /**
   * Gets the object representing the action type for the element
   * @param apply       the string value of the action to retrieve
   * @param elementType the type of the element
   * @return            the object representing the action type for the element
   */
  public static ActionType getActionType(String apply, TypeProvider elementType) {
    // Element type is BaseElement, with no other actionable methods available.
    for (BasicElementActionType action : values()) {
      if (action.getApplyString().equals(apply)) {
        return action;
      }
    }
    if (!(elementType instanceof UnionType)) {
      return null;
    }
    List<TypeProvider> actionableTypes = ((UnionType) elementType).getExtendedTypes();
    for (TypeProvider actionableType : actionableTypes) {
      if (actionableType == BasicElementInterface.editable) {
        for (EditableActionType action : EditableActionType.values()) {
          if (action.getApplyString().equals(apply)) {
            return action;
          }
        }
      }
      if (actionableType == BasicElementInterface.touchable) {
        for (TouchableActionType action : TouchableActionType.values()) {
          if (action.getApplyString().equals(apply)) {
            return action;
          }
        }
      }
      if (actionableType == BasicElementInterface.clickable) {
        for (ClickableActionType action : ClickableActionType.values()) {
          if (action.getApplyString().equals(apply)) {
            return action;
          }
        }
      }
      if (actionableType == BasicElementInterface.draggable) {
        for (DraggableActionType action : DraggableActionType.values()) {
          if (action.getApplyString().equals(apply)) {
            return action;
          }
        }
      }
      for (ActionableActionType action : ActionableActionType.values()) {
        if (action.getApplyString().equals(apply)) {
          return action;
        }
      }
    }
    return null;
  }

  // used in unit tests
  @SuppressWarnings("rawtypes")
  Class[] getParameterClasses() {
    return Stream.of(actionParameters).map(TypeUtilities::getClassFromFullName).toArray(Class[]::new);
  }

  // used in unit tests
  @SuppressWarnings("rawtypes")
  Class getElementClass() {
    return BasicElement.class;
  }

  @Override
  public TypeProvider getReturnType() {
    return returnType;
  }

  private static final List<TypeProvider> CONTAINS_LOCATOR = Stream.of(SELECTOR)
      .collect(Collectors.toList());
  private static final List<TypeProvider> CONTAINS_LOCATOR_AND_BOOLEAN = Stream
      .of(SELECTOR, PrimitiveType.BOOLEAN).collect(Collectors.toList());

  @Override
  public List<TypeProvider> getParametersTypes(String parserContext, int parameterCount) {
    if (this == containsElement && (parameterCount == 1 || parameterCount == 2)) {
      return parameterCount == 1? CONTAINS_LOCATOR : CONTAINS_LOCATOR_AND_BOOLEAN;
    } else {
      int expected = actionParameters.length;
      if (actionParameters.length != parameterCount) {
        throw new UtamCompilerIntermediateError(ERROR_CODE_FOR_PARAMETERS, parserContext, this.name(),
            String.valueOf(expected), String.valueOf(parameterCount));
      }
      return Stream.of(actionParameters).collect(Collectors.toList());
    }
  }

  @Override
  public String getApplyString() {
    return this.name();
  }

  /**
   * most action types have corresponding method for UI element, which we test in unit tests
   * except for size or containsElement that has overloaded method and can't be found
   * @return false if there is no method to check for presence in unit tests
   */
  boolean hasMethodToTest() {
    return this != size && this != containsElement;
  }
}
