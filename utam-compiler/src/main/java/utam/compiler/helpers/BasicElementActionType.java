package utam.compiler.helpers;

import utam.compiler.grammar.UtamArgument;
import utam.compiler.helpers.TypeUtilities.BasicElementInterface;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.BasicElement;
import utam.core.framework.consumer.UtamError;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.helpers.TypeUtilities.ROOT_ELEMENT_TYPE;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.helpers.TypeUtilities.VOID;

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

  public static ActionType getActionType(String apply, TypeProvider elementType, String elementName) {
    if (!TypeUtilities.BasicElementInterface.isBasicType(elementType)
        && !elementType.isSameType(ROOT_ELEMENT_TYPE)) {
      throw new UtamError(
          String.format(
              ActionableActionType.ERR_NOT_HTML_ELEMENT,
              elementName,
              elementType.getSimpleName()));
    }
    // Element type is BaseElement, with no other actionable methods available.
    for (BasicElementActionType action : values()) {
      if (action.getApplyString().equals(apply)) {
        return action;
      }
    }
    TypeUtilities.BasicElementInterface[] actionableTypes =
        TypeUtilities.BasicElementInterface.getBasicElementTypes(elementType);
    if (actionableTypes != null) {
      for (TypeUtilities.BasicElementInterface actionableType : actionableTypes) {
        if (actionableType == TypeUtilities.BasicElementInterface.editable) {
          for (EditableActionType action : EditableActionType.values()) {
            if (action.getApplyString().equals(apply)) {
              return action;
            }
          }
        }
        if (actionableType == TypeUtilities.BasicElementInterface.touchable) {
          for (TouchableActionType action : TouchableActionType.values()) {
            if (action.getApplyString().equals(apply)) {
              return action;
            }
          }
        }
        if (actionableType == TypeUtilities.BasicElementInterface.clickable) {
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
    }
    String actionableTypeNames =
        actionableTypes == null || actionableTypes.length == 0 ?
            "no declared interfaces" :
            "declared interfaces " + Arrays.stream(actionableTypes)
                .map(Enum::name)
                .collect(Collectors.joining(","));
    throw new UtamError(
        String.format(
            ActionableActionType.ERR_UNKNOWN_ACTION, apply, elementName, actionableTypeNames));
  }

  // used in unit tests
  @SuppressWarnings("rawtypes")
  Class[] getParameterClasses() {
    return Stream.of(actionParameters).map(TypeProvider::getClassType).toArray(Class[]::new);
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

  @Override
  public List<TypeProvider> getParametersTypes() {
    return Stream.of(actionParameters).collect(Collectors.toList());
  }

  @Override
  public String getApplyString() {
    return this.name();
  }

  @Override
  public UtamArgument[] getTransformedArgs(UtamArgument[] args) {
    if (this == BasicElementActionType.containsElement && args.length == 1) {
      // If the action is "containsElement", it may have one argument (a selector),
      // or two arguments (a selector and a boolean indicating whether to search in
      // the shadow DOM) declared in the JSON. If the second argument is omitted,
      // it can be assumed to be false, so substitute that value here.
      return new UtamArgument[]{args[0], new UtamArgument.UtamArgumentLiteral(Boolean.FALSE)};
    }
    return args;
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
