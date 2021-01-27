package utam.compiler.helpers;

import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.Actionable;
import utam.core.selenium.expectations.ExpectationsUtil;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * this enum links element actions with translator code <br>
 *
 * @see ExpectationsUtil every static method should be registered as enum here
 *     to use in JSON as applicable action <br>
 * @see Actionable as of 228 every method has enum here to use in translator
 * @author elizaveta.ivanova
 * @since 226
 */
public enum ActionableActionType implements ActionType {
  /**
   * wait for element absence <br>
   * throws exception if fails
   */
  waitForAbsence(PrimitiveType.VOID),
  /**
   * focus on the value <br>
   * throws exception if fails
   */
  focus(PrimitiveType.VOID),
  /**
   * focus on the element <br>
   * executes javascript `arguments[0].focus();` <br>
   * Throws exception if element not found within timeout
   */
  isFocused(PrimitiveType.BOOLEAN),
  /**
   * get value of the given attribute <br>
   * returns value of the attribute with the given name
   */
  getAttribute(PrimitiveType.STRING, PrimitiveType.STRING),
  /**
   * get value of the "class" attribute <br>
   */
  getClass(PrimitiveType.STRING),
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
   * wait for element absence <br>
   * throws TimeoutException if fails
   */
  waitForInvisible(PrimitiveType.VOID),
  /**
   * check if element is displayed <br>
   * same as "displayed" but does not throw exception if false returned
   */
  isVisible(PrimitiveType.BOOLEAN),
  /**
   * returns true if element is found AND enabled <br>
   * it's an immediate check, no waiting is involved. Never throws any exceptions, just returns
   * true/false
   *
   * <p>return true if element is present and enabled
   */
  isEnabled(PrimitiveType.BOOLEAN),
  /**
   * check if element is present immediately <br>
   * same as "present" but does not throw exception if false returned
   */
  isPresent(PrimitiveType.BOOLEAN),
  /**
   * performs Actions.moveToElement from Selenium, <br>
   * which "Moves the mouse to the middle of the element. The element is scrolled into view". <br>
   * Throws exception if element not found within timeout or element could not be moved to
   */
  moveTo(PrimitiveType.VOID),
  /**
   * scroll to the element <br>
   * executes javascript `return arguments[0].scrollIntoView(true);` <br>
   * Throws exception if element not found within timeout
   */
  scrollToTop(PrimitiveType.VOID),
  /**
   * scrolls current element to the center of the screen <br>
   * executes javascript `arguments[0].scrollIntoView({block:'center'})` <br>
   * Throws exception if element not found within timeout or element could not be scrolled to center
   */
  scrollToCenter(PrimitiveType.VOID),
  /**
   * Only applicable to the element marked as a list <br>
   * Throws exception if element not found within timeout
   *
   * <p>return number of found elements if element is marked as a list
   */
  size(PrimitiveType.NUMBER),
  /**
   * wait for element visibility <br>
   * throws TimeoutException if fails
   */
  waitForVisible(PrimitiveType.VOID),
  /**
   * blur the element <br>
   * executes javascript `return arguments[0].blur();` <br>
   * Throws exception if element not found within timeout
   */
  blur(PrimitiveType.VOID);

  static final String ERR_NOT_HTML_ELEMENT = "element '%s' is not HTML element, its type is '%s'";
  static final String ERR_UNKNOWN_ACTION = "unknown action '%s' for %s element '%s'";
  // return type of the action
  private final PrimitiveType returnType;
  // parameters accepted by the action
  private final PrimitiveType[] actionParameters;

  ActionableActionType(PrimitiveType returnType, PrimitiveType... parameters) {
    if (parameters.length == 0) {
      this.actionParameters = PrimitiveType.EMPTY_ARRAY;
    } else {
      this.actionParameters = parameters;
    }
    this.returnType = returnType;
  }

  public static ActionType getActionType(String apply, TypeProvider elementType, String elementName) {
    TypeUtilities.Element actionableType =
        Stream.of(TypeUtilities.Element.values())
            .filter(element -> element.getType().equals(elementType))
            .findFirst()
            .orElseThrow(
                () ->
                    new UtamError(
                        String.format(
                            ERR_NOT_HTML_ELEMENT,
                            elementName,
                            elementType.getFullName())));
    if (actionableType == TypeUtilities.Element.editable) {
      for (EditableActionType action : EditableActionType.values()) {
        if (action.getApplyString().equals(apply)) {
          return action;
        }
      }
    }
    if (actionableType == TypeUtilities.Element.touchable) {
      for (TouchableActionType action : TouchableActionType.values()) {
        if (action.getApplyString().equals(apply)) {
          return action;
        }
      }
    }
    if (actionableType == TypeUtilities.Element.editable
        || actionableType == TypeUtilities.Element.clickable) {
      for (ClickableActionType action : ClickableActionType.values()) {
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
    throw new UtamError(String.format(ERR_UNKNOWN_ACTION, apply, actionableType.name(), elementName));
  }

  // used in unit tests
  Class[] getParameterClasses() {
    return Stream.of(actionParameters).map(PrimitiveType::getClassType).toArray(Class[]::new);
  }

  // used in unit tests
  Class getElementClass() {
    return Actionable.class;
  }

  @Override
  public PrimitiveType getReturnType() {
    return returnType;
  }

  @Override
  public List<TypeProvider> getParametersTypes() {
    return Stream.of(actionParameters).collect(Collectors.toList());
  }

  @Override
  public boolean isListAction() {
    return this == waitForAbsence
        || this == waitForVisible
        || this == waitForInvisible
        || this == isVisible
        || this == isPresent
        || this == size;
  }

  @Override
  public String getApplyString() {
    return this.name();
  }

  @Override
  public String getInvokeMethodName() {
    if( this == getClass) {
      return "getClassAttribute";
    }
    return name();
  }

  /**
   * most action types have corresponding method for UI element, which we test in unit tests
   * except for size
   * @return false if there is no method to check for presence in unit tests
   */
  boolean hasMethodToTest() {
    return this != size;
  }
}
