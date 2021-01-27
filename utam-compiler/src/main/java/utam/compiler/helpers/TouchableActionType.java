package utam.compiler.helpers;

import appium.element.GestureDirection;
import declarative.representation.TypeProvider;
import selenium.element.Touchable;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
  flick(PrimitiveType.VOID, PrimitiveType.NUMBER, PrimitiveType.NUMBER),
  /**
   * Flicks a list of web elements in the desired direction.<br>
   * Compares the original element's text and position to the updated element after the flick.<br>
   * If the text and position are the same as before the flick, then the end of the list is reached.
   * <br>
   * throws exception if fails
   */
  flickItems(PrimitiveType.BOOLEAN, PrimitiveType.STRING);

  // parameters accepted by the action
  private final PrimitiveType[] actionParameters;
  // return type of the action
  private final PrimitiveType returnType;

  TouchableActionType(PrimitiveType returnType, PrimitiveType... parameters) {
    if (parameters.length == 0) {
      this.actionParameters = PrimitiveType.EMPTY_ARRAY;
    } else {
      this.actionParameters = parameters;
    }
    this.returnType = returnType;
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
    return false;
  }

  @Override
  public String getApplyString() {
    return this.name();
  }

  @Override
  public String getInvokeMethodName() {
    return this.name();
  }

  Class[] getParameterClasses() { // used in unit tests
    List<Class> paramTypeList = new ArrayList<>();
    Stream.of(actionParameters)
        .map(PrimitiveType::getClassType)
        .forEach(
            (k) -> {
              // Map to argument type in Touchable interface
              // flick method takes int primitives as arguments
              if (k.getTypeName().contains("Integer")) {
                paramTypeList.add(int.class);
              }
              // flickItems method takes GestureDirection as argument
              else {
                paramTypeList.add(GestureDirection.class);
              }
            });

    return paramTypeList.toArray(new Class[0]);
  }

  Class getElementClass() {
    return Touchable.class;
  }
}
