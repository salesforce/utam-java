package utam.compiler.helpers;

import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.Collections;
import utam.core.declarative.representation.TypeProvider;
import utam.core.selenium.element.Clickable;
import utam.core.selenium.expectations.ExpectationsUtil;

import java.util.List;

/**
 * this enum links element actions with translator code <br>
 *
 * @see ExpectationsUtil every static method should be registered as enum here
 *     to use in JSON as applicable action <br>
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
   * executes javascript "arguments[0].click();" to trick Selenium into clicking on the element that
   * is not considered clickable by the Web Driver. It's a workaround for inconsistent behavior of
   * some browsers. Otherwise same as "click"
   */
  javascriptClick();

  // used in unit tests
  Class[] getParameterClasses() {
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
  public List<TypeProvider> getParametersTypes() {
    return Collections.EMPTY_LIST;
  }

  @Override
  public String getApplyString() {
    return this.name();
  }
}
