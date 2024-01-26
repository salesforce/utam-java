package utam.core.element;

import utam.core.element.Element.ElementRectangle;
import utam.core.framework.base.UtamBase;

/**
 * interaction methods for UI element
 *
 * @author james.evans
 * @since 234
 */
public interface BasicElement extends UtamBase {

  /**
   * returns true if element is found AND enabled <br>
   * it's an immediate check, no waiting is involved. Never throws any exceptions, just returns
   * true/false
   *
   * @return true if element is present and enabled
   */
  boolean isEnabled();

  /**
   * Returns string value of a given attribute, returns the value of the attribute matching the
   * name. <br>
   * Throws exception if element not found within timeout or could not return attribute value or
   * returned null value.
   *
   * @param attribute name of the attribute
   * @return string value of the attribute
   */
  String getAttribute(String attribute);

  /**
   * same as getAttribute("class")
   *
   * @return string value of the value attribute
   */
  String getClassAttribute();

  /**
   * Returns string with the innerText of an element. <br>
   * Throws exception if element not found within timeout or could not return innerText or returned
   * null value.
   *
   * @return inner text from the element
   */
  String getText();

  /**
   * same as getAttribute("title")
   *
   * @return string value of title attribute
   */
  String getTitle();

  /**
   * same as getAttribute("value")
   *
   * @return string value of the value attribute
   */
  String getValue();

  /**
   * Returns the string value of the CSS property with the given name
   *
   * @param propertyName the name of the CSS property whose value to return
   * @return the value of the CSS property
   */
  String getCssPropertyValue(String propertyName);

  /**
   * Check if element has focus (is active) inside document or its parent shadow root
   *
   * @return true if current element is active
   */
  boolean isFocused();

  /**
   * Get element coordinates and size, can be used for drag and drop
   *
   * @return rectangle object
   */
  ElementRectangle getRect();
}
