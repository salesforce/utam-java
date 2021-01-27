package selenium.element;

import appium.element.GestureDirection;

/**
 * interaction methods for touchable element
 * @author r.rajasekaran
 * @since 232
 */

public interface Touchable extends Actionable {

  /**
   * Flick on the touch screen using finger motion events. Start point is middle of the element.
   * End point is determined by x and y offset coordinates.
   * For vertical flick, use xOffset as 0 and yOffset as pixels to flick by.
   * For horizontal flick, use yOffset as 0 and xOffset as pixels to flick by.
   *
   * @param xOffset Offset for x
   * @param yOffset Offset for y
  */
  void flick(int xOffset, int yOffset);
    
  /**
   * Flicks a list of web elements in the desired direction repeatedly until the end of the list is reached.
   * <p>
   * This method will compare the original element's text and position to the updated element after the flick.  If
   * the text and position are the same as before the flick, then the end of the list is reached.
   * 
   * </p>
   *
   * @param direction The direction of the flick gesture. Allowed values are DOWN, UP, LEFT, RIGHT.
   * @return True if the list was scrolled as part of the flick, false if the list could not be scrolled.
   */
  boolean flickItems(GestureDirection direction);

}
