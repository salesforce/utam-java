package utam.core.selenium.element;

/**
 * element that is editable
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface Editable extends Clickable {

  /**
   * Apply WebElement.sendKeys from Selenium - "simulate typing into an element, which may set its
   * value". <br>
   * Method is wrapped in fluent wait to find the element. Throws exception if nothing found within
   * timeout.
   *
   * @param text text to enter
   */
  void setText(String text);

  /**
   * Applies Selenium standard method WebElement.clear (from selenium docs: If this element is a
   * text entry element, this will clear the value). Method is wrapped in fluent wait to find the
   * element. Throws exception if nothing found within timeout.
   */
  void clear();

  /**
   * clear then setText
   *
   * @param text text to enter
   */
  void clearAndType(String text);
}
