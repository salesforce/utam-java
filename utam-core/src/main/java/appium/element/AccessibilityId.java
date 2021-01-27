package appium.element;

import selenium.element.LocatorNodeImpl;
import selenium.element.LocatorUtilities;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class AccessibilityId extends LocatorNodeImpl {

  public AccessibilityId(String selector, String selectorString, Filter filter) {
    super(Mobile.byAccessibilityId(selector), selectorString, LocatorUtilities.DEFAULT_TRANSFORMER, filter);
  }

  AccessibilityId(String selector) {
    this(selector, selector, LocatorUtilities.EMPTY_FILTER);
  }

  @Override
  protected LocatorNodeImpl getCopy() {
    return new AccessibilityId(getSelector().getValue(), getSelectorString(), getFilterCopy());
  }
}
