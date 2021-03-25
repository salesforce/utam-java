package utam.core.appium.element;

import utam.core.selenium.element.LocatorNodeImpl;
import utam.core.selenium.element.LocatorUtilities;

import static utam.core.selenium.element.LocatorUtilities.EMPTY_FILTER;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class UIAutomator extends LocatorNodeImpl {

  public static final String UI_AUTOMATOR_SELECTOR_PREFIX = "new UiSelector().";

  public UIAutomator(String selector, String selectorString, Filter filter) {
    super(Mobile.byUiAutomator(getSelectorWithPrefix(selector)), getSelectorWithPrefix(selectorString), LocatorUtilities.DEFAULT_TRANSFORMER, filter);
  }

  UIAutomator(String selector) {
    this(selector, selector, EMPTY_FILTER);
  }

  public static String getSelectorWithPrefix(String selectorString) {
    if(selectorString.startsWith(UI_AUTOMATOR_SELECTOR_PREFIX)) {
      return selectorString;
    }
    return UI_AUTOMATOR_SELECTOR_PREFIX + selectorString;
  }

  @Override
  protected LocatorNodeImpl getCopy() {
    return new UIAutomator(getSelector().getValue(), getSelectorString(), getFilterCopy());
  }

  public enum Method {
    CHECKABLE("checkable"),
    CHECKED("checked"),
    CLASSNAME("className"),
    DESCRIPTION("description"),
    DESCRIPTIONCONTAINS("descriptionContains"),
    DESCRIPTIONSTARTSWITH("descriptionStartsWith"),
    ENABLED("enabled"),
    SELECTED("selected"),
    RESOURCEID("resourceId");

    private final String MethodValue;

    Method(String MethodValue) {
      this.MethodValue = MethodValue;
    }

    @Override
    public String toString() {
      return MethodValue;
    }
  }
}
