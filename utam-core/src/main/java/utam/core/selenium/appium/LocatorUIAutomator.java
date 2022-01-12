package utam.core.selenium.appium;

import io.appium.java_client.MobileBy;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.openqa.selenium.By;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.LocatorBy;

/**
 * owned by mobile team
 *
 * @since 230
 */
public class LocatorUIAutomator extends LocatorBy {

  private static final String SUPPORTED_UIAUTOMATOR_METHODS =
      Stream.of(LocatorUIAutomator.Method.values())
          .map(method -> method.name().toLowerCase())
          .collect(Collectors.joining(", "));
  static final String ERR_SELECTOR_UIAUTOMATOR_UNSUPPORTED_METHOD =
      "unsupported UiSelector method '%s', supported are: " + SUPPORTED_UIAUTOMATOR_METHODS;
  static final String ERR_SELECTOR_UIAUTOMATOR_UISCROLLABLE_UNSUPPORTED_METHOD =
      "unsupported UiSelector method '%s', supported method are: scrollable, " + SUPPORTED_UIAUTOMATOR_METHODS;

  /**
   * Initializes a new instance of the LocatorUIAutomator class
   *
   * @param selectorString selector string to use
   */
  public LocatorUIAutomator(String selectorString) {
    super(selectorString);
    validateUIAutomatorSelector(this.stringValue);
  }

  private static void validateUIAutomatorSelector(String uiautomator) {
    try {
      if (uiautomator.startsWith("new UiScrollable")) {
        // Examples
        // 1. new UiScrollable(new UiSelector().scrollable(true)).scrollIntoView(new UiSelector().resourceId("com.salesforce.chatter:id/app_launcher_menu_item"))
        // 2. new UiScrollable(new UiSelector().scrollable(true)).scrollIntoView(resourceId("com.salesforce.chatter:id/app_launcher_menu_item"))
        String match1 = uiautomator
            .substring(uiautomator.indexOf(".") + 1, uiautomator.indexOf("(", uiautomator.indexOf(".") + 1));
        if (!match1.equals("scrollable")) {
          if (Stream.of(LocatorUIAutomator.Method.values())
              .noneMatch(method -> method.value.equals(match1))) {
            throw new UtamError(String.format(ERR_SELECTOR_UIAUTOMATOR_UISCROLLABLE_UNSUPPORTED_METHOD, match1));
          }
        }
        // Extract input to first method
        String match2 = uiautomator.substring(uiautomator.indexOf("))") + 2);
        // Extract inner locator
        // continue validating inner method
        uiautomator = match2.substring(match2.indexOf("(") + 1, match2.indexOf("))") + 1);
      }
      if (uiautomator.startsWith("new UiSelector")) {
        // Example - new UiSelector().resourceId("com.salesforce.chatter:id/app_launcher_menu_item")
        // extract method
        // continue validating method
        uiautomator = uiautomator.substring(uiautomator.indexOf(".") + 1);
      }
      // Example - resourceId("com.salesforce.chatter:id/app_launcher_menu_item")
      String match = uiautomator
          .substring(0, uiautomator.indexOf("("));
      if (Stream.of(LocatorUIAutomator.Method.values())
          .noneMatch(method -> method.value.equals(match))) {
        throw new UtamError(String.format(ERR_SELECTOR_UIAUTOMATOR_UNSUPPORTED_METHOD, match));
      }
    } catch (StringIndexOutOfBoundsException e) {
      throw new UtamError(String.format("incorrect UIAutomator selector format '%s'", uiautomator));
    }
  }

  @Override
  public LocatorBy getCopy(String valueWithParameters) {
    return new LocatorUIAutomator(valueWithParameters);
  }

  @Override
  public By getValue() {
    return MobileBy.AndroidUIAutomator(this.stringValue);
  }

  enum Method {
    CHECKABLE("checkable"),
    CHECKED("checked"),
    CLASSNAME("className"),
    DESCRIPTION("description"),
    DESCRIPTIONCONTAINS("descriptionContains"),
    DESCRIPTIONSTARTSWITH("descriptionStartsWith"),
    ENABLED("enabled"),
    SELECTED("selected"),
    RESOURCEID("resourceId"),
    RESOURCEIDMATCHES("resourceIdMatches");

    final String value;

    Method(String methodValue) {
      this.value = methodValue;
    }
  }
}
