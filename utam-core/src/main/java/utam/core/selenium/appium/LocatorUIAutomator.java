package utam.core.selenium.appium;

import io.appium.java_client.MobileBy;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.openqa.selenium.By;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.LocatorBy;

/**
 * @since 230
 */
public class LocatorUIAutomator extends LocatorBy {

  public static final String UI_AUTOMATOR_SELECTOR_PREFIX = "new UiSelector().";
  private static final String SUPPORTED_UIAUTOMATOR_METHODS =
      Stream.of(LocatorUIAutomator.Method.values())
          .map(method -> method.name().toLowerCase())
          .collect(Collectors.joining(", "));
  static final String ERR_SELECTOR_UIAUTOMATOR_UNSUPPORTED_METHOD =
      "unsupported UiSelector method '%s', supported are: " + SUPPORTED_UIAUTOMATOR_METHODS;

  public LocatorUIAutomator(String selectorString) {
    super(getSelectorWithPrefix(selectorString));
    validateUIAutomatorSelector(this.stringValue);
  }

  private static String getSelectorWithPrefix(String selectorString) {
    if (selectorString.startsWith(UI_AUTOMATOR_SELECTOR_PREFIX)) {
      return selectorString;
    }
    return UI_AUTOMATOR_SELECTOR_PREFIX + selectorString;
  }

  private static void validateUIAutomatorSelector(String uiautomator) {
    try {
      String match = uiautomator
          .substring(uiautomator.indexOf(".") + 1, uiautomator.indexOf("(", uiautomator.indexOf(".") + 1));
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
    RESOURCEID("resourceId");

    final String value;

    Method(String methodValue) {
      this.value = methodValue;
    }
  }
}
