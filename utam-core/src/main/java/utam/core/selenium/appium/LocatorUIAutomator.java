package utam.core.selenium.appium;

import io.appium.java_client.MobileBy;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.openqa.selenium.By;
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

  /**
   * Initializes a new instance of the LocatorUIAutomator class
   *
   * @param selectorString selector string to use
   */
  public LocatorUIAutomator(String selectorString) {
    super(selectorString);
  }

  @Override
  public LocatorBy getCopy(String valueWithParameters) {
    return new LocatorUIAutomator(valueWithParameters);
  }

  @Override
  public By getValue() {
    return MobileBy.AndroidUIAutomator(this.stringValue);
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
    RESOURCEID("resourceId"),
    RESOURCEIDMATCHES("resourceIdMatches");

    private final String value;

    Method(String methodValue) {
      this.value = methodValue;
    }

    public String getValue() {
      return value;
    }

    public static String getSupportedMethods(boolean withScrollable) {
      return (withScrollable ? "scrollable, " : "") + SUPPORTED_UIAUTOMATOR_METHODS;
    }
  }
}
