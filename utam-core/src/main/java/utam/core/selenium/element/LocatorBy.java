package utam.core.selenium.element;

import java.util.AbstractMap.SimpleEntry;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.regex.Pattern;
import org.openqa.selenium.By;
import utam.core.element.Locator;
import utam.core.selenium.appium.LocatorAccessibilityId;
import utam.core.selenium.appium.LocatorClassChain;
import utam.core.selenium.appium.LocatorUIAutomator;

/**
 * combines selector string with its type into one object
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public abstract class LocatorBy implements Locator<By> {

  protected final String stringValue;
  private final int parametersCount;

  protected LocatorBy(String stringValue) {
    this.stringValue = stringValue;
    this.parametersCount = getParametersCount(stringValue);
  }

  public static LocatorBy byCss(String value) {
    return new LocatorByCss(value);
  }

  public static LocatorBy byAccessibilityId(String value) {
    return new LocatorAccessibilityId(value);
  }

  public static LocatorBy byClassChain(String value) {
    return new LocatorClassChain(value);
  }

  public static LocatorBy byUiAutomator(String value) {
    return new LocatorUIAutomator(value);
  }

  static int getParametersCount(String string) {
    if (string.equals(SELECTOR_STRING_PARAMETER)) {
      return 1;
    }
    return string.split(Pattern.quote(SELECTOR_STRING_PARAMETER)).length
        - 1
        + string.split(Pattern.quote(SELECTOR_INTEGER_PARAMETER)).length
        - 1;
  }

  @Override
  public Entry<Integer, Locator<By>> setParameters(int currentIndex, Object... parameters) {
    if (parametersCount <= 0 || parameters == null || parameters.length == 0) {
      return new SimpleEntry<>(currentIndex, this);
    }
    Object[] values = new Object[parametersCount];
    for (int i = currentIndex; i < parametersCount + currentIndex; i++) {
      if (i < parameters.length) {
        values[i - currentIndex] = parameters[i];
      } else {
        throw new IndexOutOfBoundsException(
            String.format("index %d is out of bounds: total number of parameters is %d", i,
                parameters.length));
      }
    }
    String mutableValue = String.format(stringValue, values);
    return new SimpleEntry<>(currentIndex + parametersCount, getCopy(mutableValue));
  }

  // public because used in tests from other package
  abstract public LocatorBy getCopy(String valueWithParameters);

  public Locator getCopy() {
    return getCopy(stringValue);
  }

  @Override
  public String getStringValue() {
    return stringValue;
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof LocatorBy)) {
      return false;
    }
    return ((LocatorBy) obj).getValue().equals(getValue());
  }

  @Override
  public int hashCode() {
    return Objects.hash(getClass(), stringValue);
  }
}
