package utam.core.element;

import java.util.AbstractMap.SimpleEntry;
import java.util.Map.Entry;

/**
 * Locator for an elements
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface Locator<T> {

  String SELECTOR_STRING_PARAMETER = "%s";
  String SELECTOR_INTEGER_PARAMETER = "%d";

  T getValue();

  default String getStringValue() {
    return getValue().toString();
  }

  default Entry<Integer, Locator<T>> setParameters(int currentIndex, Object... values) {
    return new SimpleEntry<>(currentIndex, this);
  }

  Locator getCopy();
}
