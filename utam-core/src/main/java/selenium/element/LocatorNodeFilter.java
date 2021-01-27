package selenium.element;

import org.openqa.selenium.WebElement;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * filter applied as part of finding a WebElement
 *
 * @author elizaveta.ivanova
 * @since 224
 */
abstract class LocatorNodeFilter implements LocatorNode.Filter {

  static final String ERR_JAVASCRIPT_INTERPRETATION_NOT_SUPPORTED =
      "javascript interpretation is not supported for the filter type ";
  private static final List<Map.Entry<Integer, WebElement>> EMPTY_MAPPED = new ArrayList<>();
  private static final List<WebElement> EMPTY_FILTERED = new ArrayList<>();

  abstract LocatorNode.Filter getCopy();

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof LocatorNodeFilter)) {
      return false;
    }
    return getFilterString().equals(((LocatorNodeFilter) obj).getFilterString());
  }

  @Override
  public int hashCode() {
    return Objects.hash(getFilterString());
  }

  String getJavascript() {
    throw new NullPointerException(
        ERR_JAVASCRIPT_INTERPRETATION_NOT_SUPPORTED + getClass().getSimpleName());
  }

  Predicate<WebElement> getCondition() {
    return element -> true;
  }

  @Override
  public List<Map.Entry<Integer, WebElement>> map(List<WebElement> found) {
    if (found == null || found.isEmpty()) {
      return EMPTY_MAPPED;
    }
    Predicate<WebElement> condition = isEmpty() ? element -> true : getCondition();
    List<Map.Entry<Integer, WebElement>> res = new ArrayList<>();
    for (int i = 0; i < found.size(); i++) {
      if (condition.test(found.get(i))) {
        res.add(new AbstractMap.SimpleEntry<>(i, found.get(i)));
      }
    }
    return res;
  }

  @Override
  public List<WebElement> filter(List<WebElement> found) {
    if (found == null) {
      return EMPTY_FILTERED;
    }
    if (isEmpty()) {
      return found;
    }
    return found.stream().filter(getCondition()).collect(Collectors.toList());
  }

  static final class Index extends LocatorNodeFilter {

    final int index;

    Index(int index) {
      this.index = index;
    }

    @Override
    public List<Map.Entry<Integer, WebElement>> map(List<WebElement> found) {
      if (found == null || index >= found.size()) {
        return EMPTY_MAPPED;
      }
      return Collections.singletonList(new AbstractMap.SimpleEntry<>(index, found.get(index)));
    }

    @Override
    public List<WebElement> filter(List<WebElement> found) {
      if (found == null || index >= found.size()) {
        return EMPTY_FILTERED;
      }
      return Collections.singletonList(found.get(index));
    }

    @Override
    public final String getFilterString() {
      if (index == 0) {
        return "";
      }
      return String.format("[%s]", index);
    }

    @Override
    LocatorNode.Filter getCopy() {
      return new Index(index);
    }

    @Override
    public boolean isEmpty() {
      return index <= 0;
    }
  }

  static class Empty extends LocatorNodeFilter {

    static final LocatorNodeFilter INSTANCE = new Empty();

    private Empty() {}

    @Override
    public final String getFilterString() {
      return "";
    }

    @Override
    LocatorNode.Filter getCopy() {
      return this;
    }

    @Override
    public boolean isEmpty() {
      return true;
    }
  }
}
