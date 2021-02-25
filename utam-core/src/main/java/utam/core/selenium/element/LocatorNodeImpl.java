package utam.core.selenium.element;

import org.openqa.selenium.By;
import org.openqa.selenium.NotFoundException;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;
import utam.core.selenium.context.WebDriverUtilities;

import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.core.selenium.element.Web.SelectorImpl;

import static utam.core.selenium.element.LocatorUtilities.EMPTY_FILTER;
import static utam.core.selenium.element.LocatorUtilities.QUERY_FILTER_WRAPPER;
import static utam.core.selenium.element.Selector.Type.CSS;
import static utam.core.selenium.element.ShadowBoundary.EXPAND_SHADOW_ROOT;

/**
 * one of the locator links
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public abstract class LocatorNodeImpl implements LocatorNode {

  static final String NOT_FOUND_ERR_FORMAT = "'%s' not found in scope of '%s'";
  static final String NOT_FOUND_ERR_FORMAT_SHADOW_ROOT = "'%s' not found in shadow root of '%s'";
  static final String ERR_ELEMENT_SELECTED_BY = "element selected by '%s': ";
  static final String ERR_CANT_FIND_FILTERED_ELEMENTS = "can't find elements matching filter '%s'";
  private final Transformer elementContext;
  private final Selector selector;
  private LocatorNodeFilter filter;
  private LocatorNode nextElement;
  private String selectorString;

  protected LocatorNodeImpl(
      Selector selector, String selectorString, Transformer transformer, Filter filter) {
    this.elementContext = transformer;
    this.selector = selector;
    this.selectorString = selectorString;
    this.filter = (LocatorNodeFilter) (filter == null ? EMPTY_FILTER : filter);
  }

  final String getNotFoundError(LocatorNode scope) {
    String scopeSelector = scope == null ? "browser" : scope.getSelector().getValue();
    return String.format(
        (getScopeTransformer() == EXPAND_SHADOW_ROOT
            ? NOT_FOUND_ERR_FORMAT_SHADOW_ROOT
            : NOT_FOUND_ERR_FORMAT),
        getSelector().getValue(),
        scopeSelector);
  }

  @Override
  public Transformer getScopeTransformer() {
    return elementContext;
  }

  @Override
  public final LocatorNode getNext() {
    return nextElement;
  }

  final void setNext(LocatorNode next) {
    this.nextElement = next;
  }

  @Override
  public LocatorNodeFilter getFilter() {
    return filter;
  }

  @Override
  public final Selector getSelector() {
    return selector;
  }

  // public for tests to access
  public final By by() {
    return new SelectorImpl(getSelector().getType(), getSelectorString()).by();
  }

  // public for tests to access
  public final String getSelectorString() {
    return selectorString;
  }

  @Override
  public final void setIndex(int index) {
    filter = new LocatorNodeFilter.Index(index);
  }

  protected abstract LocatorNodeImpl getCopy();

  @Override
  public final void setParameters(Locator.Parameters parameters) {
    if (this instanceof Web) {
      return;
    }
    this.selectorString = parameters.apply(selector.getValue());
  }

  protected final Filter getFilterCopy() {
    return filter.getCopy();
  }

  // overridden for Web and JS node types
  List<WebElement> findElements(
      SearchContext searchContext,
      LocatorNodeImpl scope,
      WebDriverUtilities utilities,
      LocatorUtilities.Find find) {
    List<WebElement> found;
    try {
      found =
          getScopeTransformer().apply(searchContext, utilities).findElements(by());
    } catch (NotFoundException e) {
      // if "shadow root is null" error appeared, add more meaningful message abt scope
      throw new NotFoundException(getNotFoundError(scope) + ": " + e.getMessage());
    }
    if (found.isEmpty() && !find.isCanBeEmpty()) {
      throw new NotFoundException(getNotFoundError(scope));
    }
    return found;
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof LocatorNodeImpl)) {
      return false;
    }
    return getSelectorString().equals(((LocatorNodeImpl) obj).getSelectorString())
        && getFilter().equals(((LocatorNodeImpl) obj).getFilter())
        && getScopeTransformer().equals(((LocatorNodeImpl) obj).getScopeTransformer());
  }

  @Override
  public int hashCode() {
    return Objects.hash(elementContext, filter, selectorString);
  }

  final boolean isApplyFilter(LocatorUtilities.Find find) {
    return find.isFiltered() && !filter.isEmpty();
  }

  final List<WebElement> applyFilter(List<WebElement> found, LocatorUtilities.Find find) {
    if (isApplyFilter(find)) {
      found = filter.filter(found);
      if (found.isEmpty() && !find.isCanBeEmpty()) {
        throw new NotFoundException(
            String.format(ERR_CANT_FIND_FILTERED_ELEMENTS, filter.getFilterString()));
      }
    }
    return found;
  }

  static class Css extends LocatorNodeImpl {

    Css(String selector, String selectorString, Filter filter, Transformer transformer) {
      super(
          new utam.core.selenium.element.Web.SelectorImpl(CSS, selector),
          selectorString,
          transformer,
          filter);
    }

    // used in tests
    Css(String selector, Filter filter, Transformer transformer) {
      this(selector, selector, filter, transformer);
    }

    // used in tests
    Css(String selector) {
      this(selector, selector, EMPTY_FILTER, ShadowBoundary.NONE);
    }

    // used in tests
    Css(String selector, Filter filter) {
      this(selector, selector, filter, ShadowBoundary.NONE);
    }

    @Override
    protected LocatorNodeImpl getCopy() {
      return new Css(
          getSelector().getValue(),
          getSelectorString(),
          getFilter().getCopy(),
          getScopeTransformer());
    }
  }

  static class Javascript extends Css {

    Javascript(String selector, String selectorString, Filter filter, Transformer elementContext) {
      super(selector, selectorString, filter, elementContext);
    }

    // used in tests
    Javascript(String selector) {
      super(selector, selector, EMPTY_FILTER, ShadowBoundary.NONE);
    }

    // used in tests
    Javascript(String selector, LocatorNodeFilter filter) {
      super(selector, selector, filter, ShadowBoundary.NONE);
    }

    // iterate from current and accumulate JS for all elements till leaf
    void setJavascriptBuilderAll(StringBuilder builder, LocatorUtilities.Find find) {
      LocatorNodeImpl.Javascript current = this;
      while (current.getNext() != null) {
        // all elements except last should be called as element
        current.setJavascriptBuilder(
            builder, LocatorUtilities.Find.FILTERED_ELEMENT, LocatorUtilities.QueryType.ELEMENT);
        current = (LocatorNodeImpl.Javascript) current.getNext();
      }
      // last element should be queries as requested from caller
      current.setJavascriptBuilder(builder, find, LocatorUtilities.QueryType.ELEMENT);
    }

    void setJavascriptBuilder(
        StringBuilder builder, LocatorUtilities.Find find, LocatorUtilities.QueryType queryType) {
      if (isApplyFilter(find)
          // index is hardcoded in generated JS, not applied as filter
          && !(getFilter() instanceof LocatorNodeFilter.Index)) {
        String accumulated = builder.toString();
        // remove everything from builder and replace by wrapped
        builder.delete(0, accumulated.length());
        builder.append(getFilteredListQuery(accumulated, queryType));
        if (find == LocatorUtilities.Find.FILTERED_ELEMENT) {
          // if filtered list is requested, we would not need index
          builder.append("[0]");
        }
        return;
      }
      if (find.isList()) {
        builder.append(queryType.getElements(getSelectorString(), getScopeTransformer()));
        return;
      }
      builder.append(getUnfilteredIndexedElementQuery(queryType));
    }

    private String getFilteredListQuery(String accumulated, LocatorUtilities.QueryType queryType) {
      return String.format(
              QUERY_FILTER_WRAPPER,
              accumulated + queryType.getElements(getSelectorString(), getScopeTransformer()))
          + (getFilter().getJavascript());
    }

    private String getUnfilteredIndexedElementQuery(LocatorUtilities.QueryType queryType) {
      if (getFilter() instanceof LocatorNodeFilter.Index && !getFilter().isEmpty()) {
        String jsQuery = queryType.getElements(getSelectorString(), getScopeTransformer());
        return String.format("%s[%d]", jsQuery, ((LocatorNodeFilter.Index) getFilter()).index);
      }
      return queryType.getElement(getSelectorString(), getScopeTransformer());
    }

    @Override
    protected final LocatorNodeImpl getCopy() {
      return new Javascript(
          getSelector().getValue(),
          getSelectorString(),
          getFilter().getCopy(),
          getScopeTransformer());
    }
  }

  public static final class Web extends LocatorNodeImpl {

    static final Selector MOCK_SELECTOR = new utam.core.selenium.element.Web.SelectorImpl(CSS, "WebElement");
    private final Supplier<SearchContext> self;

    Web(Supplier<SearchContext> self) {
      super(MOCK_SELECTOR, "WebElement", ShadowBoundary.NONE, EMPTY_FILTER);
      this.self = () -> self.get();
    }

    @Override
    final List<WebElement> findElements(
        SearchContext searchContext,
        LocatorNodeImpl scope,
        WebDriverUtilities utilities,
        LocatorUtilities.Find find) {
      return Stream.of((WebElement)self.get()).collect(Collectors.toList());
    }

    @Override
    protected LocatorNodeImpl getCopy() {
      return new Web(self);
    }
  }
}
