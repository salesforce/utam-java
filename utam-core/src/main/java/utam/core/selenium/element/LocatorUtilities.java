package utam.core.selenium.element;

import utam.core.appium.element.AccessibilityId;
import utam.core.appium.element.ClassChain;
import utam.core.appium.element.UIAutomator;
import utam.core.framework.base.PageMarker;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.consumer.LocationPolicy;
import utam.core.framework.consumer.LocationPolicyType;
import utam.core.framework.consumer.UtamError;
import org.openqa.selenium.SearchContext;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.expectations.ElementListExpectations;
import utam.core.selenium.expectations.ExpectationsUtil;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

import static utam.core.selenium.element.Selector.Type.*;
import static utam.core.selenium.element.ShadowBoundary.EXPAND_SHADOW_ROOT;

/**
 * public utilities to work with locators <br>
 * hide its internal implementations from other packages
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class LocatorUtilities {

  public static final LocatorNode.Transformer DEFAULT_TRANSFORMER = ShadowBoundary.NONE;
  public static final LocatorNodeFilter EMPTY_FILTER = LocatorNodeFilter.Empty.INSTANCE;
  public static final String QUERY_SELECTOR = ".querySelector(\"%s\")";
  static final String ERR_LOCATOR_IS_NULL = "Element locator can't be null";
  static final String NON_EXISTING_FIELD_ERROR = "non-existing field '%s' is referenced as a scope";
  static final String QUERY_FILTER_WRAPPER = "[...%s]";
  private static final String QUERY_SHADOW_BOUNDARY = ".shadowRoot";
  private static final String JQUERY_ELEMENT = "$(\"%s\")";
  private static final String JQUERY_ELEMENT_ALL = "$$(\"%s\")";
  private static final String QUERY_SELECTOR_ALL = ".querySelectorAll(\"%s\")";

  public static Locator getSingleNodeLocator(LocatorNode element, LocationPolicy locationPolicy) {
    if (locationPolicy == LocationPolicyType.JAVASCRIPT) {
      return new LocatorImpl.Javascript(element);
    }
    return new LocatorImpl(element);
  }

  public static LocatorNode getLocatorNode(Supplier<SearchContext> supplier) {
    return new LocatorNodeImpl.Web(supplier);
  }

  /**
   * build instance of locator to use inside container or inside foreign Page Object
   * @param policy location policy from config
   * @param selector css or mobile selector
   * @param isExpandShadow boolean to expand parent shadow if locator will be scoped inside something else
   * @return instance of locator
   */
  public static Locator buildLocator(LocationPolicy policy, Selector selector, boolean isExpandShadow) {
    return getSingleNodeLocator(
        getLocatorNode(
            selector,
            LocatorUtilities.EMPTY_FILTER,
            ShadowBoundary.valueOf(isExpandShadow),
            policy),
        policy);
  }

  static Selector getElementSelectorFromAnnotation(ElementMarker.Find annotation) {
    Selector selector;
    if (!annotation.accessid().isEmpty()) {
      selector = new Web.SelectorImpl(Selector.Type.ACCESSID, annotation.accessid());
    } else if (!annotation.classchain().isEmpty()) {
      selector = new Web.SelectorImpl(Selector.Type.CLASSCHAIN, annotation.classchain());
    } else if (!annotation.uiautomator().isEmpty()) {
      selector = new Web.SelectorImpl(Selector.Type.UIAUTOMATOR, annotation.uiautomator());
    } else if (!annotation.css().isEmpty()) {
      selector = new Web.SelectorImpl(Selector.Type.CSS, annotation.css());
    } else {
      return null;
    }
    return selector;
  }

  public static Selector getPageSelectorFromAnnotation(PageMarker.Find annotation) {
    Selector selector;
    if (!annotation.accessid().isEmpty()) {
      selector = new Web.SelectorImpl(Selector.Type.ACCESSID, annotation.accessid());
    } else if (!annotation.classchain().isEmpty()) {
      selector = new Web.SelectorImpl(Selector.Type.CLASSCHAIN, annotation.classchain());
    } else if (!annotation.uiautomator().isEmpty()) {
      selector = new Web.SelectorImpl(Selector.Type.UIAUTOMATOR, annotation.uiautomator());
    } else {
      selector = new Web.SelectorImpl(Selector.Type.CSS, annotation.css());
    }
    return selector;
  }

  public static LocatorNode getLocatorNode(
      Selector selector,
      LocatorNode.Filter filter,
      LocatorNode.Transformer transformer,
      LocationPolicy locationPolicy) {
    String selectorString = selector.getValue();
    switch (selector.getType()) {
      case ACCESSID:
        return new AccessibilityId(selectorString, selectorString, filter);
      case CLASSCHAIN:
        return new ClassChain(selectorString, selectorString, filter);
      case UIAUTOMATOR:
        return new UIAutomator(selectorString, selectorString, filter);
    }
    if (locationPolicy == LocationPolicyType.JAVASCRIPT) {
      return new LocatorNodeImpl.Javascript(selectorString, selectorString, filter, transformer);
    } else {
      return new LocatorNodeImpl.Css(selectorString, selectorString, filter, transformer);
    }
  }

  public static Actionable getElement(Locator locator, SeleniumContext context) {
    if (locator == null) {
      throw new UtamError(ERR_LOCATOR_IS_NULL);
    }
    return new ElementImpl(locator, context);
  }

  public static SearchContext find(Actionable root) {
    return ((ElementImpl) root).find(false);
  }

  public static ContainerElement getContainer(
      Locator locator, boolean isShadowRoot, PageObjectsFactory factory) {
    if (locator == null) {
      throw new UtamError(ERR_LOCATOR_IS_NULL);
    }
    return new ElementContainerImpl(locator, factory, isShadowRoot);
  }

  public static LocatorImpl getElementLocator(BaseElement element) {
    return ((ElementImpl) element).getLocator();
  }

  public static LocatorNode.Transformer getContextTransformer(boolean isExpand) {
    return ShadowBoundary.valueOf(isExpand);
  }

  // public because needs to be used in tests in other module
  public static List<LocatorNode> getAllNodes(Locator locator) {
    List<LocatorNode> chain = new ArrayList<>();
    LocatorNode current = ((LocatorImpl) locator).getRoot();
    while (current != null) {
      chain.add(current);
      current = current.getNext();
    }
    return chain;
  }

  public static Selector getSelector(String selectorString, Selector.Type type) {
    return new Web.SelectorImpl(type, selectorString);
  }

  /**
   * find an element inside polling wait AKA waiting for presence
   * @param element actionable element
   * @deprecated when we refactor UI interactions, this method no longer will be needed
   */
  @Deprecated
  public static void waitForPresence(Actionable element) {
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.presence();
    new ElementWaitImpl(expectation.getLogMessage(), ((ElementImpl)element).getLocator(), ((ElementImpl)element).getSeleniumContext()).wait(expectation);
  }

  /**
   * type of JS query to build
   *
   * @since 230
   */
  enum QueryType {
    ROOT_JQUERY,
    ROOT_JS,
    ELEMENT;

    String getElements(String css, LocatorNode.Transformer transformer) {
      return getTransformerString(transformer)
          + String.format(this == ROOT_JQUERY ? JQUERY_ELEMENT_ALL : QUERY_SELECTOR_ALL, css);
    }

    String getElement(String css, LocatorNode.Transformer transformer) {
      return getTransformerString(transformer)
          + String.format(this == ROOT_JQUERY ? JQUERY_ELEMENT : QUERY_SELECTOR, css);
    }

    private String getTransformerString(LocatorNode.Transformer transformer) {
      if (this == ROOT_JQUERY || this == ROOT_JS) {
        return "";
      }
      if (transformer == EXPAND_SHADOW_ROOT) {
        return QUERY_SHADOW_BOUNDARY;
      }
      return "";
    }
  }

  /**
   * type of the element to find
   *
   * @since 230
   */
  enum Find {
    FILTERED_ELEMENT,
    FILTERED_LIST,
    FILTERED_NULLABLE_LIST,
    UNFILTERED_LIST;

    static Find getFilteredList(boolean isNullable) {
      return isNullable ? FILTERED_NULLABLE_LIST : FILTERED_LIST;
    }

    boolean isList() {
      return this == FILTERED_LIST || this == FILTERED_NULLABLE_LIST || this == UNFILTERED_LIST;
    }

    boolean isFiltered() {
      return this == FILTERED_NULLABLE_LIST || this == FILTERED_LIST || this == FILTERED_ELEMENT;
    }

    boolean isCanBeEmpty() {
      return this == FILTERED_NULLABLE_LIST;
    }

    Find getFindIntermittentElementType() {
      // if we search for nullable, intermittent elements are allowed to be not found
      return isCanBeEmpty() ? FILTERED_NULLABLE_LIST : FILTERED_LIST;
    }
  }

  public static final class Builder {

    private final LocationPolicy locationPolicy;
    private final Locator root;
    private final Map<String, Locator> pageElements;

    public Builder(LocationPolicy locationPolicy, Locator root, Map<String, Locator> pageElements) {
      this.locationPolicy = locationPolicy;
      this.root = root;
      this.pageElements = pageElements;
    }

    public LocatorImpl getLocator(Field f) {
      ElementMarker.Find annotation = f.getDeclaredAnnotation(ElementMarker.Find.class);
      Selector selector = getElementSelectorFromAnnotation(annotation);
      if (selector == null) {
        return ((LocatorImpl) root).getCopy();
      }
      LocatorNode element =
          getLocatorNode(
              selector,
              EMPTY_FILTER,
              ShadowBoundary.valueOf(annotation.expand()),
              locationPolicy);
      String scopeString = annotation.scope();
      if (scopeString.isEmpty()) {
        return (LocatorImpl) root.add(element);
      }
      if (pageElements.containsKey(scopeString)) {
        return (LocatorImpl) pageElements.get(scopeString).add(element);
      }
      throw new UtamError(String.format(NON_EXISTING_FIELD_ERROR, scopeString));
    }

    // for container element its own selector is ignored
    public LocatorImpl getContainerLocator(Field f) {
      ElementMarker.Find annotation = f.getDeclaredAnnotation(ElementMarker.Find.class);
      String scopeString = annotation.scope();
      if (scopeString.isEmpty()) {
        return (LocatorImpl) root;
      }
      if (pageElements.containsKey(scopeString)) {
        return (LocatorImpl) pageElements.get(scopeString);
      }
      throw new UtamError(String.format(NON_EXISTING_FIELD_ERROR, scopeString));
    }
  }
}
