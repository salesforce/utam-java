package framework.base;

import framework.consumer.*;
import org.openqa.selenium.NotFoundException;
import org.openqa.selenium.SearchContext;
import selenium.element.*;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
import java.util.function.Supplier;

import static framework.base.BasePageObject.castToImpl;
import static selenium.element.LocatorUtilities.getLocatorNode;

/**
 * builder for page objects <br>
 * unlike factory, it's using already existing PO and its factory
 *
 * @author elizaveta.ivanova
 * @since 224
 */
public class PageObjectBuilderImpl implements PageObjectBuilder, BootstrapParameters {

  private static final String ERR_ROOT_IS_NOT_SET =
      "root selector is not set for the page object instance %s";
  final PageObjectsFactory factory;
  // parent scope can be null if we build root page object
  private final Locator optionalParentScope;
  // if set to true, can return null or empty list
  private final boolean isNullable;
  // can be set from annotation for root Page Object, so not final
  Locator injectedRoot;

  public PageObjectBuilderImpl(
      PageObjectsFactory factory, Locator parentScope, boolean isNullable, Locator injectedRoot) {
    this.factory = factory;
    this.optionalParentScope = parentScope;
    this.injectedRoot = injectedRoot;
    this.isNullable = isNullable;
  }

  /**
   * this constructor is used to build a page object from loader
   *
   * @param factory page objects factory
   * @param injectedRoot root locator
   */
  public PageObjectBuilderImpl(PageObjectsFactory factory, Locator injectedRoot) {
    this(factory, null, false, injectedRoot);
  }

  /**
   * this constructor is used to build a root page object from loader <br>
   * root will be injected from annotation
   *
   * @param factory page objects factory
   */
  public PageObjectBuilderImpl(PageObjectsFactory factory) {
    this(factory, null, false, null);
  }

  private static String getRootNotSetError(Object instance) {
    return String.format(ERR_ROOT_IS_NOT_SET, instance.getClass().getName());
  }

  private static Locator getSearchContextLocator(Supplier<SearchContext> supplier) {
    return LocatorUtilities.getSingleNodeLocator(
        LocatorUtilities.getLocatorNode(supplier), LocationPolicyType.getDefault());
  }

  @Override
  public <T extends PageObject> T build(Class<T> type, Predicate<T> filter) {
    T instance = buildByClass(type, 0);
    if (filter == null || filter.test(instance)) {
      return instance;
    }
    BaseElement rootElement = castToImpl(instance).getRootElement();
    int quantity =
        new PageObjectElementBuilderImpl(factory, rootElement).buildList(Actionable.class).size();
    for (int i = 1; i < quantity; i++) {
      T indexedObject = buildByClass(type, i);
      if (filter.test(indexedObject)) {
        return indexedObject;
      }
    }
    if (isNullable) {
      return null;
    }
    String errMessage =
        String.format("Can't find Page Object [%s] that matches condition", type.getName());
    throw new NotFoundException(errMessage);
  }

  @Override
  public <T extends PageObject> T build(Class<T> type) {
    return build(type, null);
  }

  @Override
  public <T extends PageObject> List<T> buildList(Class<T> type, Predicate<T> filter) {
    List<T> res = new ArrayList<>();
    T instance = buildByClass(type, 0);
    if (filter == null || filter.test(instance)) {
      res.add(instance);
    }
    BaseElement rootElement = castToImpl(instance).getRootElement();
    int quantity =
        new PageObjectElementBuilderImpl(factory, rootElement).buildList(Actionable.class).size();
    for (int i = 1; i < quantity; i++) {
      T indexedObject = buildByClass(type, i);
      if (filter == null || filter.test(indexedObject)) {
        res.add(indexedObject);
      }
    }
    return res;
  }

  @Override
  public <T extends PageObject> List<T> buildList(Class<T> type) {
    return buildList(type, null);
  }

  <T extends PageObject> T buildByClass(Class<T> type, int index) {
    T instance = factory.getContext().getBean(type);
    setRootLocator(instance, index);
    factory.bootstrap(instance, this);
    return instance;
  }

  @Override
  public Locator getScopedRoot() {
    if (optionalParentScope == null) {
      return injectedRoot;
    }
    return optionalParentScope.scope(injectedRoot);
  }

  void setRootLocator(Object instance, int index) {
    if (instance instanceof Container || instance instanceof Contained) {
      throw new UtamError(
          String.format(
              "wrong builder used to scope Page Object '%s'", instance.getClass().getName()));
    }
    if (injectedRoot == null) {
      if (!instance.getClass().isAnnotationPresent(PageMarker.Find.class)) {
        throw new UtamError(getRootNotSetError(instance));
      }
      // it's root PO type, so isExpand is always false
      PageMarker.Find markerFind = instance.getClass().getDeclaredAnnotation(PageMarker.Find.class);
      injectedRoot = getLocator(LocatorUtilities.getPageSelectorFromAnnotation(markerFind));
    }
    if (index > 0) {
      injectedRoot = injectedRoot.setIndex(index);
    }
  }

  Locator getLocator(Selector selector) {
    LocationPolicy policy = factory.getSeleniumContext().getLocationPolicy();
    LocatorNode element =
        getLocatorNode(
            selector, LocatorUtilities.EMPTY_FILTER, LocatorUtilities.DEFAULT_TRANSFORMER, policy);
    return LocatorUtilities.getSingleNodeLocator(element, policy);
  }

  /**
   * UTAM PO as a parent of external PO <br>
   * it needs to get
   */
  static class ExternalChild extends PageObjectBuilderImpl {

    private static final Object[] EMPTY_PARAMS = new Object[0];

    ExternalChild(PageObjectsFactory factory, Locator parentScope, Locator injectedRoot) {
      super(factory, getParentSearchContext(factory, parentScope), false, injectedRoot);
    }

    private static Locator getParentSearchContext(PageObjectsFactory factory, Locator parentScope) {
      Actionable scope = LocatorUtilities.getElement(parentScope, factory.getSeleniumContext());
      return getSearchContextLocator(() -> LocatorUtilities.find(scope));
    }

    @Override
    <T extends PageObject> T buildByClass(Class<T> type, int index) {
      T instance = factory.getContext().getExternalBean(type, EMPTY_PARAMS);
      if (!(instance instanceof Contained)) {
        throw new UtamError(
            String.format(
                "wrong builder used to scope Page Object '%s' inside UTAM parent",
                instance.getClass().getName()));
      }
      ((Contained) instance).setRoot(getRootSupplier(instance));
      return instance;
    }

    private Supplier<SearchContext> getRootSupplier(Object instance) {
      if (injectedRoot == null) {
        throw new UtamError(getRootNotSetError(instance));
      }
      Actionable root = LocatorUtilities.getElement(injectedRoot, factory.getSeleniumContext());
      return () -> LocatorUtilities.find(root);
    }

    @Override
    final void setRootLocator(Object instance, int index) {
      if (!(instance instanceof Contained)) {
        throw new UtamError(
            String.format(
                "wrong builder used to scope Page Object '%s' inside UTAM parent",
                instance.getClass().getName()));
      }
      injectedRoot = getSearchContextLocator(getRootSupplier(instance));
    }
  }

  /** UTAM PO as a child of external PO <br> */
  public static class UtamChild extends PageObjectBuilderImpl {

    public UtamChild(PageObjectsFactory factory, Container container, Selector injectSelector) {
      super(factory, getSearchContextLocator(container.getScope()), false, null);
      this.injectedRoot = getLocator(injectSelector);
    }

    @Override
    void setRootLocator(Object instance, int index) {
      // locator already set
      if (instance instanceof Container || instance instanceof Contained) {
        throw new UtamError(
            String.format(
                "wrong builder used to scope Page Object '%s'", instance.getClass().getName()));
      }
    }
  }
}
