/*
 * @Copyright, 1999-2018, salesforce.com
 *  All Rights Reserved
 *  Company Confidential
 *  Project LPOP
 */

package utam.core.framework.base;

import utam.core.framework.consumer.Container;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.consumer.LocationPolicy;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.PlatformType;
import utam.core.selenium.element.*;

import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Stream;

import static utam.core.framework.UtamLogger.info;
import static utam.core.framework.base.PageObjectsFactoryImpl.setField;
import static utam.core.selenium.element.LocatorUtilities.*;
import static utam.core.selenium.element.PageObjectElementBuilderImpl.setContainerParameters;

/**
 * base class for any utam page object
 * @author elizaveta.ivanova
 * @since 228
 */
public abstract class BasePageObject implements RootPageObject {

  private final Map<String, Locator> pageElements = new TreeMap<>();
  private final PlatformType pagePlatform;
  // lazy element injected by factory
  Locator rootLocator;
  // lazy factory
  PageObjectsFactory factory;
  // lazy element created by factory
  private Actionable root;

  protected BasePageObject() {
    if (getClass().isAnnotationPresent(PageMarker.Switch.class)) {
      pagePlatform = getClass().getAnnotation(PageMarker.Switch.class).value();
    } else {
      pagePlatform = PlatformType.WEB;
    }
  }

  /**
   * cast object to this base class
   *
   * @param component instance of the object
   * @return instance of this class
   */
  static BasePageObject castToImpl(Object component) {
    if (component == null) {
      return null;
    }
    if (component instanceof BasePageObject) {
      return (BasePageObject) component;
    }
    throw new UtamError(
        String.format(
            "to bootstrap class '%s' it should extend '%s'",
            component.getClass(), BasePageObject.class.getName()));
  }

  private String getErrorMsg(String message) {
    return "Page Object " + getClass().getName() + ": " + message;
  }

  protected final Actionable getRootElement() {
    if (root == null) {
      throw new NullPointerException(getErrorMsg("root element is null"));
    }
    return root;
  }

  private PageObjectsFactory getFactory() {
    if (factory == null) {
      throw new NullPointerException(getErrorMsg("factory is null"));
    }
    return factory;
  }

  protected Locator getRootLocator() {
    if (rootLocator == null) {
      throw new NullPointerException(getErrorMsg("root locator is null"));
    }
    return rootLocator;
  }

  private void log(String message) {
    if (!message.isEmpty()) {
      info(String.format("Page Object '%s': %s", getClass().getSimpleName(), message));
    }
  }

  @Override
  public void load() {
    log("wait for load");
    waitForPresence(getRootElement());
  }

  final void bootstrapElements() {
    LocationPolicy locationPolicy = getFactory().getSeleniumContext().getLocationPolicy();
    LocatorUtilities.Builder builder =
        new LocatorUtilities.Builder(locationPolicy, getRootLocator(), pageElements);
    Stream.of(getClass().getDeclaredFields())
        .filter(f -> BaseElement.class.isAssignableFrom(f.getType()))
        // fields in natural order by name
        .forEach(
            f -> {
              Locator locator = builder.getLocator(f);
              setField(
                  this, f, LocatorUtilities.getElement(locator, getFactory().getSeleniumContext()));
              pageElements.put(f.getName(), locator);
            });
    root = LocatorUtilities.getElement(getRootLocator(), getFactory().getSeleniumContext());
    Stream.of(getClass().getDeclaredFields())
        .filter(f -> ContainerElement.class.isAssignableFrom(f.getType()))
        // fields in natural order by name
        .forEach(
            f -> {
              Locator container = builder.getContainerLocator(f);
              setField(
                  this,
                  f,
                  LocatorUtilities.getContainer(
                      container, f.getAnnotation(ElementMarker.Find.class).expand(), getFactory()));
              pageElements.put(f.getName(), container);
            });
  }

  final void bootstrapPageContext() {
    PageObjectsFactoryImpl.bootstrapPageContext(this, getFactory());
  }

  final PlatformType getPagePlatform() {
    return pagePlatform;
  }

  @Override
  public final boolean isPresent() {
    log("check for immediate presence inside its scope");
    return getRootElement().isPresent();
  }

  @SuppressWarnings("unused")
  // used by generator - scope inside element of the page object
  protected final PageObjectBuilder inScope(
      BaseElement scopeElement, Locator selector, boolean isNullable) {
    return new PageObjectBuilderImpl(
        getFactory(), getElementLocator(scopeElement), isNullable, selector);
  }

  @SuppressWarnings("unused")
  // used by generator for external page objects only (result is never nullable)
  protected final PageObjectBuilder inScope(BaseElement scopeElement, Locator selector) {
    return new PageObjectBuilderImpl.ExternalChild(
        getFactory(), getElementLocator(scopeElement), selector);
  }

  protected final PageObjectElementBuilder element(BaseElement pageObjectElement) {
    return new PageObjectElementBuilderImpl(getFactory(), pageObjectElement);
  }

  protected final ContainerElement inContainer(BaseElement scopeElement, boolean isExpandShadow) {
    return getContainer(getElementLocator(scopeElement), isExpandShadow, getFactory());
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  // used by generator - return imperative utility
  protected <T extends ImperativeProvider> T getUtility(Class<T> type) {
    T utility = ImperativeProvider.build(type);
    utility.setInstance(this);
    return utility;
  }

  // used by generator - set container elements parameter
  protected final ContainerElement setParameters(ContainerElement element, Object... values) {
    return setContainerParameters(getFactory(), element, values);
  }

  // used by generator - injects selector when building component
  protected Locator by(String selectorString, Selector.Type type, boolean isExpandScopeShadowRoot) {
    LocationPolicy policy = getFactory().getSeleniumContext().getLocationPolicy();
    LocatorNode element =
        getLocatorNode(
            LocatorUtilities.getSelector(selectorString, type),
            LocatorUtilities.EMPTY_FILTER,
            LocatorUtilities.getContextTransformer(isExpandScopeShadowRoot),
            policy);
    return LocatorUtilities.getSingleNodeLocator(element, policy);
  }

  // used by generator - injects selector when building container
  protected Selector by(String selectorString, Selector.Type type) {
    return LocatorUtilities.getSelector(selectorString, type);
  }
}
