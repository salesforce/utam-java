/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static utam.core.element.FindContext.Type.EXISTING;
import static utam.core.framework.base.PageMarker.getRootLocatorFromAnnotation;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import utam.core.driver.Driver;
import utam.core.driver.DriverContext;
import utam.core.element.Element;
import utam.core.element.ElementLocation;
import utam.core.element.Locator;
import utam.core.framework.consumer.PageObjectContext;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.consumer.UtamLoaderConfig;
import utam.core.framework.context.PlatformType;
import utam.core.framework.element.ElementLocationChain;
import utam.core.framework.element.ExpectationsImpl;

/**
 * selenium page objects factory
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class PageObjectsFactoryImpl implements PageObjectsFactory {

  private final PageObjectContext pageObjectContext;
  private final Driver driver;
  private final DriverContext driverContext;

  public PageObjectsFactoryImpl(
      PageObjectContext pageObjectContext, DriverContext driverContext,
      Driver driver) {
    this.pageObjectContext = pageObjectContext;
    this.driverContext = driverContext;
    this.driver = driver;
  }

  public PageObjectsFactoryImpl(UtamLoaderConfig utamLoaderConfig, Driver driver) {
    this(utamLoaderConfig.getPageContext(), utamLoaderConfig.getDriverContext(), driver);
  }

  @Override
  public void bootstrap(PageObject instance, ElementLocation root) {
    if (!(instance instanceof BasePageObject)) {
      throw new UtamError(
          String.format(
              "class '%s' it should extend '%s'",
              instance.getClass(), BasePageObject.class.getName()));
    }
    BasePageObject pageObject = (BasePageObject) instance;
    pageObject.setBootstrap(root, this);
    new FieldsBuilder(pageObject).bootstrapElements();
    setPlatform(instance);
  }

  private void setPlatform(PageObject instance) {
    PlatformType pagePlatform;
    if (instance.getClass().isAnnotationPresent(PageMarker.Switch.class)) {
      pagePlatform = instance.getClass().getAnnotation(PageMarker.Switch.class).value();
    } else {
      pagePlatform = PlatformType.WEB;
    }
    if (getDriver().isMobile()) {
      if (pagePlatform.equals(PlatformType.WEB)) {
        getDriver().setPageContextToWebView(getDriverContext().getBridgeAppTitle(),
            getDriverContext().getTimeouts().getWaitForTimeout(),
            getDriverContext().getTimeouts().getPollingInterval());
      } else {
        getDriver().setPageContextToNative();
      }
    }
  }

  @Override
  public PageObjectContext getPageContext() {
    return pageObjectContext;
  }

  @Override
  public DriverContext getDriverContext() {
    return driverContext;
  }

  @Override
  public Driver getDriver() {
    return driver;
  }

  @Override
  public Element findElement(ElementLocation location) {
    return driver.waitFor(driverContext.getTimeouts().getFindTimeout(),
        driverContext.getTimeouts().getPollingInterval(),
        new ExpectationsImpl<>("find element", location::findElement));
  }

  @Override
  public List<Element> findElements(ElementLocation location) {
    return driver.waitFor(driverContext.getTimeouts().getFindTimeout(),
        driverContext.getTimeouts().getPollingInterval(),
        new ExpectationsImpl<>("find element", location::findElements));
  }

  @Override
  public <T extends RootPageObject> T create(Class<T> rootPageObjectType) {
    T instance = getPageContext().getBean(rootPageObjectType);
    Class<? extends RootPageObject> pageObjectClass = instance.getClass();
    if (!pageObjectClass.isAnnotationPresent(PageMarker.Find.class)) {
      throw new UtamError(String.format("root selector is not set for the page object instance %s",
          pageObjectClass.getName()));
    }
    Locator rootLocator = getRootLocatorFromAnnotation(pageObjectClass.getDeclaredAnnotation(PageMarker.Find.class));
    ElementLocation rootElementLocation = new ElementLocationChain(rootLocator, EXISTING);
    bootstrap(instance, rootElementLocation);
    return instance;
  }

  // assign values to the fields
  static class FieldsBuilder {

    static final String NON_EXISTING_FIELD_ERROR = "non-existing field '%s' is referenced as a scope";

    private final BasePageObject instance;
    private final Map<String, ElementLocation> pageElements = new TreeMap<>();

    FieldsBuilder(BasePageObject instance) {
      this.instance = instance;
    }

    // get fields including base classes
    private List<Field> getFields() {
      List<Field> fields = new ArrayList<>();
      Class clazz = instance.getClass();
      while (clazz != Object.class) {
        fields.addAll(Arrays.asList(clazz.getDeclaredFields()));
        clazz = clazz.getSuperclass();
      }
      return fields;
    }

    ElementLocation getLocator(Field f) {
      ElementMarker.Find annotation = f.getDeclaredAnnotation(ElementMarker.Find.class);
      String scopeString = annotation.scope();
      ElementLocation finder;
      if (scopeString.isEmpty()) {
        finder = instance.getRootLocator()
            .scope(ElementMarker.getLocator(annotation),
                ElementMarker.getFinderContext(annotation));
      } else if (pageElements.containsKey(scopeString)) {
        finder = pageElements.get(scopeString)
            .scope(ElementMarker.getLocator(annotation),
                ElementMarker.getFinderContext(annotation));
      } else {
        throw new UtamError(String.format(NON_EXISTING_FIELD_ERROR, scopeString));
      }
      pageElements.put(f.getName(), finder);
      return finder;
    }

    void bootstrapElements() {
      getFields()
          .stream()
          .filter(f -> ElementLocation.class.isAssignableFrom(f.getType())
              && f.getAnnotation(ElementMarker.Find.class) != null)
          .forEach(
              f -> {
                ElementLocation elementLocation = getLocator(f);
                try {
                  f.setAccessible(true);
                  f.set(instance, elementLocation);
                } catch (Exception e) {
                  throw new UtamError(
                      String.format(
                          "Error while setting field '%s' in class '%s'",
                          f.getName(), instance.getClass().getSimpleName()),
                      e);
                }
              });
    }
  }
}
