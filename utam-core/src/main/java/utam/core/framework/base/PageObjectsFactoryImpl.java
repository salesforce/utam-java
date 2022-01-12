/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static utam.core.framework.base.ElementMarker.getElementLocation;
import static utam.core.framework.base.PageMarker.getRootLocatorFromAnnotation;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import utam.core.driver.Driver;
import utam.core.element.Element;
import utam.core.element.Locator;
import utam.core.framework.consumer.PageObjectContext;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.consumer.UtamLoaderConfig;
import utam.core.framework.context.PlatformType;

/**
 * selenium page objects factory
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class PageObjectsFactoryImpl implements PageObjectsFactory {

  private final PageObjectContext pageObjectContext;
  private final Driver driver;
  private final String bridgeAppTitle;

  /**
   * Initializes a new instance of the PageObjectsFactoryImpl class
   *
   * @param pageObjectContext the Page Object context
   * @param bridgeAppTitle    the bridge app title for mobile apps
   * @param driver            the driver instance
   */
  public PageObjectsFactoryImpl(
      PageObjectContext pageObjectContext,
      String bridgeAppTitle,
      Driver driver) {
    this.pageObjectContext = pageObjectContext;
    this.driver = driver;
    this.bridgeAppTitle = bridgeAppTitle;
  }

  /**
   * Initializes a new instance of the PageObjectsFactoryImpl class
   *
   * @param utamLoaderConfig the loader configuration for the UTAM loader
   * @param driver           the driver instance
   */
 public PageObjectsFactoryImpl(UtamLoaderConfig utamLoaderConfig, Driver driver) {
    this(utamLoaderConfig.getPageContext(), utamLoaderConfig.getBridgeAppTitle(), driver);
  }

  /**
   * read annotation of class and build Locator from it
   *
   * @param pageObjectInstance instance of the Page Object
   * @return locator instance
   */
  public static Locator getRootLocator(RootPageObject pageObjectInstance) {
    Class<? extends RootPageObject> pageObjectClass = pageObjectInstance.getClass();
    if (!pageObjectClass.isAnnotationPresent(PageMarker.Find.class)) {
      throw new UtamError(String.format("root selector is not set for the page object instance %s",
          pageObjectClass.getName()));
    }
    return getRootLocatorFromAnnotation(
        pageObjectClass.getDeclaredAnnotation(PageMarker.Find.class));
  }

  @Override
  public void bootstrap(PageObject instance, Element element, Locator locator) {
    if (!(instance instanceof BasePageObject)) {
      throw new UtamError(
          String.format(
              "class '%s' it should extend '%s'",
              instance.getClass(), BasePageObject.class.getName()));
    }
    BasePageObject pageObject = (BasePageObject) instance;
    pageObject.initialize(this, element, locator);
    bootstrapElements(pageObject);
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
        getDriver().setPageContextToWebView(bridgeAppTitle);
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
  public Driver getDriver() {
    return driver;
  }

  @Override
  public <T extends RootPageObject> T create(Class<T> rootPageObjectType) {
    T instance = getPageContext().getBean(rootPageObjectType);
    bootstrap(instance, null, getRootLocator(instance));
    return instance;
  }

  private static List<Field> getFields(BasePageObject instance) {
    List<Field> fields = new ArrayList<>();
    Class clazz = instance.getClass();
    while (clazz != Object.class) {
      fields.addAll(Arrays.asList(clazz.getDeclaredFields()));
      clazz = clazz.getSuperclass();
    }
    return fields;
  }

  private static void bootstrapElements(BasePageObject instance) {
    getFields(instance)
        .stream()
        .filter(f -> ElementLocation.class.isAssignableFrom(f.getType())
            && f.getAnnotation(ElementMarker.Find.class) != null)
        .forEach(
            f -> {
              ElementMarker.Find annotation = f.getDeclaredAnnotation(ElementMarker.Find.class);
              ElementLocation elementLocation = getElementLocation(annotation);
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
