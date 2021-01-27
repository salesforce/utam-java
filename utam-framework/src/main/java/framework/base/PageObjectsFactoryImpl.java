/*
 * Copyright, 1999-2018, salesforce.com
 * All Rights Reserved
 * Company Confidential
 * Project UTAM
 */

package framework.base;

import appium.context.AppiumDriverUtilities;
import framework.context.Driver;
import framework.consumer.PageObjectContext;
import framework.context.PlatformType;
import org.openqa.selenium.WebDriver;
import selenium.context.SeleniumContext;

import java.lang.reflect.Field;

public class PageObjectsFactoryImpl implements PageObjectsFactory {

  private final PageObjectContext pageObjectContext;
  private final SeleniumContext seleniumContext;

  public PageObjectsFactoryImpl(
      PageObjectContext pageObjectContext, SeleniumContext seleniumContext) {
    this.pageObjectContext = pageObjectContext;
    this.seleniumContext = seleniumContext;
  }

  static void setField(BasePageObject pageObject, Field field, Object instance) {
    if (instance == null) {
      return;
    }
    try {
      field.setAccessible(true);
      field.set(pageObject, instance);
    } catch (Exception e) {
      throw new RuntimeException(
          String.format(
              "Error while setting field '%s' in class '%s'",
              field.getName(), pageObject.getClass().getSimpleName()),
          e);
    }
  }

  static void bootstrapPageContext(PageObject instance, PageObjectsFactory factory) {
    SeleniumContext currentSeleniumContext = factory.getSeleniumContext();
    WebDriver currentDriver = currentSeleniumContext.getWebDriverUtils().getWebDriver();
    if (Driver.isMobileDriver(currentDriver)) {
      AppiumDriverUtilities appiumWdUtil =
          (AppiumDriverUtilities) currentSeleniumContext.getWebDriverUtils();
      BasePageObject pageObject = BasePageObject.castToImpl(instance);
      PlatformType currentPagePlatform = pageObject.getPagePlatform();
      if (currentPagePlatform.equals(PlatformType.WEB)) {
        appiumWdUtil.setPageContextToWebView();
      } else {
        appiumWdUtil.setPageContextToNative();
      }
    }
  }

  @Override
  public void bootstrap(PageObject instance, BootstrapParameters parameters) {
    internalBootstrap(BasePageObject.castToImpl(instance), parameters);
  }

  private void internalBootstrap(BasePageObject pageObject, BootstrapParameters parameters) {
    pageObject.factory = this;
    pageObject.rootLocator = parameters.getScopedRoot();
    pageObject.bootstrapPageContext();
    pageObject.bootstrapElements();
  }

  @Override
  public PageObjectContext getContext() {
    return pageObjectContext;
  }

  @Override
  public SeleniumContext getSeleniumContext() {
    return seleniumContext;
  }
}
