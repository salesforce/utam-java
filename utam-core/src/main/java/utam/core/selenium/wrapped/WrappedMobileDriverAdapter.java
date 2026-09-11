/*
 * Copyright (c) 2026, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.wrapped;

import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import utam.core.driver.DriverConfig;
import utam.core.element.Element;
import utam.core.selenium.appium.MobileDriverAdapter;

/**
 * WrappedMobileDriverAdapter wraps a {@link WrappedDriverDecorator} whose underlying driver is an
 * {@link AppiumDriver}.
 *
 * <p>This exists because mobile hybrid tests need both:
 *
 * <ul>
 *   <li>WebView context switching ({@code mobile: getContexts}, {@code SupportsContextSwitching}),
 *       which only {@link MobileDriverAdapter} performs.
 *   <li>Custom {@code findElement}/{@code findElements} routing through the wrapper (e.g. for
 *       smart-finder libraries that intercept element lookups).
 * </ul>
 *
 * <p>Routing strategy:
 *
 * <ul>
 *   <li>{@link DriverAdapter#getSeleniumDriver()} returns the wrapped driver, so {@code
 *       findElement} / {@code findElements} flow through it.
 *   <li>{@link MobileDriverAdapter#getAppiumDriver()} is overridden to return the unwrapped {@link
 *       AppiumDriver}, so context switching, {@code mobile: getContexts}, and other Appium-only
 *       APIs operate on the real driver.
 * </ul>
 *
 * @author mahesh.nagamalla
 * @since 266
 */
public class WrappedMobileDriverAdapter extends MobileDriverAdapter {

  private final WrappedDriverDecorator wrappedDriver;
  private final AppiumDriver appiumDriver;

  /**
   * Constructs a WrappedMobileDriverAdapter.
   *
   * @param wrappedDriver the wrapped driver decorator (used as the Selenium driver for {@code
   *     findElement} routing)
   * @param appiumDriver the underlying AppiumDriver (used for context switching and other
   *     Appium-only APIs)
   * @param driverConfig driver configuration
   */
  public WrappedMobileDriverAdapter(
      WrappedDriverDecorator wrappedDriver, AppiumDriver appiumDriver, DriverConfig driverConfig) {
    super(appiumDriver, driverConfig);
    this.wrappedDriver = wrappedDriver;
    this.appiumDriver = appiumDriver;
    // After super-ctor, the inherited Selenium driver field is the AppiumDriver. Replace it with
    // the wrappedDriver so findElement/findElements (resolved on the inherited driver) flow
    // through the wrapper. getAppiumDriver() below keeps context switching pointed at the real
    // AppiumDriver.
    resetDriver(wrappedDriver);
  }

  /** {@inheritDoc} */
  @Override
  protected WebDriver unwrap() {
    return wrappedDriver.unwrap();
  }

  /**
   * {@inheritDoc}
   *
   * <p>Returns the underlying {@link AppiumDriver} (not {@code getSeleniumDriver()}, which is the
   * wrapped driver). All Appium-only APIs in {@link MobileDriverAdapter} (context switching, {@code
   * mobile: getContexts}, etc.) flow through this method, so they operate on the real driver.
   */
  @Override
  protected AppiumDriver getAppiumDriver() {
    return appiumDriver;
  }

  /**
   * Wrap a WebElement into an Element. If the element is a {@link WrappedElementDecorator}, creates
   * a {@link WrappedElementAdapter} with smart finding capabilities.
   */
  @Override
  protected Element wrapElement(WebElement element) {
    if (element instanceof WrappedElementDecorator) {
      return new WrappedElementAdapter((WrappedElementDecorator) element, this);
    }
    return super.wrapElement(element);
  }
}
