/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.mockito.Mockito.mock;
import static utam.core.framework.consumer.UtamLoaderConfigTests.getDefaultConfig;
import static utam.core.framework.consumer.UtamLoaderImpl.getSimulatorLoader;

import io.appium.java_client.AppiumDriver;
import java.util.function.Supplier;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.driver.Driver;
import utam.core.driver.DriverTimeouts;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.RootPageObject;
import utam.core.selenium.appium.MobileDriverAdapter;
import utam.core.selenium.element.DriverAdapter;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class UtamLoaderTests {

  private static UtamLoaderImpl getDefaultLoader(boolean isMobile) {
    Driver driver = new MockUtilities(isMobile ? AppiumDriver.class : WebDriver.class)
        .getDriverAdapter();
    return new UtamLoaderImpl(getDefaultConfig(), driver);
  }

  private static UtamLoaderImpl getDefaultLoader() {
    return getDefaultLoader(false);
  }

  @Test
  public void testRootPageObjectCreation() {
    UtamLoader loader = getDefaultLoader();
    RootPageObject rootPageObject = loader.create(TestLoaderConfigPageObject.class);
    assertThat(rootPageObject, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testPageObjectLoad() {
    UtamLoader loader = getDefaultLoader();
    RootPageObject rootPageObject = loader.load(TestLoaderConfigPageObject.class);
    assertThat(rootPageObject, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testCreateUtamFromContainer() {
    UtamLoader loader = getDefaultLoader();
    ContainerMock containerMock = new ContainerMock();
    TestLoaderConfigPageObject pageObjectMock =
        loader.create(containerMock, TestLoaderConfigPageObject.class, LocatorBy.byCss("root"));
    assertThat(pageObjectMock.getRoot().getLocatorChainString(),
        is(equalTo("driver > element > By.cssSelector: root")));
  }

  @Test
  public void testDefaultConstructor() {
    UtamLoaderImpl loader = new UtamLoaderImpl(getDefaultConfig(),
        new DriverAdapter(mock(WebDriver.class)));
    assertThat(loader.getConfig().getDriverContext().getTimeouts(),
        is(equalTo(DriverTimeouts.DEFAULT)));
    assertThat(loader.getDriver(), is(instanceOf(DriverAdapter.class)));
  }

  @Test
  public void testConstructor() {
    UtamLoaderImpl loader = getDefaultLoader(true);
    assertThat(loader.getConfig().getDriverContext().getTimeouts(),
        is(equalTo(DriverTimeouts.DEFAULT)));
    assertThat(loader.getDriver(), is(instanceOf(MobileDriverAdapter.class)));
  }

  @Test
  public void testDefaultConstructorForTests() {
    UtamLoaderImpl loader = (UtamLoaderImpl) getSimulatorLoader(mock(AppiumDriver.class));
    assertThat(loader.getDriver(), is(instanceOf(MobileDriverAdapter.class)));
    assertThat(loader.getConfig().getDriverContext().getTimeouts(),
        is(equalTo(DriverTimeouts.TEST)));
  }

  @Test
  public void testResetConfig() {
    UtamLoaderImpl loader = getDefaultLoader();
    PageObjectsFactory factory = loader.getFactory();
    loader.resetContext();
    assertThat(loader.getFactory(), is(not(sameInstance(factory))));
  }

  @Test
  public void testGetDocument() {
    assertThat(getDefaultLoader().getDocument(), is(notNullValue()));
  }

  private static class ContainerMock implements Container {

    final Supplier<SearchContext> root = () -> mock(WebElement.class);

    @Override
    public Supplier<SearchContext> getScope() {
      return root;
    }
  }
}
