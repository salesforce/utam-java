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
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.consumer.UtamLoaderConfigTests.getDefaultConfig;
import static utam.core.framework.consumer.UtamLoaderImpl.getSimulatorLoader;

import io.appium.java_client.AppiumDriver;
import java.time.Duration;
import java.util.Collections;
import java.util.function.Supplier;
import org.openqa.selenium.By;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.driver.Driver;
import utam.core.element.FrameElement;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.RootPageObject;
import utam.core.framework.consumer.impl.TestLoaderConfigPageObjectImpl;
import utam.core.selenium.appium.MobileDriverAdapter;
import utam.core.selenium.element.DriverAdapter;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class UtamLoaderTests {

  private static UtamLoaderImpl getDefaultLoader(boolean isMobile) {
    Driver driver =
        new MockUtilities(isMobile ? AppiumDriver.class : WebDriver.class).getDriverAdapter();
    return new UtamLoaderImpl(getDefaultConfig(), driver);
  }

  private static UtamLoaderImpl getDefaultLoader() {
    return getDefaultLoader(false);
  }

  @Test
  public void testRootPageObjectCreation() {
    UtamLoader loader = getDefaultLoader();
    RootPageObject rootPageObject = loader.create(TestLoaderConfigPageObject.class);
    assertThat(rootPageObject, is(instanceOf(TestLoaderConfigPageObjectImpl.class)));
  }

  @Test
  public void testPageObjectLoad() {
    UtamLoader loader = getDefaultLoader();
    RootPageObject rootPageObject = loader.load(TestLoaderConfigPageObject.class);
    assertThat(rootPageObject, is(instanceOf(TestLoaderConfigPageObjectImpl.class)));
  }

  @Test
  public void testCreateUtamFromContainer() {
    UtamLoader loader = getDefaultLoader();
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.findElements(By.cssSelector("root")))
        .thenReturn(Collections.singletonList(mockElement));
    ContainerMock containerMock = new ContainerMock(() -> mockElement);
    TestLoaderConfigPageObject pageObjectMock =
        loader.create(containerMock, TestLoaderConfigPageObject.class, LocatorBy.byCss("root"));
    // driver > element > By.cssSelector: root
    assertThat(pageObjectMock.getRoot().getStringValue(), is(equalTo("root")));
  }

  @Test
  public void testDefaultConstructor() {
    UtamLoaderImpl loader =
        new UtamLoaderImpl(getDefaultConfig(), new DriverAdapter(mock(WebDriver.class), null));
    assertThat(loader.getDriver(), is(instanceOf(DriverAdapter.class)));
  }

  @Test
  public void testConstructorWithConfigAndDriver() {
    UtamLoaderImpl loader = new UtamLoaderImpl(getDefaultConfig(), mock(WebDriver.class));
    assertThat(loader.getDriver(), is(instanceOf(DriverAdapter.class)));
  }

  @Test
  public void testConstructor() {
    UtamLoaderImpl loader = getDefaultLoader(true);
    assertThat(loader.getDriver(), is(instanceOf(MobileDriverAdapter.class)));
  }

  @Test
  public void testDefaultConstructorForTests() {
    UtamLoaderImpl loader = (UtamLoaderImpl) getSimulatorLoader(mock(AppiumDriver.class));
    assertThat(loader.getDriver(), is(instanceOf(MobileDriverAdapter.class)));
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

  @Test
  public void testGetNavigation() {
    assertThat(getDefaultLoader().getNavigation(), is(notNullValue()));
  }

  @Test
  public void testEnterFrame() {
    UtamLoader loader = getDefaultLoader();
    FrameElement frameElement = new MockUtilities().getFrameElement();
    loader.enterFrame(frameElement);
  }

  @Test
  public void testEnterFrameAndLoad() {
    UtamLoader loader = getDefaultLoader();
    FrameElement frameElement = new MockUtilities().getFrameElement();
    loader.enterFrameAndLoad(frameElement, TestLoaderConfigPageObject.class);
  }

  @Test
  public void testExitToParentFrame() {
    UtamLoader loader = getDefaultLoader();
    loader.exitToParentFrame();
  }

  @Test
  public void testExitFrame() {
    UtamLoader loader = getDefaultLoader();
    loader.exitFrame();
  }

  @Test
  public void testGetConfig() {
    UtamLoader loader = getDefaultLoader();
    assertThat(loader.getConfig(), is(instanceOf(UtamLoaderConfig.class)));
  }

  @Test
  public void testDriverConfigReset() {
    UtamLoaderConfig config = new UtamLoaderConfigImpl();
    UtamLoaderImpl loader = new UtamLoaderImpl(config, mock(WebDriver.class));
    loader.getConfig().setBridgeAppTitle("title");
    loader.resetContext();
    assertThat(loader.getDriver().getDriverConfig().getBridgeAppTitle(), is(equalTo("title")));
  }

  @Test
  public void testBridgeAppInDriverConfig() {
    UtamLoaderConfig config = new UtamLoaderConfigImpl();
    UtamLoaderImpl loader = new UtamLoaderImpl(config, mock(WebDriver.class));
    assertThat(loader.getDriver().getDriverConfig().getBridgeAppTitle(), is(emptyString()));
    loader.resetContext();
    assertThat(loader.getDriver().getDriverConfig().getBridgeAppTitle(), is(emptyString()));
  }

  @Test
  public void testWaitFor() {
    UtamLoader loader = getDefaultLoader();
    Object returned = loader.waitFor(() -> true, "message");
    assertThat(returned, equalTo(true));
    expectThrows(
        TimeoutException.class, () -> loader.waitFor(() -> false, "test", Duration.ofSeconds(1)));
  }

  private static class ContainerMock implements Container {

    final Supplier<SearchContext> root;

    ContainerMock(Supplier<SearchContext> root) {
      this.root = root;
    }

    @Override
    public Supplier<SearchContext> getScope() {
      return root;
    }
  }
}
