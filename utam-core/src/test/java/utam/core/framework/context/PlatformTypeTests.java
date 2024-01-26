/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.context.PlatformType.NATIVE;
import static utam.core.framework.context.PlatformType.WEB;

import io.appium.java_client.AppiumDriver;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.MockUtilities.MockDriver;
import utam.core.driver.Driver;
import utam.core.framework.base.BaseRootPageObject;
import utam.core.framework.base.PageMarker.Find;
import utam.core.framework.base.PageMarker.Switch;

/**
 * Provides tests for the PlatformType enumerated type
 *
 * @author james.evans
 */
public class PlatformTypeTests {

  /** The fromString method should return valid values for known types */
  @Test
  public void testFromString() {
    assertThat(PlatformType.fromString("native"), is(equalTo(PlatformType.NATIVE)));
    assertThat(PlatformType.fromString("web"), is(equalTo(PlatformType.WEB)));
    assertThat(PlatformType.fromString("NATIVE"), is(equalTo(PlatformType.NATIVE)));
    assertThat(PlatformType.fromString("WEB"), is(equalTo(PlatformType.WEB)));
    assertThat(PlatformType.fromString(null), is(nullValue()));
    assertThat(PlatformType.fromString(""), is(nullValue()));
  }

  /** The fromString method should throw the appropriate exception for unknown types */
  @Test
  public void testFromStringWithUnknownTypeThrows() {
    Exception e =
        expectThrows(IllegalArgumentException.class, () -> PlatformType.fromString("illegal"));
    assertThat(e.getMessage(), containsString("No enum constant"));
  }

  @Test
  public void testSwitchContext() {
    final MockUtilities mockUtilities = new MockDriver(AppiumDriver.class);
    Driver driver = mockUtilities.getDriverAdapter();
    mockUtilities.getFactory().create(TestContextSwitchWeb.class);
    verify(driver, times(1)).setPageContext(WEB);
    mockUtilities.getFactory().create(TestContextSwitchNative.class);
    verify(driver, times(1)).setPageContext(NATIVE);
    mockUtilities.getFactory().create(TestContextSwitchEmpty.class);
    verify(driver, times(2)).setPageContext(WEB);
  }

  @Switch(WEB)
  @Find(css = "css")
  public static class TestContextSwitchWeb extends BaseRootPageObject {}

  @Switch(NATIVE)
  @Find(css = "css")
  public static class TestContextSwitchNative extends BaseRootPageObject {}

  @Find(css = "css")
  public static class TestContextSwitchEmpty extends BaseRootPageObject {}
}
