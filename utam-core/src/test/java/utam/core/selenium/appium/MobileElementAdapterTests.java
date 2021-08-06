/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.when;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Point;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Element.GestureDirection;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class MobileElementAdapterTests {

  @Test
  public void testFlickIOS() {
    MockUtilities mock = new MockUtilities(IOSDriver.class);
    when(mock.getMobileDriverAdapter().getWebViewElement()).thenReturn(mock.getWebElementMock());
    when(mock.getWebElementMock().getLocation()).thenReturn(new Point(1,2));
    when(mock.getWebElementMock().getSize()).thenReturn(new Dimension(10, 20));
    String title = "title";
    when(mock.getAppiumDriverMock().getTitle()).thenReturn(title);
    when(mock.getAppiumDriverMock().getContext()).thenReturn(title);
    mock.getElementAdapter().flick(1, 1);
  }

  @Test
  public void testFlickAndroid() {
    MockUtilities mock = new MockUtilities(AndroidDriver.class);
    when(mock.getMobileDriverAdapter().getWebViewElement()).thenReturn(mock.getWebElementMock());
    when(mock.getWebElementMock().getLocation()).thenReturn(new Point(1,2));
    when(mock.getWebElementMock().getSize()).thenReturn(new Dimension(10, 20));
    String title = "title";
    when(mock.getAppiumDriverMock().getTitle()).thenReturn(title);
    when(mock.getAppiumDriverMock().getContext()).thenReturn(title);
    mock.getElementAdapter().flick(1, 1);
  }

  @Test
  public void testFlickItems() {
    MockUtilities mock = new MockUtilities.MockAdapter(AppiumDriver.class);
    assertThat(mock.getElementAdapter().flickItems(GestureDirection.DOWN), is(false));
  }
}
