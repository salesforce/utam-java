/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import static org.mockito.Mockito.when;

import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Point;
import org.openqa.selenium.Rectangle;
import org.testng.annotations.Test;
import utam.core.MockUtilities;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class MobileElementAdapterTests {

  @Test
  public void testFlickIOS() {
    MockUtilities mock = new MockUtilities(IOSDriver.class);
    when(mock.getMobileDriverAdapter().getWebViewElement()).thenReturn(mock.getWebElementMock());
    when(mock.getWebElementMock().getLocation()).thenReturn(new Point(1, 2));
    when(mock.getWebElementMock().getRect()).thenReturn(new Rectangle(1, 2, 10, 20));
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
    when(mock.getWebElementMock().getLocation()).thenReturn(new Point(1, 2));
    when(mock.getWebElementMock().getRect()).thenReturn(new Rectangle(1, 2, 10, 20));
    when(mock.getWebElementMock().getSize()).thenReturn(new Dimension(10, 20));
    String title = "title";
    when(mock.getAppiumDriverMock().getTitle()).thenReturn(title);
    when(mock.getAppiumDriverMock().getContext()).thenReturn(title);
    mock.getElementAdapter().flick(1, 1);
  }
}
