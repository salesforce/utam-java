/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.TouchAction;
import io.appium.java_client.touch.WaitOptions;
import io.appium.java_client.touch.offset.PointOption;
import java.time.Duration;
import org.openqa.selenium.Point;
import org.openqa.selenium.WebElement;
import utam.core.driver.Driver;
import utam.core.selenium.element.ElementAdapter;

/**
 * implementations for mobile browser, web implementation of same methods throws
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class MobileElementAdapter extends ElementAdapter {

  private static final Duration DEFAULT_FLICK_ACTION_WAIT_MILLISECONDS = Duration.ofMillis(500);

  public MobileElementAdapter(WebElement element, Driver driverAdapter) {
    super(element, driverAdapter);
  }

  @Override
  public void flick(int xOffset, int yOffset) {
    AppiumDriver appiumDriver = MobileDriverAdapter.getAppiumDriver(driverAdapter);
    Point nativeStartPoint = getWebElement().getLocation().moveBy(
        getWebElement().getSize().getWidth() / 2,
        getWebElement().getSize().getHeight() / 2
    );
    Point nativeEndPoint = nativeStartPoint.moveBy(xOffset, yOffset);
    Point[] movement = MobileDriverUtils.getFlickCoordinates((MobileDriverAdapter) driverAdapter, nativeStartPoint, nativeEndPoint);
    driverAdapter.setPageContextToNative();
    Point start = movement[0];
    Point end = movement[1];
    // simulate flick using touch control
    new TouchAction(appiumDriver)
        .press(PointOption.point(start.getX(), start.getY()))
        .waitAction(WaitOptions.waitOptions(DEFAULT_FLICK_ACTION_WAIT_MILLISECONDS))
        .moveTo(PointOption.point(end.getX(), end.getY()))
        .release()
        .perform();
  }

  @Override
  public boolean flickItems(GestureDirection direction) {
    // todo
    return false;
  }
}
