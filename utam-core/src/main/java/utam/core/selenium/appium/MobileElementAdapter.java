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
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebElement;
import utam.core.driver.Driver;
import utam.core.element.Element;
import utam.core.selenium.element.ElementAdapter;

/**
 * implementations for mobile browser, web implementation of same methods throws
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class MobileElementAdapter extends ElementAdapter {

  private static final Duration DEFAULT_FLICK_ACTION_WAIT_MILLISECONDS = Duration.ofMillis(500);

  /**
   * Initializes a new instance of the MobileElementAdapter class
   *
   * @param element the underlying element to wrap
   * @param driverAdapter the driver used to drive this element
   */
  public MobileElementAdapter(WebElement element, Driver driverAdapter) {
    super(element, driverAdapter);
  }

  @Override
  public void flick(int xOffset, int yOffset) {
    AppiumDriver appiumDriver = MobileDriverAdapter.getAppiumDriver(driverAdapter);
    Rectangle rect = getWebElement().getRect();
    Point nativeStartPoint = rect.getPoint().moveBy(rect.getWidth() / 2, rect.getHeight() / 2);
    Point nativeEndPoint = nativeStartPoint.moveBy(xOffset, yOffset);
    Point[] movement =
        MobileDriverUtils.getFlickCoordinates(
            (MobileDriverAdapter) driverAdapter, nativeStartPoint, nativeEndPoint);
    driverAdapter.setPageContextToNative();
    Point start = movement[0];
    Point end = movement[1];
    // simulate flick using touch control
    // Below code is the Selenium 4.x version of this code.
    //    PointerInput finger = new PointerInput(Kind.TOUCH, "finger");
    //    Sequence flickSequence = new Sequence(finger, 0);
    //    flickSequence.addAction(
    //        finger.createPointerMove(
    //            java.time.Duration.ZERO, Origin.viewport(), start.getX(), start.getY()));
    //    flickSequence.addAction(finger.createPointerDown(MouseButton.LEFT.asArg()));
    //    flickSequence.addAction(new Pause(finger, DEFAULT_FLICK_ACTION_WAIT_MILLISECONDS));
    //    flickSequence.addAction(
    //        finger.createPointerMove(
    //            DEFAULT_FLICK_ACTION_MOVEMENT_DURATION, Origin.viewport(), end.getX(),
    // end.getY()));
    //    flickSequence.addAction(finger.createPointerUp(MouseButton.LEFT.asArg()));
    //    appiumDriver.perform(List.of(flickSequence));
    new TouchAction(appiumDriver)
        .press(PointOption.point(start.getX(), start.getY()))
        .waitAction(WaitOptions.waitOptions(DEFAULT_FLICK_ACTION_WAIT_MILLISECONDS))
        .moveTo(PointOption.point(end.getX(), end.getY()))
        .release()
        .perform();
  }

  @Override
  protected Element wrapElement(WebElement element) {
    return new MobileElementAdapter(element, driverAdapter);
  }
}
