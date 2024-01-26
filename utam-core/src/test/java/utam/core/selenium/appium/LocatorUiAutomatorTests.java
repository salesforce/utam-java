/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

import io.appium.java_client.MobileBy;
import org.openqa.selenium.By;
import org.testng.annotations.Test;
import utam.core.element.Locator;
import utam.core.selenium.appium.LocatorUIAutomator.Method;
import utam.core.selenium.element.LocatorBy;

/**
 * Android UIAutomator selector based locator
 *
 * @author Qingchun Ren
 * @since 228
 */
public class LocatorUiAutomatorTests {

  @Test
  public void testCreation() {
    String selector = "description('test')";
    Locator<By> locator = new LocatorUIAutomator(selector);
    By by = MobileBy.AndroidUIAutomator(selector);
    assertThat(locator.getValue(), is(equalTo(by)));
    assertThat(locator.getStringValue(), is(equalTo(selector)));
    assertThat(locator.setParameters().getValue(), is(equalTo(by)));
  }

  @Test
  public void testCreationDoesNotThrow() {
    String selector1 = "unsupported";
    new LocatorUIAutomator(selector1);
  }

  @Test
  public void testValidateUIAutomatorSelectorIncludingPrefix() {
    LocatorBy.byUiAutomator(
        "new UiSelector().\"**/XCUIElementTypeStaticText[`label == 'something'`]\"");
  }

  @Test
  public void testSupportedMethods() {
    for (Method method : Method.values()) {
      Locator locator = new LocatorUIAutomator(method.getValue() + "()");
      assertThat(locator.getStringValue(), is(equalTo(method.getValue() + "()")));
    }
  }

  @Test
  public void testSupportedMethodsWithPrefix() {
    for (Method method : Method.values()) {
      Locator locator = new LocatorUIAutomator("new UiSelector()." + method.getValue() + "()");
      assertThat(
          locator.getStringValue(), is(equalTo("new UiSelector()." + method.getValue() + "()")));
    }
  }

  @Test
  public void testUiScrollable() {
    String selector =
        "new UiScrollable(new"
            + " UiSelector().scrollable(true)).scrollIntoView(resourceId(\"com.salesforce.chatter:id/app_launcher_menu_item\"))";
    Locator<By> locator = new LocatorUIAutomator(selector);
    By by = MobileBy.AndroidUIAutomator(selector);
    assertThat(locator.getValue(), is(equalTo(by)));
    assertThat(locator.getStringValue(), is(equalTo(selector)));
  }

  @Test
  public void testUiScrollableWithInnerUiSelector() {
    String selector =
        "new UiScrollable(new UiSelector().scrollable(true)).scrollIntoView(new"
            + " UiSelector().resourceId(\"com.salesforce.chatter:id/app_launcher_menu_item\"))";
    Locator<By> locator = new LocatorUIAutomator(selector);
    By by = MobileBy.AndroidUIAutomator(selector);
    assertThat(locator.getValue(), is(equalTo(by)));
    assertThat(locator.getStringValue(), is(equalTo(selector)));
  }

  @Test
  public void testUiScrollableWithResourceId() {
    String selector =
        "new UiScrollable(new"
            + " UiSelector().resourceId(\"com.salesforce.fieldservice.app:id/work_view\")).scrollIntoView(new"
            + " UiSelector().resourceId(\"com.salesforce.chatter:id/app_launcher_menu_item\"))";
    Locator<By> locator = new LocatorUIAutomator(selector);
    By by = MobileBy.AndroidUIAutomator(selector);
    assertThat(locator.getValue(), is(equalTo(by)));
    assertThat(locator.getStringValue(), is(equalTo(selector)));
  }
}
