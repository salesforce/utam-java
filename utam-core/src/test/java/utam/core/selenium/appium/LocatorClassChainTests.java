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
import static org.hamcrest.Matchers.sameInstance;

import io.appium.java_client.MobileBy;
import org.testng.annotations.Test;
import utam.core.element.Locator;
import utam.core.selenium.element.LocatorBy;

/**
 * iOS Class Chain selector based locator tests
 *
 * @author Qingchun Ren
 * @since 228
 */
public class LocatorClassChainTests {

  /** An ElementLocatorClassChain object should be able to be created */
  @Test
  public void testElementLocatorClassChainCreation() {
    String selector = "selector";
    LocatorBy locator = new LocatorClassChain(selector);
    assertThat(locator.getValue(), is(equalTo(MobileBy.iOSClassChain(selector))));
    assertThat(locator.getStringValue(), is(equalTo(selector)));
    assertThat(locator.getCopy(selector), is(equalTo(locator)));
    assertThat(locator.setParameters("parameters"), is(sameInstance(locator)));
  }

  /** The applyParameters method should modify the CSS selector string */
  @Test
  public void testApplyParameters() {
    String selector = "selector::nth-of-type(%d)";
    Locator locator = new LocatorClassChain(selector);
    assertThat(locator.getValue(), is(equalTo(MobileBy.iOSClassChain(selector))));
    assertThat(locator.getStringValue(), is(equalTo(selector)));
    String filteredSelector = String.format(selector, 1);
    locator = locator.setParameters(1);
    assertThat(locator.getValue(), is(equalTo(MobileBy.iOSClassChain(filteredSelector))));
    assertThat(locator.getStringValue(), is(equalTo(filteredSelector)));
  }

  @Test
  public void testGetClassChainSelectorQuotedByDollarSign() {
    String locator = "**/XCUIElementTypeStaticText[$label == 'something'$]";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testGetClassChainSelectorWithoutAttribute() {
    String locator = "**/XCUIElementTypeStaticText";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testGetClassChainSelectorWithPositiveIndex() {
    String locator = "**/XCUIElementTypeStaticText[1]";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testGetClassChainSelectorWithNegtiveIndex() {
    String locator = "**/XCUIElementTypeStaticText[-1]";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testGetClassChainSelectorWithIntegerArg() {
    String locator = "**/XCUIElementTypeStaticText[%d]";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testGetClassChainSelectorWithBeginsWith() {
    String locator = "**/XCUIElementTypeStaticText[`label BEGINSWITH 'something'`]";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testGetClassChainSelectorWithEndsWith() {
    String locator = "**/XCUIElementTypeStaticText[`label ENDSWITH 'something'`]";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testGetClassChainSelectorWithContains() {
    String locator = "**/XCUIElementTypeStaticText[`label CONTAINS 'something'`]";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testGetClassChainSelectorWithOr() {
    String locator = "**/XCUIElementTypeStaticText[`label OR 'something'`]";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testGetClassChainSelectorWithAnd() {
    String locator = "**/XCUIElementTypeStaticText[`label AND 'something'`]";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testGetClassChainSelectorWithContainsAnd() {
    String locator =
        "**/XCUIElementTypeStaticText[`label CONTAINS 'something' AND text == 'fake value'`]";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testGetClassChainSelectorWithMultipleLevels() {
    String locator =
        "**/XCUIElementTypeCell[`name == 'cell.appTitle'`]/XCUIElementTypeStaticText[-1]";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testValidateClassChainSelectorNoAttribute() {
    String locator = "**/XCUIElementTypeStaticText";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testValidateClassChainSelectorWithSlashInAttribut() {
    String locator =
        "**/XCUIElementTypeStaticText[`text =="
            + " 'https://q3lex.lightning.force.com/lightning/r/Account/0019A00000K9wzfQAB/view'`]";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }
}
