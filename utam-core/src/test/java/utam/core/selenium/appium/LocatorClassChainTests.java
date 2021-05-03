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
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.appium.LocatorClassChain.ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_OPERATOR;
import static utam.core.selenium.appium.LocatorClassChain.ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_QUOTE;

import io.appium.java_client.MobileBy;
import java.util.Map.Entry;
import org.openqa.selenium.By;
import org.testng.annotations.Test;
import utam.core.element.Locator;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.LocatorBy;

/**
 * iOS Class Chain selector based locator tests
 *
 * @author Qingchun Ren
 * @since 228
 */
public class LocatorClassChainTests {

  /**
   * An ElementLocatorClassChain object should be able to be created
   */
  @Test
  public void testElementLocatorClassChainCreation() {
    String selector = "selector";
    LocatorBy locator = new LocatorClassChain(selector);
    assertThat(locator.getValue(), is(equalTo(MobileBy.iOSClassChain(selector))));
    assertThat(locator.getStringValue(), is(equalTo(selector)));
    assertThat(locator.getCopy(selector), is(equalTo(locator)));
    assertThat(locator.setParameters(0, "parameters").getValue(), is(sameInstance(locator)));
  }

  /**
   * The applyParameters method should modify the CSS selector string
   */
  @Test
  public void testApplyParameters() {
    String selector = "selector::nth-of-type(%d)";
    Locator locator = new LocatorClassChain(selector);
    assertThat(locator.getValue(), is(equalTo(MobileBy.iOSClassChain(selector))));
    assertThat(locator.getStringValue(), is(equalTo(selector)));
    String filteredSelector = String.format(selector, 1);
    Entry<Integer, Locator<By>> withParameters = locator.setParameters(0, 1);
    assertThat(withParameters.getKey(), is(equalTo(1)));
    locator = withParameters.getValue();
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
    String locator = "**/XCUIElementTypeStaticText[`label CONTAINS 'something' AND text == 'fake value'`]";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testGetClassChainSelectorWithMultipleLevels() {
    String locator = "**/XCUIElementTypeCell[`name == 'cell.appTitle'`]/XCUIElementTypeStaticText[-1]";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testValidateClassChainSelectorInCorrectQuote() {
    UtamError e = expectThrows(UtamError.class,
        () -> LocatorBy.byClassChain("**/XCUIElementTypeStaticText[\"label == 'something'\"]"));
    assertThat(e.getMessage(), is(equalTo(ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_QUOTE)));
  }

  @Test
  public void testValidateClassChainSelectorNoAttribute() {
    String locator = "**/XCUIElementTypeStaticText";
    Locator locatorValue = LocatorBy.byClassChain(locator);
    assertThat(locatorValue.getValue(), is(equalTo(MobileBy.iOSClassChain(locator))));
    assertThat(locatorValue.getStringValue(), is(equalTo(locator)));
  }

  @Test
  public void testValidateClassChainSelectorUnsupportOperatorThrows() {
    UtamError e = expectThrows(UtamError.class,
        () -> LocatorBy
            .byClassChain("**/XCUIElementTypeStaticText[$label STARTWITH 'something'$]"));
    assertThat(e.getMessage(), is(equalTo(ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_OPERATOR)));
  }

  @Test
  public void testValidateClassChainSelectorNoSpaceThrows() {
    UtamError e = expectThrows(UtamError.class,
        () -> LocatorBy.byClassChain("**/XCUIElementTypeStaticText[`label=='something'`]"));
    assertThat(e.getMessage(), is(equalTo(ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_OPERATOR)));
  }

  @Test
  public void testGetClassChainSelectorIncorrectAtSublevelThrows() {
    UtamError e = expectThrows(UtamError.class, () -> LocatorBy.byClassChain(
        "**/XCUIElementTypeCell[`name == 'cell.appTitle'`]/XCUIElementTypeStaticText[`name EQUALS 'something'`]"));
    assertThat(e.getMessage(), is(equalTo(ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_OPERATOR)));
  }

}
