/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import utam.core.appium.element.Mobile;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.selenium.context.SeleniumContextProvider;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static utam.core.selenium.element.LocatorImpl.LOCATOR_CHAIN_SEPARATOR;
import static utam.core.selenium.element.LocatorImplTests.getParentLocator;
import static utam.core.selenium.element.LocatorImplTestsUtilities.*;
import static utam.core.selenium.element.LocatorNodeImpl.NOT_FOUND_ERR_FORMAT;
import static utam.core.selenium.element.LocatorNodeImpl.NOT_FOUND_ERR_FORMAT_SHADOW_ROOT;
import static utam.core.selenium.element.LocatorUtilities.EMPTY_FILTER;

/**
 * Provides tests for the ElementLocatorCss class
 *
 * @author james.evans
 */
public class LocatorNodeImplTestsCss {

  static LocatorNodeImpl getParentLocatorNode() {
    return new FakeLocator(EMPTY_FILTER, ShadowBoundary.NONE);
  }

  /** The hasNext method should return false for locators that do not have children */
  @Test
  public void testGetNext() {
    assertThat(getParentLocator().getRoot().getNext(), is(nullValue()));
  }

  /** The getTransformer method should return a valid value */
  @Test
  public void testGetTransformer() {
    assertThat(getParentLocatorNode().getScopeTransformer(), is(equalTo(ShadowBoundary.NONE)));
  }

  static LocatorNodeImpl getChildLocatorNode() {
    return new FakeChildLocator(EMPTY_FILTER, ShadowBoundary.NONE);
  }

  /** The scope method should return a valid scoped locator value */
  @Test
  public void testSetNext() {
    LocatorNodeImpl parent = getParentLocatorNode();
    LocatorNodeImpl child = getChildLocatorNode();
    parent.setNext(child);
    assertThat(parent.getNext(), is(sameInstance(child)));
    assertThat(
        new LocatorImpl(parent).getSelectorString(),
        is(
            equalTo(
                String.format(
                    "%s%s%s", SELECTOR_STRING, LOCATOR_CHAIN_SEPARATOR, CHILD_SELECTOR_STRING))));
  }

  /** The getErrorPrefix method should return a valid value for a null locator */
  @Test
  public void testGetNotFoundErrorWithNullLocator() {
    LocatorNodeImpl locator = new LocatorNodeImpl.Css("css");
    assertThat(
        locator.getNotFoundError(null),
        is(equalTo(String.format(NOT_FOUND_ERR_FORMAT, locator.getSelectorString(), "browser"))));
  }

  /** The getErrorPrefix method should return a valid value for a valid locator */
  @Test
  public void testGetNotFoundErrorWithValidLocator() {
    LocatorNodeImpl scope = getParentLocatorNode();
    LocatorNodeImpl locator = new LocatorNodeImpl.Css("css");
    assertThat(
        locator.getNotFoundError(scope),
        is(
            equalTo(
                String.format(
                    NOT_FOUND_ERR_FORMAT,
                    locator.getSelectorString(),
                    scope.getSelectorString()))));
  }

  /** The getErrorPrefix method should return a valid value for a valid shadow root locator */
  @Test
  public void testGetNotFoundErrorWithValidShadowRootLocator() {
    LocatorNodeImpl locator = new FakeLocator(EMPTY_FILTER, ShadowBoundary.EXPAND_SHADOW_ROOT);
    assertThat(locator.getNotFoundError(null),
        is(
            equalTo(
                String.format(
                    NOT_FOUND_ERR_FORMAT_SHADOW_ROOT, locator.getSelectorString(), "browser"))));
  }

  /** The getLogString method should return a valid value */
  @Test
  public void testGetLogString() {
    assertThat(
        getParentLocatorNode().getSelectorString(),
        is(equalTo(LocatorImplTestsUtilities.SELECTOR_STRING)));
  }

  /** An ElementLocatorCss object should be able to be created */
  @Test
  public void testElementLocatorCssCreation() {
    String selector = ".fakeSelector";
    LocatorNodeImpl locator = new LocatorNodeImpl.Css(selector);
    assertThat(locator.by(), is(equalTo(By.cssSelector(selector))));
    assertThat(locator.getSelectorString(), is(equalTo(selector)));
  }

  /** The applyParameters method should modify the CSS selector string */
  @Test
  public void testApplyParameters() {
    String selector = ".fakeSelector::nth-of-type(%d)";
    LocatorNodeImpl filteredLocator = new LocatorNodeImpl.Css(selector);
    assertThat(filteredLocator.by(), is(equalTo(By.cssSelector(selector))));
    assertThat(filteredLocator.getSelectorString(), is(equalTo(selector)));

    String filteredSelector = String.format(selector, 1);
    filteredLocator.setParameters(new LocatorParameters(1));
    assertThat(filteredLocator.by(), is(equalTo(By.cssSelector(filteredSelector))));
    assertThat(filteredLocator.getSelectorString(), is(equalTo(filteredSelector)));
  }

  /**
   * The getSelfCopy method should return a new copy of the ElementSelectorCss object, with the
   * original, unmodified selector, even if parameters have been applied
   */
  @Test
  public void testGetSelfCopy() {
    String selector = ".fakeSelector::nth-of-type(%d)";
    LocatorNodeImpl filteredLocator = new LocatorNodeImpl.Css(selector);
    filteredLocator.setParameters(new LocatorParameters(1));
    assertThat(filteredLocator.by(), is(equalTo(By.cssSelector(".fakeSelector::nth-of-type(1)"))));
    assertThat(filteredLocator.getSelectorString(), is(equalTo(".fakeSelector::nth-of-type(1)")));
    filteredLocator.setParameters(new LocatorParameters(2));
    assertThat(filteredLocator.getSelectorString(), is(equalTo(".fakeSelector::nth-of-type(2)")));
  }

  /** The getLastFilter method should return a valid AbstractLocatorFilter value */
  @Test
  public void testGetLastFilter() {
    assertThat(getParentLocatorNode().getFilter(), is(equalTo(EMPTY_FILTER)));
  }

  @Test
  public void testEqualsWithAnotherType() {
    LocatorNode self = new LocatorNodeImpl.Css("css");
    assertThat(self.equals(null), is(false));
    assertThat(self.equals(new Object()), is(false));
  }

  @Test
  public void testFindElement() {
    WebDriver driver = getMockDriverForCss();
    SeleniumContextProvider provider = new SeleniumContextProvider(driver);
    LocatorNodeImpl locatorNode = getParentLocatorNode();
    LocatorNodeImpl childLocatorNode = getChildLocatorNode();
    List<WebElement> element =
        locatorNode.findElements(
            driver, null, provider.getWebDriverUtils(), LocatorUtilities.Find.FILTERED_LIST);
    assertThat(element.get(0).getText(), is(equalTo(getParentElementText())));
    List<WebElement> childElement =
        childLocatorNode.findElements(
            element.get(0),
            locatorNode,
            provider.getWebDriverUtils(),
            LocatorUtilities.Find.FILTERED_LIST);
    assertThat(childElement.get(0).getText(), is(equalTo(getChildElementText())));
    WebElement grandchildElement =
            new FakeGrandchildLocator(EMPTY_FILTER, ShadowBoundary.NONE)
            .findElements(
                childElement.get(0),
                childLocatorNode,
                provider.getWebDriverUtils(),
                LocatorUtilities.Find.FILTERED_LIST)
            .get(0);
    assertThat(grandchildElement.getText(), is(equalTo(getGrandChildElementText())));
  }

  /** The findElements method should return a valid list of Selenium WebElement objects */
  @Test
  public void testFindElements() {
    WebDriver driver = LocatorImplTestsUtilities.getMockDriverForCss();
    SeleniumContextProvider provider = new SeleniumContextProvider(driver);
    List<WebElement> elements =
        getParentLocatorNode()
            .findElements(driver, null, provider.getWebDriverUtils(), LocatorUtilities.Find.FILTERED_LIST);
    List<String> elementTextList =
        elements.stream().map(WebElement::getText).collect(Collectors.toList());
    assertThat(elementTextList, is(equalTo(Collections.singletonList(getParentElementText()))));
  }

  @Test
  public void testSelectorImplEquals() {
    Selector selector = Web.byCss("css");
    assertThat(selector.equals(selector), is(true));
    assertThat(selector.equals(null), is(false));
    assertThat(selector.equals(Mobile.byClassChain("css")), is(false));
    assertThat(selector.equals(new Object()), is(false));
  }
}
