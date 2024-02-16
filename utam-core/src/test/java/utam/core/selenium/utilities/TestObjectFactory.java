package utam.core.selenium.utilities;

import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.refEq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;
import static utam.core.selenium.element.ShadowRootWebElement.GET_SHADOW_ROOT_QUERY_SELECTOR;
import static utam.core.selenium.element.ShadowRootWebElement.GET_SHADOW_ROOT_QUERY_SELECTOR_ALL;
import static utam.core.selenium.element.ShadowRootWebElement.SHADOW_ROOT_DETECTION_SCRIPT_FRAGMENT;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.WrapsDriver;
import utam.core.selenium.utilities.WebDriverSimulator.WebElementInfo;

/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
public class TestObjectFactory extends WebDriverSimulatorObjectFactory {

  public TestObjectFactory() {
    super();
  }

  @Override
  public WebDriver createDriver(Map<String, WebElementInfo> knownElements) {
    WebDriver driver =
        mock(
            WebDriver.class,
            withSettings()
                .name("driver")
                .extraInterfaces(JavascriptExecutor.class, SearchContext.class));

    for (String elementName : knownElements.keySet()) {
      WebElementInfo elementInfo = knownElements.get(elementName);
      when(((WrapsDriver) elementInfo.getElement()).getWrappedDriver()).thenReturn(driver);

      // If the parent element is null, the element is to be found from the
      // root of the page.
      if (elementInfo.getParentElementName() == null) {
        when(driver.findElements(elementInfo.getCssSelector()))
            .thenReturn(Collections.singletonList(elementInfo.getElement()));
        when(driver.findElement(elementInfo.getCssSelector())).thenReturn(elementInfo.getElement());
      }

      // If this is set for more than one element then only the last element would be mocked.
      if (elementInfo.isFocused()) {
        WebDriver.TargetLocator targetLocator = mock(WebDriver.TargetLocator.class);
        when(driver.switchTo()).thenReturn(targetLocator);
        when(targetLocator.activeElement()).thenReturn(elementInfo.getElement());
      }

      // Get a list all of the child element info objects that are children of this element
      List<WebElementInfo> childElementInfos =
          knownElements.values().stream()
              .filter((info) -> Objects.equals(info.getParentElementName(), elementName))
              .collect(Collectors.toList());

      // Next, set this element to be recognized as a shadow root if any of the children
      // are in its shadow DOM
      boolean isShadowHost = childElementInfos.stream().anyMatch(WebElementInfo::isInShadowDOM);
      when(((JavascriptExecutor) driver)
              .executeScript(
                  contains(SHADOW_ROOT_DETECTION_SCRIPT_FRAGMENT), refEq(elementInfo.getElement())))
          .thenReturn(isShadowHost);

      // Now get the set of all unique selectors for these children
      Set<String> childSelectors =
          childElementInfos.stream().map(WebElementInfo::getSelector).collect(Collectors.toSet());

      // Finally, for each selector, generate the list of WebElements that are
      // children of this element and match the selector, and return them when
      // the appropriate WebDriver API is called
      for (String selector : childSelectors) {
        List<WebElement> childElements =
            childElementInfos.stream()
                .filter(
                    (info) -> Objects.equals(info.getSelector(), selector) && !info.isInShadowDOM())
                .map(WebElementInfo::getElement)
                .collect(Collectors.toList());
        when(elementInfo.getElement().findElements(By.cssSelector(selector)))
            .thenReturn(childElements);
        if (!childElements.isEmpty()) {
          when(elementInfo.getElement().findElement(By.cssSelector(selector)))
              .thenReturn(childElements.get(0));
        }

        List<WebElement> shadowChildElements =
            childElementInfos.stream()
                .filter(
                    (info) -> Objects.equals(info.getSelector(), selector) && info.isInShadowDOM())
                .map(WebElementInfo::getElement)
                .collect(Collectors.toList());
        String finderScriptAll =
            String.format(GET_SHADOW_ROOT_QUERY_SELECTOR_ALL, selector.replace("'", "\\'"));
        when(((JavascriptExecutor) driver)
                .executeScript(contains(finderScriptAll), refEq(elementInfo.getElement())))
            .thenReturn(shadowChildElements);
        if (!shadowChildElements.isEmpty()) {
          String finderScript =
              String.format(GET_SHADOW_ROOT_QUERY_SELECTOR, selector.replace("'", "\\'"));
          when(((JavascriptExecutor) driver)
                  .executeScript(contains(finderScript), refEq(elementInfo.getElement())))
              .thenReturn(shadowChildElements.get(0));
        }
      }
    }
    return driver;
  }

  @Override
  public WebElement createElement(String elementName) {
    return mock(
        WebElement.class,
        withSettings().name(elementName).extraInterfaces(WrapsDriver.class, SearchContext.class));
  }

  @Override
  public void setElementAttribute(WebElement element, String attributeName, String attributeValue) {
    when(element.getAttribute(attributeName)).thenReturn(attributeValue);
  }

  @Override
  public void setElementText(WebElement element, String text) {
    when(element.getText()).thenReturn(text);
  }

  @Override
  public void setElementVisibility(WebElement element, boolean isVisible) {
    when(element.isDisplayed()).thenReturn(isVisible);
  }
}
