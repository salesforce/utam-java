/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.element.LocatorNodeImpl;
import utam.core.selenium.element.LocatorUtilities;

import java.util.Collections;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Provides tests for the ElementLocatorWebElement class
 *
 * @author james.evans
 */
public class LocatorNodeImplTestsWeb {

  private static final WebElement element = mock(WebElement.class);

  private static LocatorNodeImpl.Web getLocator() {
    return new LocatorNodeImpl.Web(() -> element);
  }

  /** The applyParameters method should throw the proper exception */
  @Test
  public void testApplyParametersThrows() {
    LocatorNodeImpl locator = getLocator();
    locator.setParameters(null);
  }

  /** The findElements method should return a list containing only the locator's element */
  @Test
  public void testFindElements() {
    WebDriver mockDriver = mock(WebDriver.class);
    when(mockDriver.findElements(By.cssSelector(".fakeSelector")))
        .thenReturn(Collections.singletonList(element));
    SeleniumContext context = mock(SeleniumContext.class);
    LocatorNodeImpl locatorNode = getLocator();
    List<WebElement> found = locatorNode.findElements(element, null, context.getWebDriverUtils(), LocatorUtilities.Find.FILTERED_LIST);
    assertThat(found, is(equalTo(Collections.singletonList(element))));
    assertThat(locatorNode.applyFilter(found, LocatorUtilities.Find.FILTERED_LIST), is(equalTo(Collections.singletonList(element))));
  }

  /** The getSelfCopy method should return the locator */
  @Test
  public void testGetSelfCopy() {
    LocatorNodeImpl locator = getLocator();
    assertThat(locator.getCopy(), is(not(sameInstance(locator))));
  }

  /** The getString method should return the proper value */
  @Test
  public void testGetString() {
    LocatorNodeImpl locator = getLocator();
    assertThat(
        locator.getSelectorString(), containsString(LocatorNodeImpl.Web.MOCK_SELECTOR.getValue()));
  }
}
