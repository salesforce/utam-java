/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.refEq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.element.ElementAdapter.BLUR_VIA_JAVASCRIPT;
import static utam.core.selenium.element.ElementAdapter.CLICK_VIA_JAVASCRIPT;
import static utam.core.selenium.element.ElementAdapter.ERR_DRAG_AND_DROP_OPTIONS;
import static utam.core.selenium.element.ElementAdapter.FOCUS_VIA_JAVASCRIPT;
import static utam.core.selenium.element.ElementAdapter.NULL_ELEMENT;
import static utam.core.selenium.element.ElementAdapter.SCROLL_CENTER_VIA_JAVASCRIPT;
import static utam.core.selenium.element.ElementAdapter.SCROLL_TOP_VIA_JAVASCRIPT;
import static utam.core.selenium.element.LocatorBy.byCss;
import static utam.core.selenium.element.ShadowRootWebElement.*;

import java.awt.Point;
import java.util.Collections;
import org.openqa.selenium.By;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Element;
import utam.core.element.Element.DragAndDropOptions;
import utam.core.element.Element.GestureDirection;
import utam.core.element.Element.ScrollOptions;
import utam.core.element.FindContext.Type;
import utam.core.framework.consumer.UtamError;

/**
 * element tests
 *
 * @author jim.evans, elizaveta.ivanova
 */
public class ElementAdapterTests {

  @Test
  public void testFind() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().findElement(By.cssSelector("css")))
        .thenReturn(mock(WebElement.class));
    assertThat(
        mock.getElementAdapter().findElement(byCss("css"), Type.EXISTING),
        is(notNullValue()));
    assertThat(
        mock.getElementAdapter().findElement(byCss("css1"), Type.NULLABLE).isNull(),
        is(true));
    when(mock.getWebElementMock().findElements(By.cssSelector("css")))
        .thenReturn(Collections.singletonList(mock(WebElement.class)));
    assertThat(
        mock.getElementAdapter().findElements(byCss("css"), Type.EXISTING),
        is(not(empty())));
    assertThat(
        mock.getElementAdapter().findElements(byCss("css1"), Type.NULLABLE),
        is(empty()));
  }

  @Test
  public void testFindInShadow() {
    MockUtilities mock = new MockUtilities();
    mock.setShadowMock(mock.getWebElementMock(), "css");
    assertThat(
        mock.getElementAdapter().findElement(byCss("css"), Type.EXISTING_IN_SHADOW).isNull(),
        is(false));
    assertThat(
        mock.getElementAdapter().findElement(byCss("css1"), Type.NULLABLE_IN_SHADOW).isNull(),
        is(true));
    assertThat(
        mock.getElementAdapter().findElements(byCss("css"), Type.EXISTING_IN_SHADOW),
        is(not(empty())));
    assertThat(
        mock.getElementAdapter().findElements(byCss("css1"), Type.NULLABLE_IN_SHADOW),
        is(empty()));
  }

  @Test
  public void testIsNull() {
    assertThat(ElementAdapter.NULL_ELEMENT.isNull(), is(true));
    assertThat(new ElementAdapter(mock(WebElement.class), mock(WebDriver.class)).isNull(), is(false));
  }

  @Test
  public void testClick() {
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().click();
    verify(mock.getWebElementMock(), times(1)).click();
  }

  @Test
  public void testJavascriptClick() {
    MockUtilities mock = new MockUtilities.MockDriver();
    mock.getElementAdapter().deprecatedClick();
    verify(mock.getDriverAdapter(), times(1))
        .executeScript(CLICK_VIA_JAVASCRIPT, mock.getWebElementMock());
  }

  @Test
  public void testClear() {
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().clear();
    verify(mock.getWebElementMock(), times(1)).clear();
  }

  @Test
  public void testSetText() {
    String text = "text";
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().setText(text);
    verify(mock.getWebElementMock(), times(1)).sendKeys(text);
  }

  @Test
  public void testIsExisting() {
    assertThat(NULL_ELEMENT.isExisting(), is(false));
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().isDisplayed()).thenReturn(true);
    assertThat(mock.getElementAdapter().isExisting(), is(true));
    when(mock.getWebElementMock().isDisplayed()).thenThrow(StaleElementReferenceException.class);
    assertThat(mock.getElementAdapter().isExisting(), is(false));
  }

  @Test
  public void testIsExistingNotFound() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().isDisplayed()).thenThrow(org.openqa.selenium.NoSuchElementException.class);
    assertThat(mock.getElementAdapter().isExisting(), is(false));
  }

  @Test
  public void testIsDisplayed() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().isDisplayed()).thenReturn(true);
    assertThat(mock.getElementAdapter().isDisplayed(), is(true));
    verify(mock.getWebElementMock(), times(1)).isDisplayed();
  }

  @Test
  public void testGetAttribute() {
    String attr = "nameAndValue";
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().getAttribute(attr)).thenReturn(attr);
    assertThat(mock.getElementAdapter().getAttribute(attr), is(equalTo(attr)));
    verify(mock.getWebElementMock(), times(1)).getAttribute(attr);
  }

  @Test
  public void testGetText() {
    String text = "text";
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().getText()).thenReturn(text);
    assertThat(mock.getElementAdapter().getText(), is(equalTo(text)));
  }

  @Test
  public void testMoveTo() {
    MockUtilities mock = new MockUtilities.MockDriver();
    mock.getElementAdapter().moveTo();
  }

  @Test
  public void testIsEnabled() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().isEnabled()).thenReturn(true);
    assertThat(mock.getElementAdapter().isEnabled(), is(true));
    verify(mock.getWebElementMock(), times(1)).isEnabled();
  }

  @Test
  public void testHasFocus() {
    MockUtilities mock = new MockUtilities();
    Element element = mock.getElementAdapter();
    WebDriver.TargetLocator targetLocator = mock(WebDriver.TargetLocator.class);
    when(mock.getWebDriverMock().switchTo()).thenReturn(targetLocator);
    when(targetLocator.activeElement()).thenReturn(mock(WebElement.class));
    assertThat("element has no focus", element.hasFocus(), is(false));
    when(targetLocator.activeElement()).thenReturn(mock.getWebElementMock());
    assertThat("element has focus", element.hasFocus(), is(true));
  }

  @Test
  public void testFocus() {
    MockUtilities mock = new MockUtilities.MockDriver();
    mock.getElementAdapter().focus();
    verify(mock.getDriverAdapter(), times(1))
        .executeScript(FOCUS_VIA_JAVASCRIPT, mock.getWebElementMock());
  }

  @Test
  public void testScrollToTop() {
    MockUtilities mock = new MockUtilities.MockDriver();
    when(mock.getElementAdapter().isDisplayed()).thenReturn(false).thenReturn(false)
        .thenReturn(false).thenReturn(true);
    mock.getElementAdapter().scrollIntoView(ScrollOptions.TOP);
    verify(mock.getDriverAdapter(), times(2))
        .executeScript(SCROLL_TOP_VIA_JAVASCRIPT, mock.getWebElementMock());
  }

  @Test
  public void testScrollToCenter() {
    MockUtilities mock = new MockUtilities.MockDriver();
    mock.getElementAdapter().scrollIntoView(ScrollOptions.CENTER);
    verify(mock.getDriverAdapter(), times(1))
        .executeScript(SCROLL_CENTER_VIA_JAVASCRIPT, mock.getWebElementMock());
  }

  @Test
  public void testBlur() {
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().blur();
    verify(mock.getExecutorMock(), times(1))
        .executeScript(BLUR_VIA_JAVASCRIPT, mock.getWebElementMock());
  }

  @Test
  public void testContainsElement() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().findElements(By.cssSelector("css")))
        .thenReturn(Collections.singletonList(mock.getWebElementMock()));
    assertThat(mock.getElementAdapter().containsElements(byCss("css"), false), is(equalTo(1)));
    assertThat(mock.getElementAdapter().containsElements(byCss("css1"), false), is(equalTo(0)));
  }

  @Test
  public void testContainsElementInShadow() {
    MockUtilities mock = new MockUtilities();
    when(mock.getExecutorMock()
        .executeScript(contains(String.format(GET_SHADOW_ROOT_QUERY_SELECTOR_ALL, "css")),
            refEq(mock.getWebElementMock())))
        .thenReturn(Collections.singletonList(mock.getWebElementMock()));
    assertThat(mock.getElementAdapter().containsElements(byCss("css"), true), is(equalTo(1)));
  }

  @Test
  public void testMobileActionsThrow() {
    MockUtilities mock = new MockUtilities();
    assertThrows(() -> mock.getElementAdapter().flick(0, 0));
    assertThrows(() -> mock.getElementAdapter().flickItems(GestureDirection.DOWN));
  }

  @Test
  public void testDragAndDropWithTargetElement() {
    MockUtilities mock = new MockUtilities.MockDriver();
    DragAndDropOptions options = new DragAndDropOptions() {
      @Override
      public Element getTargetElement() {
        return mock.getElementAdapter();
      }
    };
    mock.getElementAdapter().dragAndDrop(options);
  }

  @Test
  public void testDragAndDropWithOffset() {
    MockUtilities mock = new MockUtilities.MockDriver();
    DragAndDropOptions options = new DragAndDropOptions() {
      @Override
      public Point getOffset() {
        return new Point(0,0);
      }
    };
    mock.getElementAdapter().dragAndDrop(options);
  }

  @Test
  public void testDragAndDropWithoutTargetThrows() {
    MockUtilities mock = new MockUtilities.MockDriver();
    UtamError e = expectThrows(UtamError.class, () -> mock.getElementAdapter().dragAndDrop(new DragAndDropOptions() {}));
    assertThat(e.getMessage(), is(equalTo(ERR_DRAG_AND_DROP_OPTIONS)));
  }
}
