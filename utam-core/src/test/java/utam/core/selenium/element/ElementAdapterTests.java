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
import static utam.core.selenium.element.ElementAdapter.BLUR_VIA_JAVASCRIPT;
import static utam.core.selenium.element.ElementAdapter.CLICK_VIA_JAVASCRIPT;
import static utam.core.selenium.element.ElementAdapter.FOCUS_VIA_JAVASCRIPT;
import static utam.core.selenium.element.ElementAdapter.SCROLL_CENTER_VIA_JAVASCRIPT;
import static utam.core.selenium.element.ElementAdapter.SCROLL_TOP_VIA_JAVASCRIPT;
import static utam.core.selenium.element.LocatorBy.byCss;
import static utam.core.selenium.element.ShadowRootWebElement.SHADOW_ROOT_DETECTION_SCRIPT_FRAGMENT;

import java.util.Collections;
import org.openqa.selenium.By;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Element;
import utam.core.element.Element.GestureDirection;
import utam.core.element.Element.ScrollOptions;
import utam.core.element.FindContext.Type;

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
    assertThat(new ElementAdapter(null).isNull(), is(true));
    assertThat(new ElementAdapter(mock(WebElement.class)).isNull(), is(false));
  }

  @Test
  public void testClick() {
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().click();
    verify(mock.getWebElementMock(), times(1)).click();
  }

  @Test
  public void testJavascriptClick() {
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().deprecatedClick(mock.getDriverAdapterMock());
    verify(mock.getDriverAdapterMock(), times(1))
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
    assertThat(new ElementAdapter(null).isExisting(), is(false));
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().isDisplayed()).thenReturn(true);
    assertThat(mock.getElementAdapter().isExisting(), is(true));
    when(mock.getWebElementMock().isDisplayed()).thenThrow(StaleElementReferenceException.class);
    assertThat(mock.getElementAdapter().isExisting(), is(false));
    // todo - fix
    /*
    when(mock.getWebElementMock().isDisplayed())
        .thenThrow(org.openqa.selenium.NoSuchElementException.class);
    assertThat(mock.getElementAdapter().isExisting(), is(false));
     */
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
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().moveTo(mock.getDriverAdapterMock());
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
    assertThat("element has no focus", element.hasFocus(mock.getDriverAdapter()), is(false));
    when(targetLocator.activeElement()).thenReturn(mock.getWebElementMock());
    assertThat("element has focus", element.hasFocus(mock.getDriverAdapter()), is(true));
  }

  @Test
  public void testFocus() {
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().focus(mock.getDriverAdapterMock());
    verify(mock.getDriverAdapterMock(), times(1))
        .executeScript(FOCUS_VIA_JAVASCRIPT, mock.getWebElementMock());
  }

  @Test
  public void testScrollToTop() {
    MockUtilities mock = new MockUtilities();
    when(mock.getElementAdapter().isDisplayed()).thenReturn(false).thenReturn(false)
        .thenReturn(false).thenReturn(true);
    mock.getElementAdapter().scrollIntoView(mock.getDriverAdapterMock(), ScrollOptions.TOP);
    verify(mock.getDriverAdapterMock(), times(2))
        .executeScript(SCROLL_TOP_VIA_JAVASCRIPT, mock.getWebElementMock());
  }

  @Test
  public void testScrollToCenter() {
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().scrollIntoView(mock.getDriverAdapterMock(), ScrollOptions.CENTER);
    verify(mock.getDriverAdapterMock(), times(1))
        .executeScript(SCROLL_CENTER_VIA_JAVASCRIPT, mock.getWebElementMock());
  }

  @Test
  public void testBlur() {
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().blur(mock.getDriverAdapter());
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
        .executeScript(contains(SHADOW_ROOT_DETECTION_SCRIPT_FRAGMENT),
            refEq(mock.getWebElementMock())))
        .thenReturn(Collections.singletonList(mock.getWebElementMock()));
    assertThat(mock.getElementAdapter().containsElements(byCss("css"), true), is(equalTo(1)));
  }

  @Test
  public void testMobileActionsThrow() {
    MockUtilities mock = new MockUtilities();
    assertThrows(() -> mock.getElementAdapter().flick(mock.getDriverAdapter(), 0, 0));
    assertThrows(() -> mock.getElementAdapter().flickItems(GestureDirection.DOWN));
  }
}
