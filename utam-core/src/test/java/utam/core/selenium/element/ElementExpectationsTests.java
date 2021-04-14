package utam.core.selenium.element;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.refEq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.element.ElementExpectations.absence;
import static utam.core.framework.element.ElementExpectations.blur;
import static utam.core.framework.element.ElementExpectations.clear;
import static utam.core.framework.element.ElementExpectations.clearAndType;
import static utam.core.framework.element.ElementExpectations.click;
import static utam.core.framework.element.ElementExpectations.focus;
import static utam.core.framework.element.ElementExpectations.getAttribute;
import static utam.core.framework.element.ElementExpectations.getText;
import static utam.core.framework.element.ElementExpectations.isPresent;
import static utam.core.framework.element.ElementExpectations.javascriptClick;
import static utam.core.framework.element.ElementExpectations.moveTo;
import static utam.core.framework.element.ElementExpectations.setText;
import static utam.core.framework.element.ElementExpectations.visibility;
import static utam.core.framework.element.ElementExpectations.waitFor;
import static utam.core.selenium.element.ElementAdapter.SCROLL_INTO_VIEW_ERR;
import static utam.core.selenium.element.ElementAdapter.SCROLL_INTO_VIEW_JS;
import static utam.core.selenium.element.ElementAdapter.SCROLL_TOP_VIA_JAVASCRIPT;

import org.openqa.selenium.ElementNotVisibleException;
import org.openqa.selenium.JavascriptException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Element;
import utam.core.element.Element.ScrollOptions;
import utam.core.framework.element.ElementExpectations;
import utam.core.framework.element.ElementExpectations.Match;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class ElementExpectationsTests {

  @Test
  public void testWaitForAbsence() {
    MockUtilities mock = new MockUtilities();
    ElementExpectations<Boolean> expectations = absence();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()), is(false));
    assertThat(expectations.apply(mock.getDriverAdapter(), new ElementAdapter(null)), is(true));
  }

  @Test
  public void testVisible() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().isDisplayed()).thenReturn(false);
    ElementExpectations<Boolean> visible = visibility(true);
    ElementExpectations<Boolean> invisible = visibility(false);
    assertThat(visible.apply(mock.getDriverAdapter(), mock.getElementAdapter()), is(false));
    assertThat(invisible.apply(mock.getDriverAdapter(), mock.getElementAdapter()), is(true));
    when(mock.getWebElementMock().isDisplayed()).thenReturn(true);
    assertThat(visible.apply(mock.getDriverAdapter(), mock.getElementAdapter()), is(true));
    assertThat(invisible.apply(mock.getDriverAdapter(), mock.getElementAdapter()), is(false));
  }

  @Test
  public void testClear() {
    MockUtilities mock = new MockUtilities();
    ElementExpectations<Element> expectations = clear();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(notNullValue()));
  }

  @Test
  public void testClearAndType() {
    final String testValue = "testValue";
    MockUtilities mock = new MockUtilities();
    ElementExpectations<Element> expectations = clearAndType(testValue);
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(notNullValue()));
  }

  @Test
  public void testGetAttribute() {
    final String attributeName = "attributeName";
    final String attributeValue = "attributeValue";
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().getAttribute(attributeName)).thenReturn(attributeValue);
    ElementExpectations<String> expectations = getAttribute(attributeName);
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(equalTo(attributeValue)));
    when(mock.getWebElementMock().getAttribute(attributeName)).thenReturn(null);
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(nullValue()));
  }

  @Test
  public void testGetText() {
    final String elementText = "textValue";
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().getText()).thenReturn(elementText);
    ElementExpectations<String> expectations = getText();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(equalTo(elementText)));
  }

  @Test
  public void testSetText() {
    final String textValue = "textValue";
    MockUtilities mock = new MockUtilities();
    ElementExpectations<Element> expectations = setText(textValue);
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(notNullValue()));
  }

  @Test
  public void testIsPresent() {
    MockUtilities mock = new MockUtilities();
    ElementExpectations<Match> expectations = isPresent();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(equalTo(Match.TRUE)));
  }

  @Test
  public void testJavascriptClick() {
    MockUtilities mock = new MockUtilities();
    ElementExpectations<Element> expectations = javascriptClick();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(notNullValue()));
  }

  @Test
  public void testClick() {
    MockUtilities mock = new MockUtilities();
    ElementExpectations<Element> expectations = click();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(notNullValue()));
    doThrow(new JavascriptException(
        "javascript error: Cannot read property 'defaultView' of undefined"))
        .when(mock.getWebElementMock()).click();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(notNullValue()));
    doThrow(new JavascriptException("javascript error: unknown JS error"))
        .when(mock.getWebElementMock()).click();
    assertThrows(() -> expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()));
  }

  @Test
  public void testMoveTo() {
    MockUtilities mock = new MockUtilities();
    ElementExpectations<Element> expectations = moveTo();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(notNullValue()));
  }

  @Test
  public void testFocus() {
    MockUtilities mock = new MockUtilities();
    ElementExpectations<Element> expectations = focus();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(notNullValue()));
  }

  @Test
  public void testScrollToTopWithElementAlreadyInView() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().isDisplayed()).thenReturn(true);
    ElementExpectations<Element> scrollTo = ElementExpectations.scrollTo(ScrollOptions.TOP);
    assertThat(scrollTo.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(notNullValue()));
  }

  @Test
  public void testScrollToTopWithElementAlignedToBottom() {
    MockUtilities mock = new MockUtilities();
    when(((JavascriptExecutor) mock.getWebDriverMock()).executeScript(
        contains(SCROLL_INTO_VIEW_JS),
        refEq(mock.getWebElementMock()))).then((invocation) -> when(
        ((WebElement) invocation.getArgument(1)).isDisplayed()).thenReturn(true));
    ElementExpectations<Element> scrollTo = ElementExpectations.scrollTo(ScrollOptions.TOP);
    assertThat(scrollTo.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(notNullValue()));
  }

  @Test
  public void testScrollToTopWithElementAlignedToTop() {
    MockUtilities mock = new MockUtilities();
    when(((JavascriptExecutor) mock.getWebDriverMock()).executeScript(
        contains(SCROLL_TOP_VIA_JAVASCRIPT),
        refEq(mock.getWebElementMock()))).then((invocation) -> when(
        ((WebElement) invocation.getArgument(1)).isDisplayed()).thenReturn(true));
    ElementExpectations<Element> scrollTo = ElementExpectations.scrollTo(ScrollOptions.TOP);
    assertThat(scrollTo.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(notNullValue()));
  }

  @Test
  public void testScrollIntoViewIfElementNotVisibleThrows() {
    MockUtilities mock = new MockUtilities();
    ElementNotVisibleException e =
        expectThrows(
            ElementNotVisibleException.class,
            () -> ElementExpectations.scrollTo(ScrollOptions.TOP)
                .apply(mock.getDriverAdapter(), mock.getElementAdapter()));
    assertThat(
        "error cause element is not visible",
        e.getMessage(),
        containsString(SCROLL_INTO_VIEW_ERR));
  }

  @Test
  public void testBlur() {
    MockUtilities mock = new MockUtilities();
    ElementExpectations<Element> expectations = blur();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(notNullValue()));
  }

  @Test
  public void testWaitFor() {
    MockUtilities mock = new MockUtilities();
    ElementExpectations<Element> expectations = waitFor(mock::getElementAdapter);
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        is(notNullValue()));
  }
}