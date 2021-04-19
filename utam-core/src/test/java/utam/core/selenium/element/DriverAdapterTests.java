package utam.core.selenium.element;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.contains;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static utam.core.driver.DriverTimeouts.TEST;

import java.util.Collections;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.driver.Driver;
import utam.core.driver.Expectations;
import utam.core.element.FindContext.Type;
import utam.core.framework.element.ExpectationsImpl;


public class DriverAdapterTests {

  @Test
  public void testCreation() {
    MockUtilities mock = new MockUtilities();
    DriverAdapter adapter = (DriverAdapter) mock.getDriverAdapter();
    assertThat(adapter.getSeleniumDriver(), is(instanceOf(WebDriver.class)));
    assertThat(adapter.isMobile(), is(false));
    assertThat(adapter.isNative(), is(false));
    assertThat(adapter.getSeleniumDriver(), is(notNullValue()));
  }

  /**
   * Tests that the executeJavaScript method will execute JavaScript
   */
  @Test
  public void testExecuteScript() {
    MockUtilities mock = new MockUtilities();
    when(mock.getExecutorMock().executeScript(contains("return arguments[0]"), any()))
        .then((invocation) -> invocation.getArgument(1));
    final String JAVASCRIPT_RETURN_VALUE = "testValue";
    Object scriptReturnValue =
        mock.getDriverAdapter().executeScript("return arguments[0]", JAVASCRIPT_RETURN_VALUE);
    assertThat(scriptReturnValue, is(instanceOf(Object.class)));
    assertThat(scriptReturnValue.toString(), is(equalTo(JAVASCRIPT_RETURN_VALUE)));
  }

  @Test
  public void testSetPageContextToNative() {
    assertThrows(() -> new MockUtilities().getDriverAdapter().setPageContextToNative());
  }

  @Test
  public void testSetPageContextToWebView() {
    assertThrows(() -> new MockUtilities().getDriverAdapter().setPageContextToNative());
  }

  @Test
  public void testTestSetPageContextToWebView() {
    assertThrows(() -> new MockUtilities().getDriverAdapter().setPageContextToNative());
  }

  @Test
  public void testFindElement() {
    MockUtilities mock = new MockUtilities();
    assertThat(mock.getDriverAdapter().findElement(LocatorBy.byCss("not-existing"), Type.NULLABLE)
        .isNull(), is(true));
    when(mock.getWebDriverMock().findElement(By.cssSelector("test")))
        .thenReturn(mock(WebElement.class));
    assertThat(mock.getDriverAdapter().findElement(LocatorBy.byCss("test"), Type.EXISTING).isNull(),
        is(false));
  }

  @Test
  public void testFindElements() {
    MockUtilities mock = new MockUtilities();
    Driver driver = mock.getDriverAdapter();
    assertThat(driver.findElements(LocatorBy.byCss("not-existing"), Type.NULLABLE),
        is(empty()));
    when(mock.getWebDriverMock().findElements(By.cssSelector("test")))
        .thenReturn(Collections.singletonList(mock(WebElement.class)));
    assertThat(driver.findElements(LocatorBy.byCss("test"), Type.EXISTING), is(not(empty())));
  }

  @Test
  public void testWaitFor() {
    Driver driver = new MockUtilities().getDriverAdapter();
    Expectations<Object> expectations = new ExpectationsImpl<>("test", object -> object != null);
    assertThat(
        driver.waitFor(TEST.getWaitForTimeout(), TEST.getPollingInterval(), expectations),
        is(true));
    Expectations<Object> nullExpectations = new ExpectationsImpl<>("test", object -> null);
    assertThrows(() -> driver
        .waitFor(TEST.getWaitForTimeout(), TEST.getPollingInterval(), nullExpectations));
    Expectations<Object> falseExpectations = new ExpectationsImpl<>("test", object -> false);
    assertThrows(() -> driver
        .waitFor(TEST.getWaitForTimeout(), TEST.getPollingInterval(), falseExpectations, null));
  }

  @Test
  public void testGetUrl() {
    String url = "url";
    MockUtilities mock = new MockUtilities();
    when(mock.getWebDriverMock().getCurrentUrl()).thenReturn(url);
    assertThat(mock.getDriverAdapter().getUrl(), is(equalTo(url)));
  }
}
