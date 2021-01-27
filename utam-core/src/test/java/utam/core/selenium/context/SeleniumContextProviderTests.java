package utam.core.selenium.context;

import org.openqa.selenium.*;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import utam.core.selenium.element.ShadowRootWebElement;
import utam.core.selenium.expectations.DriverExpectationsUtil;
import utam.core.selenium.expectations.DriverWait;

import java.time.Duration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.contains;
import static org.mockito.Mockito.*;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.element.ShadowRootWebElement.SHADOW_ROOT_DETECTION_SCRIPT_FRAGMENT;

/**
 * Tests methods on the SeleniumContextProvider object.
 *
 * @author james.evans
 */
public class SeleniumContextProviderTests {

  private static final String TEST_URL = "https://utam.dev/rocks/";
  private static final String JAVASCRIPT_RETURN_VALUE = "testValue";

  private WebDriver mockDriver;
  private WebElement mockElement;
  private WebElement mockElementWithShadowRoot;

  @BeforeClass
  public void setUp() {
    mockElement = mock(WebElement.class, withSettings().extraInterfaces(WrapsDriver.class));
    mockElementWithShadowRoot =
        mock(WebElement.class, withSettings().extraInterfaces(WrapsDriver.class));
    mockDriver =
        mock(
            WebDriver.class,
            withSettings().extraInterfaces(JavascriptExecutor.class, SearchContext.class));
    when(mockDriver.getCurrentUrl()).thenReturn(TEST_URL);
    when(((JavascriptExecutor) mockDriver).executeScript(contains("return arguments[0]"), any()))
        .then((invocation) -> invocation.getArgument(1));
    when(((JavascriptExecutor) mockDriver)
            .executeScript(contains(SHADOW_ROOT_DETECTION_SCRIPT_FRAGMENT), any()))
        .thenReturn(Boolean.FALSE);
    when(((JavascriptExecutor) mockDriver)
            .executeScript(
                contains(SHADOW_ROOT_DETECTION_SCRIPT_FRAGMENT), refEq(mockElementWithShadowRoot)))
        .thenReturn(Boolean.TRUE);
  }

  /** Tests the default timeouts for the SeleniumContextProvider */
  @Test
  public void testDefaultTimeouts() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    assertThat(provider.getPollingTimeout().getSeconds(), is(equalTo(20L)));
  }

  /** Tests custom timeouts for the SeleniumContextProvider */
  @Test
  public void testCustomTimeouts() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    provider.setPollingTimeout(Duration.ofSeconds(60));
    assertThat(provider.getPollingTimeout().getSeconds(), is(equalTo(60L)));
  }

  /** Tests that the method to get WebDriver utilities returns an instance of WebDriverUtilities */
  @Test
  public void testGetWebDriverUtils() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    assertThat(provider.getWebDriverUtils(), is(instanceOf(WebDriverUtilities.class)));
  }

  /** Tests that the method to get a Selenium WebDriver object returns an instance of WebDriver */
  @Test
  public void testGetWebDriver() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    assertThat(provider.getWebDriver(), is(instanceOf(WebDriver.class)));
  }

  /**
   * Tests that the method to return a Selenium JavascriptExecutor object returns an instance of
   * JavascriptExecutor
   */
  @Test
  public void testGetExecutor() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    assertThat(provider.getExecutor(), is(instanceOf(JavascriptExecutor.class)));
  }

  /** Tests that the executeJavaScript method will execute JavaScript */
  @Test
  public void testExecuteScript() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    Object scriptReturnValue =
        provider.returnJavaScript("return arguments[0]", JAVASCRIPT_RETURN_VALUE);
    assertThat(scriptReturnValue, is(instanceOf(Object.class)));
    assertThat(scriptReturnValue.toString(), is(equalTo(JAVASCRIPT_RETURN_VALUE)));
  }

  /** Tests that the method to get a wait object returns an instance of the DriverWait class */
  @Test
  public void testGetDriverWait() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    assertThat(provider.getDriverWait(), is(instanceOf(DriverWait.class)));
  }

  /** Tests that the DriverWait returned from the provider object performs a wait. */
  @Test
  public void testDriverWaitsForCondition() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    DriverWait wait = provider.getDriverWait();
    assertThat(wait.get(DriverExpectationsUtil.getUrl()), is(equalTo(TEST_URL)));
  }

  /** Tests that the expandShadowRoot method returns a ShadowRootWebElement object */
  @Test
  public void testExpandShadowRoot() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    SearchContext expandedShadowRoot = provider.expandShadowRoot(mockElementWithShadowRoot);
    assertThat(expandedShadowRoot, is(instanceOf(ShadowRootWebElement.class)));
  }

  /**
   * Tests that the expandShadowRoot method throws an appropriate exception when passed an element
   * that does not contain a shadow root
   */
  @Test
  public void testExpandShadowRootWithElementWithoutShadowRoot() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    NotFoundException e =
        expectThrows(NotFoundException.class, () -> provider.expandShadowRoot(mockElement));
    assertThat(e.getMessage(), containsString("shadow root is null"));
  }

  /**
   * Tests that the expandShadowRoot method returns the argument passed in if the argument is not a
   * WebElement
   */
  @Test
  public void testExpandShadowRootWithElementWithNonElementArgument() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    SearchContext nonElementShadowRoot = provider.expandShadowRoot(mockDriver);
    assertThat(nonElementShadowRoot, is(equalTo(mockDriver)));
  }
}
