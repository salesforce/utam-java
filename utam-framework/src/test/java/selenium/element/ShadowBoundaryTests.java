package selenium.element;

import org.openqa.selenium.*;
import org.testng.annotations.Test;
import selenium.context.SeleniumContextProvider;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.*;
import static selenium.element.ShadowRootWebElement.SHADOW_ROOT_DETECTION_SCRIPT_FRAGMENT;

/**
 * Provides tests for the SearchContextTransformer enum
 *
 * @author james.evans
 */
public class ShadowBoundaryTests {

  /**
   * The fromBoolean static method should return the expected value for the correct boolean value
   */
  @Test
  public void testFromBoolean() {
    assertThat(
        ShadowBoundary.valueOf(true),
        is(equalTo(ShadowBoundary.EXPAND_SHADOW_ROOT)));
    assertThat(ShadowBoundary.valueOf(false), is(equalTo(ShadowBoundary.NONE)));
  }

  /** The getTransformedContext method should return a valid value */
  @Test
  public void testGetTransformedContext() {
    WebDriver driver = LocatorImplTestsUtilities.getMockDriverForCss();
    SeleniumContextProvider provider = new SeleniumContextProvider(driver);
    SearchContext result =
        ShadowBoundary.NONE.apply(
            provider.getWebDriverUtils().getWebDriver(), provider.getWebDriverUtils());
    assertThat(result, is(equalTo(driver)));
  }

  /** The getTransformedContext method should return a valid value when used with a WebElement */
  @Test
  public void testGetTransformedContextWithWebElement() {
    WebDriver driver = LocatorImplTestsUtilities.getMockDriverForCss();
    SeleniumContextProvider provider = new SeleniumContextProvider(driver);
    WebElement element = mock(WebElement.class);

    SearchContext result =
        ShadowBoundary.NONE.apply(element, provider.getWebDriverUtils());
    assertThat(result, is(equalTo(element)));
  }

  /**
   * The getTransformedContext method for a shadow root context should return a valid value when
   * used with a WebElement
   */
  @Test
  public void testGetTransformedContextForShadowRootWithWebElement() {
    WebElement element = mock(WebElement.class, withSettings().extraInterfaces(WrapsDriver.class));
    WebDriver driver =
        mock(
            WebDriver.class,
            withSettings().extraInterfaces(JavascriptExecutor.class, SearchContext.class));
    when(((JavascriptExecutor) driver)
            .executeScript(contains(SHADOW_ROOT_DETECTION_SCRIPT_FRAGMENT), refEq(element)))
        .thenReturn(Boolean.TRUE);

    SeleniumContextProvider provider = new SeleniumContextProvider(driver);

    SearchContext result =
        ShadowBoundary.EXPAND_SHADOW_ROOT.apply(
            element, provider.getWebDriverUtils());
    assertThat(result, is(instanceOf(ShadowRootWebElement.class)));
  }
}
