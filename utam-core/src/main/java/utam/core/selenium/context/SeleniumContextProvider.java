package utam.core.selenium.context;

import utam.core.framework.consumer.LocationPolicy;
import utam.core.framework.consumer.LocationPolicyType;
import org.openqa.selenium.*;
import org.openqa.selenium.support.ui.FluentWait;
import utam.core.framework.consumer.UtamTimeouts;
import utam.core.framework.consumer.UtamTimeoutsImpl;
import utam.core.selenium.element.ShadowRootWebElement;
import utam.core.selenium.expectations.DriverExpectations;
import utam.core.selenium.expectations.DriverWait;

import java.time.Duration;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.core.selenium.element.ShadowRootWebElement.SHADOW_ROOT_DETECTION_SCRIPT_FRAGMENT;

/**
 * Web Driver utilities and context
 *
 * @author elizaveta.ivanova
 * @since 222
 */
@SuppressWarnings({"unchecked"})
public class SeleniumContextProvider implements SeleniumContext, WebDriverUtilities {

  private static final List<Class<? extends Throwable>> IGNORE_EXCEPTIONS =
      Stream.of(
              NullPointerException.class,
              NoSuchElementException.class,
              StaleElementReferenceException.class,
              InvalidElementStateException.class,
              WebDriverException.class)
          .collect(Collectors.toList());
  private final WebDriver driver;
  private final LocationPolicy locationPolicy;
  private final UtamTimeouts timeouts;

  protected SeleniumContextProvider(
      WebDriver driver, LocationPolicy locationPolicy, Duration timeout) {
    this.driver = driver;
    this.timeouts = new UtamTimeoutsImpl();
    if(timeout != null) {
      this.timeouts.setFindTimeout(timeout);
      this.timeouts.setWaitForTimeout(timeout);
    }
    this.locationPolicy = locationPolicy;
  }

  public SeleniumContextProvider(WebDriver driver, LocationPolicy locationPolicy) {
    this(driver, locationPolicy, null);
  }

  public SeleniumContextProvider(WebDriver driver) {
    this(driver, LocationPolicyType.getDefault(), null);
  }

  private static String getCheckShadowRootScript() {
    return "var shadowRoot = "
        + SHADOW_ROOT_DETECTION_SCRIPT_FRAGMENT
        + "if (shadowRoot && shadowRoot.nodeType === 11 && !!shadowRoot.host) {"
        + "    return true;"
        + "}"
        + "return false;";
  }

  @Override
  public WebDriverUtilities getWebDriverUtils() {
    return this;
  }

  @Override
  public JavascriptExecutor getExecutor() {
    return (JavascriptExecutor) driver;
  }

  @Override
  public void executeJavaScript(String script, Object... parameters) {
    getExecutor().executeScript(script, parameters);
  }

  @Override
  public Object returnJavaScript(String script, Object... parameters) {
    Object object = getExecutor().executeScript(script, parameters);
    if (object == null) {
      throw new InvalidElementStateException(
          String.format("javascript '%s' returned null", script));
    }
    return object;
  }

  @Override
  public SearchContext expandShadowRoot(SearchContext sc) {
    if (!(sc instanceof WebElement)) {
      return sc;
    }
    Object res = returnJavaScript(getCheckShadowRootScript(), sc);
    if (!Boolean.TRUE.equals(res)) {
      throw new NotFoundException("shadow root is null");
    }
    return new ShadowRootWebElement((WebElement) sc);
  }

  @Override
  public DriverWait getDriverWait() {
    FluentWait<WebDriver> fluentWait =
        new FluentWait(driver)
            // had to use units instead duration for compatibility with SETI
            .withTimeout(timeouts.getWaitForTimeout())
            .pollingEvery(timeouts.getPollingInterval())
            .ignoreAll(IGNORE_EXCEPTIONS);
    return new DriverWait() {
      @Override
      public <T> T get(DriverExpectations<T> expectations) {
        return fluentWait.until(driver -> expectations.getter().apply(driver));
      }
    };
  }

  @Override
  public UtamTimeouts getTimeouts() {
    return timeouts;
  }

  @Override
  public WebDriver getWebDriver() {
    return driver;
  }

  @Override
  public LocationPolicy getLocationPolicy() {
    return locationPolicy;
  }
}
