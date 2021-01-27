package selenium.context;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebDriver;

/**
 * commonly used web driver utilities
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface WebDriverUtilities {

  JavascriptExecutor getExecutor();

  void executeJavaScript(String script, Object... parameters);

  Object returnJavaScript(String script, Object... parameters);

  SearchContext expandShadowRoot(SearchContext sc);

  WebDriver getWebDriver();
}
