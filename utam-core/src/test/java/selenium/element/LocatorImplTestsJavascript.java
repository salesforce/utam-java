package selenium.element;

import org.openqa.selenium.*;
import org.testng.annotations.Test;
import selenium.context.SeleniumContextProvider;
import selenium.context.WebDriverUtilities;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static selenium.element.LocatorNodeImplTestsJavascript.getJavascriptLocatorNode;
import static selenium.element.LocatorUtilities.EMPTY_FILTER;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class LocatorImplTestsJavascript {

  private static LocatorImpl getSimpleSingleNodeLocator(String css) {
    return new LocatorImpl.Javascript(new LocatorNodeImpl.Javascript(css));
  }

  @Test
  public void testGetSelectorStringForSingleNode() {
    final String EXPECTED = "return arguments[0].querySelectorAll(\"css\")";
    assertThat(
        new LocatorImpl.Javascript(new LocatorNodeImpl.Javascript("css")).getSelectorString(),
        is(equalTo(EXPECTED)));
    assertThat(
        new LocatorImpl.Javascript(
                new LocatorNodeImpl.Javascript("css", new LocatorNodeFilter.Index(1)))
            .getSelectorString(),
        is(equalTo(EXPECTED)));
    assertThat(
        new LocatorImpl.Javascript(
                new LocatorNodeImpl.Javascript(
                    "css",
                    "css",
                    new LocatorNodeFilter.Index(1),
                    ShadowBoundary.EXPAND_SHADOW_ROOT))
            .getSelectorString(),
        is(equalTo(EXPECTED)));
  }

  @Test
  public void testGetJavascriptForSingleNode() {
    LocatorImpl locator =
        new LocatorImpl.Javascript(
            new LocatorNodeImpl.Javascript(
                "css", "css", new LocatorNodeFilter.Index(1), ShadowBoundary.EXPAND_SHADOW_ROOT));
    assertThat(
        locator.getJavascriptForConsole(LocatorUtilities.Find.FILTERED_LIST), is(equalTo("$$(\"css\")")));
    assertThat(
        locator.getJavascriptForConsole(LocatorUtilities.Find.FILTERED_ELEMENT),
        is(equalTo("$$(\"css\")[1]")));
  }

  @Test
  public void testGetSelectorStringForMultipleNodes() {
    LocatorImpl.Javascript locator =
        new LocatorImpl.Javascript(
            new LocatorNodeImpl.Javascript("css0", new LocatorNodeFilter.Index(1)),
            new LocatorNodeImpl.Javascript("css1", new LocatorNodeFilter.Index(1)));
    String EXPECTED_STRING =
        "return arguments[0].querySelectorAll(\"css0\")[1].querySelectorAll(\"css1\")";
    assertThat(locator.getSelectorString(), is(equalTo(EXPECTED_STRING)));
  }

  @Test
  public void testGetJavascriptForMultipleNodes() {
    LocatorImpl.Javascript locator =
        new LocatorImpl.Javascript(
            new LocatorNodeImpl.Javascript("css0", new LocatorNodeFilter.Index(1)),
            new LocatorNodeImpl.Javascript("css1", new LocatorNodeFilter.Index(1)));
    String EXPECTED_STRING = "$$(\"css0\")[1].querySelectorAll(\"css1\")";
    assertThat(
        locator.getJavascriptForConsole(LocatorUtilities.Find.FILTERED_LIST),
        is(equalTo(EXPECTED_STRING)));
    assertThat(
        locator.getJavascriptForConsole(LocatorUtilities.Find.FILTERED_ELEMENT),
        is(equalTo(EXPECTED_STRING + "[1]")));
  }

  @Test
  public void testSelectorStringWithNestedChildren() {
    LocatorImpl parent = getSimpleSingleNodeLocator("parent");
    LocatorImpl child = getSimpleSingleNodeLocator("child");
    LocatorImpl grandchild = getSimpleSingleNodeLocator("grandchild");
    LocatorImpl scoped = parent.scope(child).scope(grandchild);
    assertThat(
        scoped.getJavascriptForConsole(LocatorUtilities.Find.FILTERED_LIST),
        is(
            equalTo(
                "$(\"parent\")"
                    + ".querySelector(\"child\")"
                    + ".querySelectorAll(\"grandchild\")")));
    assertThat(
        scoped.getSelectorString(),
        is(
            equalTo(
                "return arguments[0]"
                    + ".querySelector(\"parent\")"
                    + ".querySelector(\"child\")"
                    + ".querySelectorAll(\"grandchild\")")));
  }

  @Test
  public void testGetRoot() {
    LocatorImpl locator = new LocatorImpl.Javascript(getJavascriptLocatorNode());
    assertThat(locator.getRoot(), is(equalTo(getJavascriptLocatorNode())));
  }

  @Test
  public void testGetSelfCopy() {
    LocatorImpl locator = new LocatorImpl.Javascript(new LocatorNodeImpl.Javascript("one"));
    LocatorImpl copy = locator.getSelfCopy();
    assertThat(copy, is(equalTo(locator)));
    assertThat(copy, is(instanceOf(LocatorImpl.Javascript.class)));

    locator = locator.add(new LocatorNodeImpl.Javascript("two"));
    copy = locator.getSelfCopy();
    assertThat(copy, is(not(equalTo(locator))));
  }

  @Test
  public void testAddNodeWithFilterAndBoundary() {
    LocatorImpl parent = new LocatorImpl.Javascript(new LocatorNodeImpl.Javascript("parent"));
    LocatorNodeImpl added =
        new LocatorNodeImpl.Javascript(
            "css", "css", EMPTY_FILTER, ShadowBoundary.EXPAND_SHADOW_ROOT);
    LocatorImpl locator = parent.add(added);
    assertThat(locator.getRoot().getNext(), is(notNullValue()));
    assertThat(locator.getRoot(), is(equalTo(parent.getRoot())));
    assertThat(locator.getRoot().getNext(), is(not(sameInstance(parent.getRoot()))));
    assertThat(locator.getRoot().getNext(), is(equalTo(added)));
    assertThat(locator.getRoot().getNext(), is(not(sameInstance(added))));
  }

  @Test
  public void testFindRootOnly() {
    final String SELECTOR_STRING = "css";
    LocatorImpl locator =
        new LocatorImpl.Javascript(new LocatorNodeImpl.Javascript(SELECTOR_STRING));
    RootElementMock mock = new RootElementMock(SELECTOR_STRING);
    List<WebElement> found = locator.find(mock.utilities, LocatorUtilities.Find.FILTERED_LIST);
    // found right elements - check text to confirm
    assertThat(found.size(), is(1));
    assertThat(found.get(0), is(equalTo(mock.rootElement)));
  }

  @Test
  public void testFindRootNothingFound() {
    final String SELECTOR_STRING = "css";
    LocatorImpl locator =
        new LocatorImpl.Javascript(new LocatorNodeImpl.Javascript(SELECTOR_STRING));
    WebDriver driver = mock(WebDriver.class);
    WebDriverUtilities utilities = new SeleniumContextProvider(driver);
    assertThrows(() -> locator.find(utilities, LocatorUtilities.Find.FILTERED_LIST));
    List<WebElement> found = locator.find(utilities, LocatorUtilities.Find.FILTERED_NULLABLE_LIST);
    // found right elements - check text to confirm
    assertThat(found.isEmpty(), is(true));
  }

  @Test
  public void testFindRootWithChildNothingFound() {
    final String ROOT_SELECTOR_STRING = "css1";
    final String NEXT_SELECTOR_STRING = "css2";
    RootElementMock mock = new RootElementMock(ROOT_SELECTOR_STRING, NEXT_SELECTOR_STRING);
    LocatorImpl locator =
        new LocatorImpl.Javascript(
            new LocatorNodeImpl.Javascript(ROOT_SELECTOR_STRING),
            new LocatorNodeImpl.Javascript(NEXT_SELECTOR_STRING));
    assertThrows(() -> locator.find(mock.utilities, LocatorUtilities.Find.FILTERED_LIST));
    List<WebElement> found = locator.find(mock.utilities, LocatorUtilities.Find.FILTERED_NULLABLE_LIST);
    // found right elements - check text to confirm
    assertThat(found.isEmpty(), is(true));
  }

  @Test
  public void testFindRootWithChildFound() {
    final String ROOT_SELECTOR_STRING = "css1";
    final String NEXT_SELECTOR_STRING = "css2";
    RootElementMock mock = new RootElementMock(ROOT_SELECTOR_STRING, NEXT_SELECTOR_STRING);
    LocatorImpl locator =
        new LocatorImpl.Javascript(
            new LocatorNodeImpl.Javascript(ROOT_SELECTOR_STRING),
            new LocatorNodeImpl.Javascript(NEXT_SELECTOR_STRING));
    mock.setJavascriptMock(
        "return arguments[0].querySelectorAll(\"css2\")", mock.rootElement, mock.nextElement);
    List<WebElement> found = locator.find(mock.utilities, LocatorUtilities.Find.FILTERED_LIST);
    // found right elements - check text to confirm
    assertThat(found.size(), is(1));
    assertThat(found.get(0), is(equalTo(mock.nextElement)));
  }

  @Test
  public void testFindRootWithChildError() {
    final String ROOT_SELECTOR_STRING = "css1";
    final String NEXT_SELECTOR_STRING = "css2";
    RootElementMock mock = new RootElementMock(ROOT_SELECTOR_STRING, NEXT_SELECTOR_STRING);
    LocatorImpl locator =
        new LocatorImpl.Javascript(
            new LocatorNodeImpl.Javascript(ROOT_SELECTOR_STRING),
            new LocatorNodeImpl.Javascript(NEXT_SELECTOR_STRING));
    mock.setJavascriptErrorMock("return arguments[0].querySelectorAll(\"css2\")", mock.rootElement);
    assertThrows(
        NotFoundException.class, () -> locator.find(mock.utilities, LocatorUtilities.Find.FILTERED_LIST));
  }

  static class RootElementMock {

    private final WebDriverUtilities utilities;
    private final JavascriptExecutor javascriptExecutor;
    private final WebElement rootElement;
    private WebElement nextElement;

    RootElementMock(String rootSelector) {
      WebDriver driver = mock(WebDriver.class);
      javascriptExecutor = mock(JavascriptExecutor.class);
      rootElement = mock(WebElement.class);
      when(driver.findElements(By.cssSelector(rootSelector)))
          .thenReturn(Collections.singletonList(rootElement));
      utilities = mock(WebDriverUtilities.class);
      when(utilities.getWebDriver()).thenReturn(driver);
      when(utilities.getExecutor()).thenReturn(javascriptExecutor);
    }

    RootElementMock(String rootSelector, String nextSelector) {
      this(rootSelector);
      nextElement = mock(WebElement.class);
      when(rootElement.findElements(By.cssSelector(nextSelector)))
          .thenReturn(Collections.singletonList(nextElement));
    }

    void setJavascriptMock(String javascript, SearchContext context, WebElement returns) {
      when(javascriptExecutor.executeScript(javascript, context))
          .thenReturn(Stream.of(returns).collect(Collectors.toList()));
    }

    void setJavascriptErrorMock(String javascript, SearchContext context) {
      when(javascriptExecutor.executeScript(javascript, context))
          .thenThrow(new WebDriverException("error"));
    }
  }
}
