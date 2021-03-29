package utam.core.selenium.element;

import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.PageObjectsFactoryImpl;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.consumer.Utilities;
import org.hamcrest.Matchers;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.context.SeleniumContextProvider;
import utam.core.selenium.expectations.ElementListExpectations;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.core.selenium.element.PageObjectElementBuilderImpl.countElements;
import static utam.core.selenium.element.PageObjectElementBuilderImpl.setContainerParameters;

/**
 * element builder tests
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class PageObjectElementBuilderImplTests {

  private static final String LOCATOR_WITH_PARAM = "locator[title='%s']";
  private static final String LOCATOR_WITHOUT_PARAM = "locator";

  private static String getSelectorString(BaseElement element) {
    return ((ElementImpl) element).getLocator().getSelectorString();
  }

  private static PageObjectElementBuilderImpl getElementBuilder(
      SeleniumContext seleniumContext, BaseElement element, boolean isNullable) {
    PageObjectsFactory factory = mock(PageObjectsFactory.class);
    when(factory.getSeleniumContext()).thenReturn(seleniumContext);
    return new PageObjectElementBuilderImpl(factory, element, isNullable);
  }

  static BaseElement getCssElement(String css, SeleniumContext context) {
    return new ElementImpl(new LocatorImpl(new LocatorNodeImpl.Css(css)), context);
  }

  static BaseElement getJavascriptElement(String selector, SeleniumContext context) {
    return new ElementImpl(
        new LocatorImpl.Javascript(new LocatorNodeImpl.Javascript(selector)),
        context);
  }

  static Function<Integer, LocatorNode.Filter> getFilter(List<Clickable> list) {
    return index -> LocatorUtilities.getElementLocator(list.get(index)).getRoot().getFilter();
  }

  /**
   * Tests that the count method returns the count of elements in the list when the nullable
   * argument is false <br>
   * Tests that the count method returns zero when the list of elements is empty and the nullable
   * argument is true
   */
  @Test
  void testCountExpectations() {
    ElementImplTests.ActionsMock mock = new ElementImplTests.ActionsMock();
    List<WebElement> elements = Stream.of(mock.webElement).collect(Collectors.toList());
    ElementListExpectations<Integer> expectationFalse = countElements(false);
    Integer result = expectationFalse.apply(mock.utilities).apply(elements);
    assertThat(result, is(equalTo(1)));
    assertThat(expectationFalse.getLogMessage(), is(equalTo("get elements count")));
    assertThat(expectationFalse.returnIfNothingFound(), is(nullValue()));

    ElementListExpectations<Integer> expectationTrue = countElements(true);
    assertThat(expectationTrue.returnIfNothingFound(), is(equalTo(0)));
    result = expectationTrue.apply(mock.utilities).apply(new ArrayList<>());
    assertThat(result, is(equalTo(0)));
    result = expectationTrue.apply(mock.utilities).apply(elements);
    assertThat(result, is(equalTo(1)));
  }

  /** The count() method should return the proper count of elements */
  @Test
  public void testCount() {
    ElementImplTests.ActionsMock mock = new ElementImplTests.ActionsMock();
    BaseElement element = mock.getElementImpl();
    assertThat(
        getElementBuilder(new ElementImplTests.MockHelper().context, element, false)
            .count(true, LocatorUtilities.getElementLocator(element)),
        is(equalTo(1)));
  }

  @Test
  public void testCountIzZeroForNullable() {
    ElementImplTests.ActionsMock mock = new ElementImplTests.ActionsMock();
    BaseElement element = mock.getElementImplNullable();
    assertThat(
        getElementBuilder(new ElementImplTests.MockHelper().context, element, true)
            .count(true, LocatorUtilities.getElementLocator(element)),
        is(equalTo(0)));
  }

  /** The setParameters() method should apply parameters to locator */
  @Test
  public void testSetParameters() {
    SeleniumContext seleniumContext = new ElementImplTests.MockHelper().context;
    BaseElement baseElement = getCssElement(LOCATOR_WITH_PARAM, seleniumContext);
    Actionable test =
        getElementBuilder(seleniumContext, baseElement, true).build(Actionable.class, "param");
    assertThat(
        getSelectorString(test), Matchers.is(equalTo(String.format(LOCATOR_WITH_PARAM, "param"))));
  }

  /**
   * The setParameters() method should return the same copy of locator if there are no parameters to
   * set
   */
  @Test
  public void testSetParametersWithEmptyParam() {
    ElementImplTests.Mock mock = new ElementImplTests.Mock();
    BaseElement element = mock.getElementImpl();
    Actionable test = getElementBuilder(mock.context, element, false).build(Actionable.class);
    assertThat(test, Matchers.is(equalTo(element)));
    assertThat(getSelectorString(test), is(equalTo(LOCATOR_WITHOUT_PARAM)));
  }

  /**
   * The asList() method should convert an object of a given type to a list of objects of the same
   * type
   */
  @Test
  public void testAsList() {
    ElementImplTests.Mock mock = new ElementImplTests.Mock();
    BaseElement element = mock.getElementImpl();
    List<Clickable> list = getElementBuilder(mock.context, element, true).buildList(Clickable.class);
    assertThat(list, Matchers.is(instanceOf(Iterable.class)));
    assertThat(list.size(), Matchers.is(equalTo(1)));
  }

  @Test
  public void testAsListNullable() {
    ElementImplTests.Mock mock = new ElementImplTests.Mock();
    BaseElement element = mock.getElementImplNullable();
    List<Clickable> list = getElementBuilder(mock.context, element, true).buildList(Clickable.class);
    assertThat(list, Matchers.is(instanceOf(Iterable.class)));
    assertThat(list.size(), Matchers.is(equalTo(0)));
  }

  @Test
  public void testAsListJavascript() {
    WebDriver driver = mock(WebDriver.class);
    String selector = "css";
    when(driver.findElements(By.cssSelector(selector)))
        .thenReturn(
            Stream.of(mock(WebElement.class), mock(WebElement.class)).collect(Collectors.toList()));
    SeleniumContext context = new SeleniumContextProvider(driver);
    BaseElement element = getJavascriptElement(selector, context);
    List<Clickable> list = getElementBuilder(context, element, true).buildList(Clickable.class);
    assertThat(list.size(), is(equalTo(2)));
    Function<Integer, LocatorNode.Filter> filter = getFilter(list);
    assertThat(filter.apply(0).getFilterString(), is(emptyString()));
    assertThat(filter.apply(1).getFilterString(), is("[1]"));
  }

  @Test
  public void testContainerSetParameters() {
    LocatorNode locator = new LocatorNodeImpl.Css("css[%d]");
    SeleniumContext seleniumContext = new SeleniumContextProvider(mock(WebDriver.class));
    PageObjectsFactory factory = new PageObjectsFactoryImpl(Utilities.test(), seleniumContext);
    ContainerElement element = new ElementContainerImpl(new LocatorImpl(locator), factory, false);
    element = setContainerParameters(factory, element, 1);
    ElementContainerImplTests.TestLoad testLoad = element.load(ElementContainerImplTests.TestLoad.class, "root-selector");
    List<LocatorNode> testLoadRoot = testLoad.getTestRootLocatorNodes();
    assertThat(testLoadRoot.size(), is(equalTo(2)));
    assertThat(((LocatorNodeImpl) testLoadRoot.get(0)).getSelectorString(), is(equalTo("css[1]")));
    assertThat(testLoadRoot.get(1).getSelector().getValue(), is(equalTo("root-selector")));
    assertThat(testLoadRoot.get(1).getScopeTransformer(), is(equalTo(ShadowBoundary.NONE)));
  }
}
