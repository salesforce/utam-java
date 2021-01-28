package utam.core.selenium.element;

import utam.core.appium.element.AccessibilityId;
import utam.core.appium.element.ClassChain;
import utam.core.appium.element.Mobile;
import utam.core.appium.element.UIAutomator;
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageMarker;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.consumer.LocationPolicy;
import utam.core.framework.consumer.LocationPolicyType;
import utam.core.framework.consumer.UtamError;
import io.appium.java_client.MobileBy;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.selenium.context.SeleniumContextProvider;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static utam.core.framework.consumer.LocationPolicyType.CHAIN;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.element.LocatorUtilities.*;
import static utam.core.selenium.element.ShadowBoundary.EXPAND_SHADOW_ROOT;

/**
 * Provides tests for the Utilities class
 *
 * @author james.evans
 */
public class LocatorUtilitiesTests {

  private static final String SELECTOR_STRING = "selectorString";
  private static final String SCOPE_SELECTOR = "scope";
  private static final Locator SCOPE =
      LocatorUtilities.getSingleNodeLocator(
          new LocatorNodeImpl.Css(SCOPE_SELECTOR), LocationPolicyType.getDefault());

  private static LocatorImpl getElementLocator(Field field) {
    LocationPolicy policy = LocationPolicyType.getDefault();
    ElementMarker.Find annotation = field.getDeclaredAnnotation(ElementMarker.Find.class);
    LocatorNode node = getLocatorNode(getElementSelectorFromAnnotation(annotation),
            EMPTY_FILTER,
            ShadowBoundary.valueOf(annotation.expand()),
            policy);
    return (LocatorImpl) LocatorUtilities.getSingleNodeLocator(node, policy);
  }

  private static LocatorUtilities.Builder getLocatorBuilder(Map<String, Locator> scoped) {
    return new LocatorUtilities.Builder(LocationPolicyType.getDefault(), SCOPE, scoped);
  }

  private static Selector getCssSelector() {
    return new Web.SelectorImpl(Selector.Type.CSS, LocatorUtilitiesTests.SELECTOR_STRING);
  }

  /** The setParameters method should not modify the string with no parameter markers */
  @Test
  public void testSetParametersWithNoParametersInString() {
    Locator.Parameters parameters = new LocatorParameters("testValue");
    assertThat(parameters.apply("test"), is(equalTo("test")));
  }

  /** The setParameters method should not modify the string with no parameter markers */
  @Test
  public void testSetParameters() {
    LocatorParameters parameters = new LocatorParameters("testValue");
    assertThat(parameters.apply("test '%s'"), is(equalTo("test 'testValue'")));
  }

  /** The getSelector method should return a valid selector using css selector */
  @Test
  public void testGetSelectorCss() {
    LocatorNode locator =
        LocatorUtilities.getLocatorNode(
            getCssSelector(), EMPTY_FILTER, ShadowBoundary.NONE, LocationPolicyType.getDefault());
    assertThat(locator.getSelector().getValue(), is(equalTo(SELECTOR_STRING)));
  }

  /** The getSelector method should return a valid selector using css selector */
  @Test
  public void testGetSelectorJavascript() {
    LocatorNode locator =
        LocatorUtilities.getLocatorNode(
                getCssSelector(),EMPTY_FILTER, ShadowBoundary.NONE, LocationPolicyType.JAVASCRIPT);
    assertThat(locator.getSelector().getValue(), is(equalTo(SELECTOR_STRING)));
  }

  /** The getElement method should return a valid element */
  @Test
  public void testGetElement() {
    WebDriver driver = mock(WebDriver.class);
    SeleniumContextProvider provider = new SeleniumContextProvider(driver);
    Locator locator = mock(LocatorImpl.class);
    assertThat(LocatorUtilities.getElement(locator, provider), is(not(nullValue())));
  }

  /** The getElement method throw the proper exception with a null locator */
  @Test
  public void testGetElementWithNullLocatorThrows() {
    WebDriver driver = mock(WebDriver.class);
    SeleniumContextProvider provider = new SeleniumContextProvider(driver);
    UtamError e = expectThrows(UtamError.class, () -> LocatorUtilities.getElement(null, provider));
    assertThat(e.getMessage(), is(equalTo(ERR_LOCATOR_IS_NULL)));
  }

  /** The getLocator method should return the same locator instance for a given element instance */
  @Test
  public void testGetLocator() {
    WebDriver driver = mock(WebDriver.class);
    SeleniumContextProvider provider = new SeleniumContextProvider(driver);

    // Chain
    Locator locator =
        LocatorUtilities.getSingleNodeLocator(
            new LocatorNodeImpl.Css(SELECTOR_STRING), LocationPolicyType.getDefault());
    ElementImpl element = new ElementImpl(locator, provider);
    assertThat(element.getLocator(), sameInstance(locator));

    // Javascript
    locator =
        LocatorUtilities.getSingleNodeLocator(
            new LocatorNodeImpl.Javascript(SELECTOR_STRING), LocationPolicyType.JAVASCRIPT);
    element = new ElementImpl(locator, provider);
    assertThat(element.getLocator(), sameInstance(locator));
  }

  /** The find method should return a valid value */
  @Test
  public void testFind() {
    WebDriver driver = mock(WebDriver.class);
    WebElement element = mock(WebElement.class);
    when(driver.findElements(By.cssSelector(SELECTOR_STRING)))
        .thenReturn(Collections.singletonList(element));
    SeleniumContextProvider provider = new SeleniumContextProvider(driver);
    LocatorNode locator =
        LocatorUtilities.getLocatorNode(
                getCssSelector(), EMPTY_FILTER, ShadowBoundary.NONE, LocationPolicyType.getDefault());
    Actionable rootElement =
        LocatorUtilities.getElement(
            LocatorUtilities.getSingleNodeLocator(locator, LocationPolicyType.getDefault()), provider);
    assertThat(LocatorUtilities.find(rootElement), is(sameInstance(element)));
  }

  @Test
  public void testGetContainer() {
    LocatorNode locator =
        LocatorUtilities.getLocatorNode(
                getCssSelector(), EMPTY_FILTER, ShadowBoundary.NONE, LocationPolicyType.getDefault());
    PageObjectsFactory factory = mock(PageObjectsFactory.class);
    assertThat(
        LocatorUtilities.getContainer(
            LocatorUtilities.getSingleNodeLocator(locator, LocationPolicyType.getDefault()),
            false,
            factory),
        is(not(nullValue())));
  }

  @Test
  public void testGetContainerWithNullLocatorThrows() {
    PageObjectsFactory factory = mock(PageObjectsFactory.class);
    UtamError e = expectThrows(UtamError.class, () -> LocatorUtilities.getContainer(null, false, factory));
    assertThat(e.getMessage(), containsString(ERR_LOCATOR_IS_NULL));
  }

  /**
   * The getElementLocator method should return a valid locator when passed a field in a page object
   * class
   */
  @Test
  public void testGetElementLocator() throws NoSuchFieldException {
    BasePageObject page = new MockPageObject();
    Field field = page.getClass().getDeclaredField("unscopedElement");
    LocatorImpl selector = getElementLocator(field);
    assertThat(selector.getSelectorString(), is(equalTo(".fakeSelector")));
    LocatorUtilities.Builder builder = getLocatorBuilder(new HashMap<>());
    assertThat(builder.getLocator(field).getSelectorString(), is(equalTo(SCOPE_SELECTOR + "|.fakeSelector")));
    assertThat(builder.getContainerLocator(field).getSelectorString(), is(equalTo(SCOPE_SELECTOR)));
  }

  /**
   * The getElementLocator method should return a valid locator when passed a field in a page object
   * class
   */
  @Test
  public void testGetElementLocatorWithEmptySelector() throws NoSuchFieldException {
    BasePageObject page = new MockPageObject();
    Field field = page.getClass().getDeclaredField("emptySelectorElement");
    LocatorUtilities.Builder builder = getLocatorBuilder(new HashMap<>());
    assertThat(builder.getLocator(field).getSelectorString(), is(equalTo(SCOPE_SELECTOR )));
    assertThat(builder.getContainerLocator(field).getSelectorString(), is(equalTo(SCOPE_SELECTOR)));
  }

  /**
   * The getElementLocator method should return a valid locator when passed a field with a declared
   * scope in a page object class
   */
  @Test
  public void testGetElementLocatorWithScopedElement()
      throws NoSuchFieldException, SecurityException {
    BasePageObject page = new MockPageObject();
    Map<String, Locator> map = new HashMap<>();
    map.put("fakeScope", new LocatorImpl(new LocatorNodeImpl.Css("test")));
    Field field = page.getClass().getDeclaredField("scopedElement");
    LocatorUtilities.Builder builder = getLocatorBuilder(map);
    assertThat(
        builder.getLocator(field).getSelectorString(), is(equalTo("test|.fakeScopedSelector")));
    assertThat(builder.getContainerLocator(field).getSelectorString(), is(equalTo("test")));
  }

  /**
   * The getElementLocator method should return a valid locator when passed a field with a declared
   * text filter in a page object class
   */
  @Test
  public void testGetElementLocatorWithTextFilter() throws NoSuchFieldException, SecurityException {
    BasePageObject page = new MockPageObject();
    Field field = page.getClass().getDeclaredField("textFilterElement");
    LocatorImpl selector = getElementLocator(field);
    assertThat(selector.getRoot().getSelectorString(), is(equalTo(".fakeTextFilterSelector")));
  }

  /**
   * The getElementLocator method should return a valid locator when passed a field with a declared
   * text filter in a page object class
   */
  @Test
  public void testGetElementLocatorWithCssFilter() throws NoSuchFieldException, SecurityException {
    BasePageObject page = new MockPageObject();
    Field field = page.getClass().getDeclaredField("cssFilterElement");
    LocatorImpl selector = getElementLocator(field);
    assertThat(selector.getRoot().getSelectorString(), is(equalTo(".fakeCssFilterSelector")));
  }

  /**
   * The getElementLocator method should return a valid locator when passed a field with a declared
   * scope where the scope does not exists in a page object class
   */
  @Test
  public void tesAllGetElementLocatorWithMissingScopeThrows() throws SecurityException, NoSuchFieldException {
    Field f = MockPageObject.class.getDeclaredField("illegalElement");
    LocatorUtilities.Builder builder = getLocatorBuilder(new HashMap<>());
    UtamError e =
        expectThrows(
            UtamError.class,
            () -> builder.getLocator(f));
    assertThat(e.getMessage(), is(equalTo(String.format(NON_EXISTING_FIELD_ERROR, "missingScope"))));
    e =
            expectThrows(
                    UtamError.class,
                    () -> builder.getContainerLocator(f));
    assertThat(e.getMessage(), is(equalTo(String.format(NON_EXISTING_FIELD_ERROR, "missingScope"))));
  }

  @Test
  public void testByMethod() {
    assertThrows(IllegalArgumentException.class, () -> LocatorUtilities.by(null, "value"));
    assertThat(LocatorUtilities.by(Selector.Type.UIAUTOMATOR, "test"), is(equalTo(MobileBy.AndroidUIAutomator("test"))));
  }

  @Test
  public void testGetLocatorNodeMobile() {
    LocationPolicy locationPolicy = LocationPolicyType.getDefault();
    LocatorNode.Filter filter = EMPTY_FILTER;
    LocatorNode.Transformer transformer = ShadowBoundary.NONE;
    assertThat(LocatorUtilities.getLocatorNode(Mobile.byAccessibilityId("test"),
            filter, transformer, locationPolicy), is(instanceOf(AccessibilityId.class)));
    assertThat(LocatorUtilities.getLocatorNode(Mobile.byClassChain("test"),
            filter, transformer, locationPolicy), is(instanceOf(ClassChain.class)));
    assertThat(LocatorUtilities.getLocatorNode(Mobile.byUiAutomator("test"),
            filter, transformer, locationPolicy), is(instanceOf(UIAutomator.class)));
  }

  @Test
  public void testGetElementSelectorFromAnnotationMobile() {
    ElementMarker.Find annotation = mock(ElementMarker.Find.class);
    when(annotation.accessid()).thenReturn("test");
    assertThat(getElementSelectorFromAnnotation(annotation), is(equalTo(Mobile.byAccessibilityId("test"))));

    when(annotation.accessid()).thenReturn("");
    when(annotation.classchain()).thenReturn("test");
    assertThat(getElementSelectorFromAnnotation(annotation), is(equalTo(Mobile.byClassChain("test"))));

    when(annotation.classchain()).thenReturn("");
    when(annotation.uiautomator()).thenReturn(UIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX + "test");
    assertThat(getElementSelectorFromAnnotation(annotation), is(equalTo(Mobile.byUiAutomator("test"))));
  }

  @Test
  public void testGetElementSelectorFromAnnotationNull() {
    ElementMarker.Find annotation = mock(ElementMarker.Find.class);
    when(annotation.accessid()).thenReturn("");
    when(annotation.classchain()).thenReturn("");
    when(annotation.uiautomator()).thenReturn("");
    when(annotation.css()).thenReturn("");
    assertThat(getElementSelectorFromAnnotation(annotation), is(nullValue()));
  }

  @Test
  public void testGetPageSelectorFromAnnotationMobile() {
    PageMarker.Find annotation = mock(PageMarker.Find.class);
    when(annotation.accessid()).thenReturn("test");
    assertThat(getPageSelectorFromAnnotation(annotation), is(equalTo(Mobile.byAccessibilityId("test"))));

    when(annotation.accessid()).thenReturn("");
    when(annotation.classchain()).thenReturn("test");
    assertThat(getPageSelectorFromAnnotation(annotation), is(equalTo(Mobile.byClassChain("test"))));

    when(annotation.classchain()).thenReturn("");
    when(annotation.uiautomator()).thenReturn(UIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX + "test");
    assertThat(getPageSelectorFromAnnotation(annotation), is(equalTo(Mobile.byUiAutomator("test"))));
  }

  @Test
  public void testBuildLocator() {
    Locator locator = buildLocator(CHAIN, Web.byCss("selector"), true);
    List<LocatorNode> nodes = getAllNodes(locator);
    assertThat(nodes, hasSize(1));
    LocatorNode locatorNode = nodes.get(0);
    assertThat(locatorNode.getScopeTransformer(), is(equalTo(EXPAND_SHADOW_ROOT)));
    assertThat(locatorNode.getFilter(), is(equalTo(LocatorUtilities.EMPTY_FILTER)));
    assertThat(locatorNode.getSelector().getValue(), is(equalTo("selector")));
  }

  @Test
  public void testGetContextTransformer() {
    assertThat(LocatorUtilities.getContextTransformer(true), is(not(nullValue())));
  }

  @Test
  public void testGetSelector() {
    Selector selector = LocatorUtilities.getSelector("selector", Selector.Type.CSS);
    assertThat(selector.getValue(), is(equalTo("selector")));
    assertThat(selector.getType(), is(equalTo(Selector.Type.CSS)));
  }

  @Test
  public void testQueryTypeGetElement() {
    String transformString = QueryType.ELEMENT.getElement(
        "selector", LocatorUtilities.getContextTransformer(true));
    assertThat(transformString, is(equalTo(".shadowRoot.querySelector(\"selector\")")));
    transformString = QueryType.ELEMENT.getElement(
        "selector", LocatorUtilities.getContextTransformer(false));
    assertThat(transformString, is(equalTo(".querySelector(\"selector\")")));
  }

  @SuppressWarnings("unused")
  static class MockPageObject extends BasePageObject {

    @ElementMarker.Find(css = ".fakeSelector")
    Actionable unscopedElement;

    @ElementMarker.Find(css = ".fakeScopedSelector", scope = "fakeScope")
    Actionable scopedElement;

    @ElementMarker.Find(css = ".fakeMissingScopeSelector", scope = "missingScope")
    Actionable illegalElement;

    @ElementMarker.Find(css = ".fakeTextFilterSelector")
    Actionable textFilterElement;

    @ElementMarker.Find(css = ".fakeCssFilterSelector")
    Actionable cssFilterElement;

    @ElementMarker.Find(css = "")
    Actionable emptySelectorElement;
  }
}
