package selenium.element;

import framework.consumer.UtamError;
import org.openqa.selenium.NotFoundException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import selenium.context.SeleniumContextProvider;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static selenium.element.LocatorImpl.*;
import static selenium.element.LocatorImplTestsUtilities.*;
import static selenium.element.LocatorNodeImpl.ERR_ELEMENT_SELECTED_BY;
import static selenium.element.LocatorNodeImplTestsCss.getChildLocatorNode;
import static selenium.element.LocatorNodeImplTestsCss.getParentLocatorNode;
import static selenium.element.LocatorNodeImplTestsJavascript.getJavascriptLocatorNode;
import static selenium.element.LocatorUtilities.EMPTY_FILTER;

/**
 * Provides tests for the AbstractLocator class
 *
 * @author james.evans
 */
public class LocatorImplTests {

  static LocatorImpl getParentLocator() {
    return new LocatorImpl(getParentLocatorNode());
  }

  private static LocatorImpl getChildLocator() {
    return new LocatorImpl(getChildLocatorNode());
  }

  private static LocatorImpl getGrandChildLocator() {
    return new LocatorImpl(new FakeGrandchildLocator(EMPTY_FILTER, ShadowBoundary.NONE));
  }

  /** The getFullPath method should return a valid value */
  @Test
  public void testGetSelectorString() {
    assertThat(
        getParentLocator().getSelectorString(), is(equalTo(LocatorImplTestsUtilities.SELECTOR_STRING)));
  }

  @Test
  public void testGetRoot() {
    assertThat(getParentLocator().getRoot(), is(equalTo(getParentLocatorNode())));
  }

  @Test
  public void testScopedLocator() {
    LocatorImpl scoped = getParentLocator().scope(getChildLocator());
    assertThat(scoped.getRoot().getNext(), is(notNullValue()));
    assertThat(scoped.getRoot(), is(equalTo(getParentLocatorNode())));
    assertThat(scoped.getRoot().getNext(), is(equalTo(getChildLocatorNode())));
  }

  /** The scope method should return a valid scoped locator value with nested children */
  @Test
  public void testScopeWithNestedChildren() {
    LocatorImpl scoped = getParentLocator().scope(getChildLocator()).scope(getGrandChildLocator());
    assertThat(
        scoped.getSelectorString(),
        is(
            equalTo(
                LocatorImplTestsUtilities.SELECTOR_STRING
                    + LOCATOR_CHAIN_SEPARATOR
                    + LocatorImplTestsUtilities.CHILD_SELECTOR_STRING
                    + LOCATOR_CHAIN_SEPARATOR
                    + LocatorImplTestsUtilities.GRANDCHILD_SELECTOR_STRING)));
  }

  private static LocatorImpl getSimpleSingleNodeLocator(String css) {
    return new LocatorImpl(new LocatorNodeImpl.Css(css));
  }

  @Test
  public void testAddNodeWithFilterAndBoundary() {
    LocatorImpl parent = getSimpleSingleNodeLocator("parent");
    LocatorNodeImpl added =
        new LocatorNodeImpl.Css(
            "css", EMPTY_FILTER, ShadowBoundary.EXPAND_SHADOW_ROOT);
    LocatorImpl locator = parent.add(added);
    assertThat(locator.getRoot().getNext(), is(notNullValue()));
    assertThat(locator.getRoot(), is(equalTo(parent.getRoot())));
    assertThat(locator.getRoot().getNext(), is(equalTo(added)));
    assertThat(locator.getRoot().getNext(), is(not(sameInstance(added))));
  }

  @Test
  public void testAddWithNestedChildren() {
    LocatorImpl parent = getSimpleSingleNodeLocator("parent");
    LocatorImpl child = getSimpleSingleNodeLocator("child");
    LocatorImpl grandChild = getSimpleSingleNodeLocator("child");
    LocatorImpl scoped = parent.scope(child).scope(grandChild);
    LocatorNodeImpl added =
        new LocatorNodeImpl.Css("css", EMPTY_FILTER, ShadowBoundary.EXPAND_SHADOW_ROOT);
    LocatorImpl locator = scoped.add(added);
    assertThat(locator.getLeaf(), is(equalTo(added)));
    assertThat(
        locator.getSelectorString(),
        is(equalTo("parent|child|child|*|css")));
  }

  /** The setIndex method should return the same locator with an index of zero */
  @Test
  public void testSetZeroIndex() {
    LocatorImpl locator = getParentLocator();
    LocatorImpl indexed = locator.setIndex(0);
    assertThat(indexed, is(sameInstance(locator)));
  }

  @Test
  public void testSetParametersNullOrEmpty() {
    Locator locator = getParentLocator();
    assertThat(locator.setParameters(null), is(sameInstance(locator)));
    assertThat(locator.setParameters(new LocatorParameters()), is(sameInstance(locator)));
  }

  /**
   * The setIndex method should return the a locator with an index argument with a non-zero index
   */
  @Test
  public void testSetIndexWithNonZeroIndex() {
    LocatorImpl locator = getParentLocator().setIndex(1);
    assertThat(locator.getRoot().getFilter(), is(equalTo(new LocatorNodeFilter.Index(1))));
    assertThat(
        locator.getSelectorString(),
        is(equalTo(SELECTOR_STRING + "[1]")));
  }

  /**
   * The setIndex method should return the a locator with an index argument for the last locator in
   * the chain with a non-zero index for a locator with nested children
   */
  @Test
  public void testSetIndexWithNonZeroIndexOnNestedLocator() {
    LocatorImpl scopedWithIndex =
        getParentLocator().scope(getChildLocator()).scope(getGrandChildLocator()).setIndex(1);
    assertThat(
        scopedWithIndex.getRoot().getFilter(), is(equalTo(LocatorNodeFilter.Empty.INSTANCE)));
    assertThat(scopedWithIndex.getLeaf().getFilter(), is(equalTo(new LocatorNodeFilter.Index(1))));
    assertThat(
        scopedWithIndex.getSelectorString(),
        is(equalTo(".fakeSelector|.fakeChildSelector|.fakeGrandChildSelector[1]")));
  }

  @Test
  public void testGetLeaf() {
    assertThat(getParentLocator().getLeaf(), is(equalTo(getParentLocatorNode())));
  }

  @Test
  public void testGetLeafScoped() {
    LocatorNodeImpl leaf =
        new LocatorNodeImpl.Css(
            "css", new LocatorNodeFilter.Index(1), ShadowBoundary.EXPAND_SHADOW_ROOT);
    assertThat(getParentLocator().add(leaf).getLeaf(), is(equalTo(leaf)));
  }

  /** The equals method should return true for the same AbstractLocator */
  @Test
  public void testEquals() {
    assertThat(getParentLocator().equals(getParentLocator().getCopy()), is(true));
  }

  /** The equals method should return true for the same AbstractLocator */
  @Test
  public void testEqualsWithUnequalLocators() {
    Locator locator = getParentLocator();
    assertThat(locator.equals(getChildLocator()), is(equalTo(false)));
    assertThat(locator.equals(new LocatorImpl.Javascript(getJavascriptLocatorNode())), is(false));
    assertThat(locator.equals(new Object()), is(equalTo(false)));
    assertThat(locator.equals(null), is(equalTo(false)));
  }

  /** A LocatorError instance should be able to be created with a specific error message */
  @Test
  public void testLocatorError() {
    String errorMessage = "test error";
    UtamError error = new UtamError(getParentLocator().getErrorPrefix() + errorMessage);
    assertThat(
        error.getMessage(),
        is(equalTo(String.format(ERR_ELEMENT_SELECTED_BY, SELECTOR_STRING) + errorMessage)));
  }

  /**
   * A LocatorError instance should be able to be created with a specific error message with an
   * inner exception
   */
  @Test
  public void testLocatorErrorWithInnerException() {
    String errorMessage = "test error";
    UnsupportedOperationException innerException =
        new UnsupportedOperationException("inner exception");
    UtamError error = new UtamError(getParentLocator().getErrorPrefix() + errorMessage, innerException);
    assertThat(
        error.getMessage(),
        is(equalTo(String.format(ERR_ELEMENT_SELECTED_BY, SELECTOR_STRING) + errorMessage)));
    assertThat(error.getCause(), is(equalTo(innerException)));
  }

  @Test
  public void testGetJavascriptThrows() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () -> getParentLocator().getJavascriptForConsole(LocatorUtilities.Find.FILTERED_LIST));
    assertThat(e.getMessage(), is(equalTo(JAVASCRIPT_STRING_NOT_SUPPORTED)));
  }

  @Test
  public void testGetSelfCopy() {
    LocatorImpl locator = new LocatorImpl(new LocatorNodeImpl.Css("one"));
    LocatorImpl copy = locator.getSelfCopy();
    assertThat(copy, is(equalTo(locator)));
    assertThat(copy, is(instanceOf(LocatorImpl.class)));
    assertThat(locator.getRoot(), is(equalTo(copy.getRoot())));
    assertThat(locator.getRoot(), is(not(sameInstance(copy.getRoot()))));

    locator = locator.add(new LocatorNodeImpl.Css("two"));
    assertThat(locator.getRoot().getNext(), is(not(nullValue())));
    copy = locator.getSelfCopy();
    assertThat(copy, is(not(equalTo(locator))));
    assertThat(locator.getRoot(), is(equalTo(copy.getRoot())));
    assertThat(locator.getRoot(), is(not(sameInstance(copy.getRoot()))));
    assertThat(copy.getRoot().getNext(), is(nullValue()));
  }

  @Test
  public void testFindRootOnly() {
    WebDriver driver = getMockDriverForCss();
    SeleniumContextProvider provider = new SeleniumContextProvider(driver);
    LocatorImpl locator = getSimpleSingleNodeLocator(SELECTOR_STRING); //workaround for clover plugin
    List<WebElement> rootElements =
            locator.find(provider.getWebDriverUtils(), LocatorUtilities.Find.FILTERED_LIST);
    // found right elements - check text to confirm
    assertThat(rootElements.size(), is(1));
    assertThat(rootElements.get(0).getText(), is(equalTo(getParentElementText())));
    //check same elements found as unfiltered
    assertThat(locator.find(provider.getWebDriverUtils(), LocatorUtilities.Find.UNFILTERED_LIST),
            is(equalTo(rootElements)));
    assertThat(locator.find(provider.getWebDriverUtils(), LocatorUtilities.Find.FILTERED_ELEMENT),
            is(equalTo(rootElements)));
  }

  @Test
  public void testFindRootOnlyNothingFoundNullableAllowed() {
    WebDriver driver = getMockDriverForCss();
    SeleniumContextProvider provider = new SeleniumContextProvider(driver);
    LocatorImpl notFound = new LocatorImpl(new LocatorNodeImpl.Css("css"));
    // if we search for not nullable list - throw exception
    assertThrows(
            NotFoundException.class,
            () -> notFound.find(provider.getWebDriverUtils(), LocatorUtilities.Find.FILTERED_LIST));
    // if we search for nullable list - return empty
    List<WebElement> found =
            notFound.find(provider.getWebDriverUtils(), LocatorUtilities.Find.FILTERED_NULLABLE_LIST);
    // found right elements - check text to confirm
    assertThat(found.isEmpty(), is(true));
  }

  @Test
  public void testFindRootWithChild() {
    WebDriver driver = getMockDriverForCss();
    SeleniumContextProvider provider = new SeleniumContextProvider(driver);
    LocatorImpl withChildNode = getSimpleSingleNodeLocator(SELECTOR_STRING)
            .scope(getSimpleSingleNodeLocator(CHILD_SELECTOR_STRING));
    List<WebElement> childElements =
            withChildNode.find(provider.getWebDriverUtils(), LocatorUtilities.Find.UNFILTERED_LIST);
    // found right elements - check text to confirm
    assertThat(childElements.size(), is(1));
    assertThat(childElements.get(0).getText(), is(equalTo(getChildElementText())));
  }

  @Test
  public void testFindRootWithChildNothingFoundNullableAllowed() {
    WebDriver driver = getMockDriverForCss();
    SeleniumContextProvider provider = new SeleniumContextProvider(driver);
    LocatorImpl notFound = new LocatorImpl(new LocatorNodeImpl.Css("css1"), new LocatorNodeImpl.Css("css2"));
    // if we search for not nullable list - throw exception
    assertThrows(
            NotFoundException.class,
            () -> notFound.find(provider.getWebDriverUtils(), LocatorUtilities.Find.FILTERED_LIST));
    // if we search for nullable list - return empty
    List<WebElement> found =
            notFound.find(provider.getWebDriverUtils(), LocatorUtilities.Find.FILTERED_NULLABLE_LIST);
    // found right elements - check text to confirm
    assertThat(found.isEmpty(), is(true));
  }
}
