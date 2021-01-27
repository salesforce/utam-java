package selenium.element;

import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;

import java.util.*;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.*;
import static org.testng.Assert.expectThrows;
import static selenium.element.LocatorNodeFilter.ERR_JAVASCRIPT_INTERPRETATION_NOT_SUPPORTED;
import static selenium.element.LocatorUtilities.EMPTY_FILTER;

/**
 * Provides tests for the AbstractLocatorFilter class
 *
 * @author james.evans
 */
public class LocatorNodeFilterTests {

  private static final String SECOND_TEST_ELEMENT = "second test element";
  private static final String FIRST_TEST_ELEMENT = "first test element";

  private static List<WebElement> getTestInnerTextElementList() {
    WebElement firstElement = mock(WebElement.class);
    when(firstElement.getText()).thenReturn(FIRST_TEST_ELEMENT);
    WebElement secondElement = mock(WebElement.class);
    when(secondElement.getText()).thenReturn(SECOND_TEST_ELEMENT);
    return Arrays.asList(firstElement, secondElement);
  }

  @Test
  public void testEquals() {
    assertThat(new LocatorNodeFilter.Index(75).equals(new LocatorNodeFilter.Index(75)), is(true));
    assertThat(LocatorNodeFilter.Empty.INSTANCE.equals(new LocatorNodeFilter.Index(75)), is(false));
    assertThat(LocatorNodeFilter.Empty.INSTANCE.equals(LocatorNodeFilter.Empty.INSTANCE), is(true));
    assertThat(LocatorNodeFilter.Empty.INSTANCE.equals(new Object()), is(false));
    assertThat(LocatorNodeFilter.Empty.INSTANCE.equals(null), is(false));
  }

  @Test
  public void testDefaultImpl() {
    assertThat(LocatorNodeFilter.Empty.INSTANCE.getCondition().test(null), is(true));
    assertThat(LocatorNodeFilter.Empty.INSTANCE.filter(null), is(emptyIterable()));
  }

  /** The applyParameters method should succeed without throwing an exception */
  @Test
  public void testZeroIndex() {
    LocatorNodeFilter filter = new LocatorNodeFilter.Index(0);
    assertThat(filter.getFilterString(), is(emptyString()));
    assertThat(filter.isEmpty(), is(true));
    NullPointerException e = expectThrows(NullPointerException.class, filter::getJavascript);
    assertThat(e.getMessage(), is(containsString(ERR_JAVASCRIPT_INTERPRETATION_NOT_SUPPORTED)));
    assertThat(filter.equals(filter.getCopy()), is(true));
    assertThat(filter.filter(null).isEmpty(), is(true));
    assertThat(filter.filter(new ArrayList<>()).isEmpty(), is(true));
    assertThat(filter.map(new ArrayList<>()).isEmpty(), is(true));
    assertThat(filter.map(null).isEmpty(), is(true));
    assertThat(filter.getCopy(), is(equalTo(filter)));
  }

  /** The getFirstMatchingIndex method should return the proper value */
  @Test
  public void testIndexOne() {
    LocatorNodeFilter filter = new LocatorNodeFilter.Index(1);
    assertThat(filter.isEmpty(), is(false));
    assertThat(filter.filter(getTestInnerTextElementList()).size(), is(equalTo(1)));
    assertThat(filter.map(getTestInnerTextElementList()).size(), is(equalTo(1)));
    assertThat(filter.getFilterString(), is(equalTo("[1]")));
    assertThat(filter.getCopy(), is(equalTo(new LocatorNodeFilter.Index(1))));
    assertThat(filter.getFilterString(), is(equalTo("[1]")));
  }

  /**
   * The getFirstMatchingIndex method should return the proper value when the argument is larger
   * than the size of the element list
   */
  @Test
  public void testIndexFilterGetFirstMatchingIndexWithInvalidValue() {
    LocatorNodeFilter filter = new LocatorNodeFilter.Index(2);
    List<WebElement> elements = getTestInnerTextElementList();
    assertThat(filter.filter(elements).size(), is(equalTo(0)));
    assertThat(filter.map(elements).size(), is(equalTo(0)));
  }

  /** The filter method should return correct element with index */
  @Test
  public void testIndexFilterWithValidIndex() {
    LocatorNodeFilter filter = new LocatorNodeFilter.Index(1);
    List<WebElement> filtered = filter.filter(getTestInnerTextElementList());
    assertThat(filtered, hasSize(1));
    assertThat(filtered.get(0).getText(), is(equalTo(SECOND_TEST_ELEMENT)));
    List<Map.Entry<Integer, WebElement>> mapped = filter.map(getTestInnerTextElementList());
    assertThat(mapped, hasSize(1));
    assertThat(mapped.get(0).getValue().getText(), is(equalTo(SECOND_TEST_ELEMENT)));
    assertThat(mapped.get(0).getKey(), is(equalTo(1)));
  }

  /** The filter method should return empty list with incorrect element with index */
  @Test
  public void testIndexFilterWithInvalidIndex() {
    LocatorNodeFilter filter = new LocatorNodeFilter.Index(3);
    List<WebElement> elements = Collections.singletonList(mock(WebElement.class));
    assertThat(filter.filter(elements), is(empty()));
    assertThat(filter.map(elements), is(empty()));
  }

  @Test
  public void emptyFilterTest() {
    LocatorNodeFilter filter = LocatorNodeFilter.Empty.INSTANCE;
    assertThat(filter.getFilterString(), is(emptyString()));
    assertThat(filter.isEmpty(), is(true));
    NullPointerException e = expectThrows(NullPointerException.class, filter::getJavascript);
    assertThat(e.getMessage(), is(containsString(ERR_JAVASCRIPT_INTERPRETATION_NOT_SUPPORTED)));
    assertThat(filter.equals(filter.getCopy()), is(true));
    assertThat(filter.map(null).isEmpty(), is(true));
    assertThat(filter.map(new ArrayList<>()).isEmpty(), is(true));
    assertThat(filter.filter(new ArrayList<>()).size(), is(equalTo(0)));
    assertThat(filter.getCopy(), is(equalTo(filter)));
    assertThat(filter.getCopy(), is(equalTo(EMPTY_FILTER)));
  }
}
