package utam.core.selenium.element;

import org.testng.annotations.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static utam.core.selenium.element.LocatorImplTestsUtilities.SELECTOR_STRING;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class LocatorNodeImplTestsJavascript {

  static LocatorNodeImpl getJavascriptLocatorNode() {
    return new LocatorNodeImpl.Javascript(SELECTOR_STRING);
  }

  @Test
  public void testGetSelectorString() {
    assertThat(getJavascriptLocatorNode().getSelectorString(), is(equalTo(SELECTOR_STRING)));
  }

  /** The getTransformer method should return a valid value */
  @Test
  public void testGetTransformer() {
    assertThat(getJavascriptLocatorNode().getScopeTransformer(), is(equalTo(ShadowBoundary.NONE)));
  }

  /** The scope method should return a valid scoped locator value */
  @Test
  public void testSetNext() {
    LocatorNodeImpl parent = getJavascriptLocatorNode();
    LocatorNodeImpl child = getJavascriptLocatorNode();
    parent.setNext(child);
    assertThat(
        new LocatorImpl.Javascript(parent).getSelectorString(),
        is(
            equalTo(
                String.format(
                    "return arguments[0].querySelector(\"%s\").querySelectorAll(\"%s\")",
                    SELECTOR_STRING, SELECTOR_STRING))));
  }
}
