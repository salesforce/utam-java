package utam.core.appium.element;

import io.appium.java_client.MobileBy;
import org.testng.annotations.Test;
import utam.core.selenium.element.LocatorNodeImpl;
import utam.core.selenium.element.LocatorParameters;
import utam.core.selenium.element.Selector;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

/**
 * iOS Class Chain selector based locator tests
 *
 * @author Qingchun Ren
 * @since 228
 */
public class ClassChainTests {

  /** An ElementLocatorClassChain object should be able to be created */
  @Test
  public void testElementLocatorClassChainCreation() {
    String selector = "fakeSelector";
    LocatorNodeImpl locator = new ClassChain(selector);
    assertThat(locator.by(), is(equalTo(MobileBy.iOSClassChain(selector))));
    assertThat(locator.getSelectorString(), is(equalTo(selector)));
    assertThat(locator.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  /** The applyParameters method should modify the CSS selector string */
  @Test
  public void testApplyParameters() {
    String selector = ".fakeSelector::nth-of-type(%d)";
    LocatorNodeImpl filteredLocator = new ClassChain(selector);
    assertThat(filteredLocator.by(), is(equalTo(MobileBy.iOSClassChain(selector))));
    assertThat(filteredLocator.getSelectorString(), is(equalTo(selector)));
    String filteredSelector = String.format(selector, 1);
    filteredLocator.setParameters(new LocatorParameters(1));
    assertThat(filteredLocator.by(), is(equalTo(MobileBy.iOSClassChain(filteredSelector))));
    assertThat(filteredLocator.getSelectorString(), is(equalTo(filteredSelector)));
  }

  /**
   * The getSelfCopy method should return a new copy of the ElementLocatorClassChain object, with
   * the original, unmodified selector, even if parameters have been applied
   */
  @Test
  public void testGetSelfCopy() {
    String selector = "fakeSelector[`name == '%d'`]";
    ClassChain filteredLocator = new ClassChain(selector);
    filteredLocator.setParameters(new LocatorParameters(1));
    LocatorNodeImpl copy = filteredLocator.getCopy();
    assertThat(copy.by(), is(equalTo(MobileBy.iOSClassChain("fakeSelector[`name == '1'`]"))));
    assertThat(copy.getSelectorString(), is(equalTo("fakeSelector[`name == '1'`]")));
    assertThat(copy.getSelector().getValue(), is(equalTo(selector)));
  }
}
