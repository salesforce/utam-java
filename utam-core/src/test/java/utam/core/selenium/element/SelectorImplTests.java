package utam.core.selenium.element;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.assertThrows;

import io.appium.java_client.MobileBy;
import org.testng.annotations.Test;
import utam.core.selenium.element.Web.SelectorImpl;

/**
 * @author elizaveta.ivanova
 * @since 232
 */
public class SelectorImplTests {

  @Test
  public void testByMethod() {
    assertThrows(IllegalArgumentException.class, () -> new SelectorImpl(null, "value").by());
    assertThat(new SelectorImpl(Selector.Type.UIAUTOMATOR, "test").by(), is(equalTo(MobileBy.AndroidUIAutomator("test"))));
  }
}
