package utam.core.framework.base;

import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.testng.Assert.assertThrows;

import org.testng.annotations.Test;
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageObject;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.base.ImperativeProvider;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class ImperativeProviderTests {

  @Test
  public void testInitUtils() {
    TestPage testPage = new TestPageImpl();
    assertThat(testPage.getUtils().getInstance(), is(sameInstance(testPage)));
  }

  @Test
  public void testInitUtilsErr() {
    assertThrows(UtamError.class, () -> ImperativeProvider.build(ErrorUtils.class));
  }

  interface TestPage extends PageObject {

    Utils getUtils();
  }

  static class ErrorUtils extends ImperativeProvider<TestPage> {

    ErrorUtils(String str) {
    }
  }

  static class Utils extends ImperativeProvider<TestPage> {

    Utils() {
    }
  }

  static class TestPageImpl extends BasePageObject implements TestPage {

    @Override
    public Utils getUtils() {
      return getUtility(Utils.class);
    }
  }
}
