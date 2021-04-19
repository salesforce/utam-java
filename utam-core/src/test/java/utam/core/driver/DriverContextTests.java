package utam.core.driver;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyString;
import static utam.core.driver.DriverContext.DEFAULT;
import static utam.core.driver.DriverContext.TEST;

import org.testng.annotations.Test;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class DriverContextTests {

  @Test
  public void testGetTimeouts() {
    assertThat(DEFAULT.getTimeouts(), is(equalTo(DriverTimeouts.DEFAULT)));
    assertThat(TEST.getTimeouts(), is(equalTo(DriverTimeouts.TEST)));
  }

  @Test
  public void testGetBridgeAppTitle() {
    assertThat(DEFAULT.getBridgeAppTitle(), is(emptyString()));
    assertThat(TEST.getBridgeAppTitle(), is(emptyString()));
  }
}