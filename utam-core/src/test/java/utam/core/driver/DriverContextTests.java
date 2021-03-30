package utam.core.driver;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyString;
import static utam.core.driver.DriverTimeouts.DEFAULT_POLLING_INTERVAL;
import static utam.core.driver.DriverTimeouts.DEFAULT_TIMEOUT;
import static utam.core.driver.DriverTimeoutsTests.TEST_DURATION;
import static utam.core.driver.DriverTimeoutsTests.TEST_TIMEOUTS;

import org.testng.annotations.Test;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class DriverContextTests {

  private static final DriverContext DEFAULT = new DriverContext();
  private static final DriverContext TEST = new DriverContext(TEST_TIMEOUTS, "title");

  @Test
  public void testGetTimeouts() {
    assertThat(DEFAULT.getTimeouts().getFindTimeout(), is(equalTo(DEFAULT_TIMEOUT)));
    assertThat(DEFAULT.getTimeouts().getWaitForTimeout(), is(equalTo(DEFAULT_TIMEOUT)));
    assertThat(DEFAULT.getTimeouts().getFluentWaitTimeout(), is(equalTo(DEFAULT_TIMEOUT)));
    assertThat(DEFAULT.getTimeouts().getPollingInterval(), is(equalTo(DEFAULT_POLLING_INTERVAL)));
    assertThat(TEST.getTimeouts().getFindTimeout(), is(equalTo(TEST_DURATION)));
    assertThat(TEST.getTimeouts().getWaitForTimeout(), is(equalTo(TEST_DURATION)));
    assertThat(TEST.getTimeouts().getFluentWaitTimeout(), is(equalTo(TEST_DURATION)));
    assertThat(TEST.getTimeouts().getPollingInterval(), is(equalTo(TEST_DURATION)));
  }

  @Test
  public void testGetBridgeAppTitle() {
    assertThat(DEFAULT.getBridgeAppTitle(), is(emptyString()));
    assertThat(TEST.getBridgeAppTitle(), is(equalTo("title")));
  }
}