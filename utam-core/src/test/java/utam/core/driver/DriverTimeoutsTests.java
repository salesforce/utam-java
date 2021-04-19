package utam.core.driver;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static utam.core.driver.DriverTimeouts.DEFAULT;

import java.time.Duration;
import org.testng.annotations.Test;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class DriverTimeoutsTests {

  static final Duration TEST_DURATION = Duration.ofSeconds(1024);
  static final DriverTimeouts TEST_TIMEOUTS = new DriverTimeouts(TEST_DURATION, TEST_DURATION,
      TEST_DURATION);

  @Test
  void testGetFindTimeout() {
    assertThat(DEFAULT.getFindTimeout(), is(equalTo(DriverTimeouts.DEFAULT_TIMEOUT)));
    assertThat(TEST_TIMEOUTS.getFindTimeout(), is(equalTo(TEST_DURATION)));
  }

  @Test
  void testGetWaitForTimeout() {
    assertThat(DEFAULT.getFindTimeout(), is(equalTo(DriverTimeouts.DEFAULT_TIMEOUT)));
    assertThat(TEST_TIMEOUTS.getFindTimeout(), is(equalTo(TEST_DURATION)));
  }

  @Test
  void testGetPollingInterval() {
    assertThat(DEFAULT.getPollingInterval(), is(equalTo(DriverTimeouts.DEFAULT_POLLING_INTERVAL)));
    assertThat(TEST_TIMEOUTS.getPollingInterval(), is(equalTo(TEST_DURATION)));
  }

  @Test
  void testGetFluentWaitTimeout() {
    assertThat(DEFAULT.getFluentWaitTimeout(), is(equalTo(DriverTimeouts.DEFAULT_TIMEOUT)));
    assertThat(TEST_TIMEOUTS.getFluentWaitTimeout(), is(equalTo(TEST_DURATION)));
  }
}
