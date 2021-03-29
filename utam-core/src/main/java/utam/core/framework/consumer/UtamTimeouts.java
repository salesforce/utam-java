package utam.core.framework.consumer;

import java.time.Duration;

/**
 * UTAM supports various timeouts for UI interactions
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface UtamTimeouts {

  /**
   * this type of timeout is used to find an element
   *
   * @return duration units
   */
  Duration getFindTimeout();

  /**
   * this type of timeout is used for all wait operations, it is NOT applied to interactions like
   * click or getAttribute or setText
   *
   * @return duration units
   */
  Duration getWaitForTimeout();

  /**
   *
   * @return duration units
   */
  Duration getPollingInterval();

  /**
   * applied for all interactions that do not wait, like click
   *
   * @return duration units
   * @deprecated should be removed when behavior transition with UTAM-JS is complete
   */
  @Deprecated
  Duration getFluentWaitTimeout();

  void setFindTimeout(Duration findTimeout);

  void setWaitForTimeout(Duration waitForTimeout);

  void setWaitForPollingInterval(Duration pollingInterval);
}
