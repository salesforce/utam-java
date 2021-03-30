package utam.core.driver;


/**
 * Driver related settings
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class DriverContext {

  private final DriverTimeouts timeouts;
  private final String bridgeAppTitle;

  public DriverContext(DriverTimeouts timeouts, String bridgeAppTitle) {
    this.timeouts = timeouts;
    this.bridgeAppTitle = bridgeAppTitle;
  }

  public DriverContext() {
    this(new DriverTimeouts(), "");
  }

  public static final DriverContext TEST = new DriverContext(DriverTimeouts.TEST, "");

  /**
   * get configured timeouts
   *
   * @return timeouts
   */
  public DriverTimeouts getTimeouts() {
    return timeouts;
  }

  /**
   * get WebView page with given title
   *
   * @return the title of the WebView page
   */
  public String getBridgeAppTitle() {
    return bridgeAppTitle;
  }
}
