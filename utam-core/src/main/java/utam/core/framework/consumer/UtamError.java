package utam.core.framework.consumer;

/**
 * TEMPORARY LOCATION TO BE REFACTORED
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamError extends RuntimeException {

  /**
   * Default serial version ID for serializable object
   */
  private static final long serialVersionUID = 1L;

  public UtamError(String message) {
    super(message);
  }

  public UtamError(String message, Throwable cause) {
    super(message, cause);
  }
}
