package utam.core.driver;

/**
 * expectations act as a function parameter for waits
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface Expectations<T,R> {

  /**
   * if applied action returned falsy value (null or false), return this value,
   * if not set return null which will cause exception inside wait
   *
   * @return non null value
   */
  R returnIfFalsy();

  /**
   * provides log message when expectations are called
   *
   * @return log message to use in logs and in error
   */
  String getLogMessage();

  /**
   * apply action and return value
   *
   * @return function to apply
   */
  R apply(Driver driver, T args);
}
