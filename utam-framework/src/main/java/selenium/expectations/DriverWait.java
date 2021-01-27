package selenium.expectations;

/**
 * @author elizaveta.ivanova
 * @since 222
 */
public interface DriverWait {

  <T> T get(DriverExpectations<T> expectation);
}
