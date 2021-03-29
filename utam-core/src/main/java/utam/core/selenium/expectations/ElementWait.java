package utam.core.selenium.expectations;

/**
 * different types of element wait for UI interactions
 *
 * @author elizaveta.ivanova
 * @since 220
 */
public interface ElementWait {

  <T> T wait(ElementExpectations<T> expectation);

  <T> T wait(ElementListExpectations<T> expectations);

  <T> T apply(ElementExpectations<T> expectation);

  <T> T apply(ElementListExpectations<T> expectations);

  boolean match(ElementExpectations<Match> expectation);

  boolean match(ElementListExpectations<Match> expectation);

  /**
   * Selenium fluentWait does not allow to return false, so this enum is a wrapper for boolean
   * methods to be able to return false instead throwing error
   *
   * @author elizaveta.ivanova
   */
  enum Match {
    TRUE,
    FALSE;

    public static Match from(boolean is) {
      return is ? TRUE : FALSE;
    }

    public boolean is() {
      return this == TRUE;
    }
  }
}
