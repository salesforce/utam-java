package selenium.expectations;

/**
 * different types of element wait for Page Object Elements actions
 *
 * @author elizaveta.ivanova
 * @since 220
 */
public interface ElementWait {

  <T> T wait(ElementExpectations<T> expectation);

  <T> T wait(ElementListExpectations<T> expectations);

  boolean match(ElementExpectations<Match> expectation);

  boolean match(ElementListExpectations<Match> expectation);

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
