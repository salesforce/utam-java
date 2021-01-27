package utam.core.selenium.expectations;

import org.testng.annotations.Test;
import utam.core.selenium.expectations.ElementWait.Match;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

/**
 * Tests the selenium.expectation.Match enum convenience methods
 *
 * @author james.evans
 */
public class MatchEnumTests {

  /**
   * Tests that Match values can be translated to boolean primitives via the enum's is() static
   * method
   */
  @Test
  public void testTranslateToBooleanPrimitive() {
    assertThat(Match.TRUE.is(), is(equalTo(true)));
    assertThat(Match.FALSE.is(), is(equalTo(false)));
  }

  /** Tests that Match values can be generated from boolean primitives */
  @Test
  public void testTranslateFromBooleanPrimitive() {
    assertThat(Match.from(true), is(equalTo(Match.TRUE)));
    assertThat(Match.from(false), is(equalTo(Match.FALSE)));
  }
}
