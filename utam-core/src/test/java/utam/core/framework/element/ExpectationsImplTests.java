package utam.core.framework.element;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;

import org.hamcrest.Matchers;
import org.testng.annotations.Test;
import utam.core.driver.Driver;
import utam.core.driver.Expectations;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class ExpectationsImplTests {

  @Test
  public void testReturnIfFalsy() {
    Expectations<Object, Object> expectations = new ExpectationsImpl("message",
        (driver, object) -> object, true);
    assertThat(expectations.returnIfFalsy(), is(equalTo(true)));
    Driver driverMock = mock(Driver.class);
    assertThat(expectations.apply(driverMock, null), is(true));
    assertThat(expectations.apply(driverMock, false), is(true));
    assertThat(expectations.apply(driverMock, "string"), is(equalTo("string")));
  }

  @Test
  public void testGetLogMessage() {
    Expectations<Object, Object> expectations = new ExpectationsImpl("message",
        (driver, object) -> object);
    assertThat(expectations.getLogMessage(), is(equalTo("message")));
  }

  @Test
  public void testApply() {
    Expectations<Object, Object> expectations = new ExpectationsImpl("message",
        (driver, object) -> object);
    assertThat(expectations.returnIfFalsy(), is(nullValue()));
    Driver driverMock = mock(Driver.class);
    assertThat(expectations.apply(driverMock, null), is(nullValue()));
    assertThat(expectations.apply(driverMock, false), is(false));
    assertThat(expectations.apply(driverMock, "string"), is(equalTo("string")));
  }

  /**
   * Tests that Match values can be translated to boolean primitives via the enum's is() static
   * method
   */
  @Test
  public void testTranslateToBooleanPrimitive() {
    assertThat(ElementExpectations.Match.TRUE.is(), Matchers.is(Matchers.equalTo(true)));
    assertThat(ElementExpectations.Match.FALSE.is(), Matchers.is(Matchers.equalTo(false)));
  }

  /**
   * Tests that Match values can be generated from boolean primitives
   */
  @Test
  public void testTranslateFromBooleanPrimitive() {
    assertThat(ElementExpectations.Match.from(true),
        Matchers.is(Matchers.equalTo(ElementExpectations.Match.TRUE)));
    assertThat(ElementExpectations.Match.from(false),
        Matchers.is(Matchers.equalTo(ElementExpectations.Match.FALSE)));
  }
}
