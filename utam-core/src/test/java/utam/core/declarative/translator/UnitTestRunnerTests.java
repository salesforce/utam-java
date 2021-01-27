package utam.core.declarative.translator;

import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

public class UnitTestRunnerTests {

  @Test
  public void testFromStringLowerCase() {
    assertThat(UnitTestRunner.fromString("testng"), is(equalTo(UnitTestRunner.TESTNG)));
    assertThat(UnitTestRunner.fromString("junit"), is(equalTo(UnitTestRunner.JUNIT)));
    assertThat(UnitTestRunner.fromString("none"), is(equalTo(UnitTestRunner.NONE)));
  }

  @Test
  public void testFromStringUpperCase() {
    assertThat(UnitTestRunner.fromString("TESTNG"), is(equalTo(UnitTestRunner.TESTNG)));
    assertThat(UnitTestRunner.fromString("JUNIT"), is(equalTo(UnitTestRunner.JUNIT)));
    assertThat(UnitTestRunner.fromString("NONE"), is(equalTo(UnitTestRunner.NONE)));
  }

  @Test
  public void testFromStringMixedCase() {
    assertThat(UnitTestRunner.fromString("TestNG"), is(equalTo(UnitTestRunner.TESTNG)));
    assertThat(UnitTestRunner.fromString("JUnit"), is(equalTo(UnitTestRunner.JUNIT)));
    assertThat(UnitTestRunner.fromString("None"), is(equalTo(UnitTestRunner.NONE)));
  }

  @Test
  public void testFromStringWithNullOrEmptyStringThrows() {
    UtamError e = expectThrows(UtamError.class, () -> UnitTestRunner.fromString(null));
    assertThat(e.getMessage(), containsString("Illegal argument, cannot continue"));
    e = expectThrows(UtamError.class, () -> UnitTestRunner.fromString(""));
    assertThat(e.getMessage(), containsString("Illegal argument, cannot continue"));
  }

  @Test
  public void testFromStringWithUnknownValueThrows() {
    UtamError e = expectThrows(UtamError.class, () -> UnitTestRunner.fromString("unknown"));
    assertThat(
        e.getMessage(),
        containsString("No enum constant utam.core.declarative.translator.UnitTestRunner.UNKNOWN"));
  }
}
