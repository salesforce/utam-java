package utam.core.framework.context;

import org.testng.annotations.Test;
import utam.core.framework.consumer.UtamError;

import static org.testng.Assert.expectThrows;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

public class StringValueProfileTests {
  
  @Test
  public void testStringValueProfile() {
    StringValueProfile profile = new StringValueProfile("testName", "testValue");
    assertThat(profile.getName(), is(equalTo("testName")));
    assertThat(profile.getValue(), is(equalTo("testValue")));
    assertThat(profile.getConfigName(), is(equalTo("testName_testValue_config")));
    assertThat(profile.hashCode(), is(equalTo("testName_testValue_config".hashCode())));
  }
  
  @Test
  public void testStringValueProfileWithNullNameThrows() {
    UtamError e = expectThrows(
        UtamError.class,
        () -> new StringValueProfile(null, "testValue"));
    assertThat(
        e.getMessage(),
        containsString("profile name must not be null or the empty string"));
  }
  
  @Test
  public void testStringValueProfileWithEmptyNameThrows() {
    UtamError e = expectThrows(
        UtamError.class,
        () -> new StringValueProfile("", "testValue"));
    assertThat(
        e.getMessage(),
        containsString("profile name must not be null or the empty string"));
  }
  
  @Test
  public void testStringValueProfileWithNullValueThrows() {
    UtamError e = expectThrows(
        UtamError.class,
        () -> new StringValueProfile("testName", null));
    assertThat(
        e.getMessage(),
        containsString("profile value must not be null or the empty string"));
  }
  
  @Test
  public void testStringValueProfileWithEmptyValueThrows() {
    UtamError e = expectThrows(
        UtamError.class,
        () -> new StringValueProfile("testName", ""));
    assertThat(
        e.getMessage(), 
        containsString("profile value must not be null or the empty string"));
  }

  @Test
  public void testStringValueProfileEquality() {
    StringValueProfile profile = new StringValueProfile("testName", "testValue");
    StringValueProfile anotherProfile = new StringValueProfile("testName", "testValue");
    assertThat(profile.equals(anotherProfile), is(equalTo(true)));
  }

  @Test
  public void testStringValueProfileInequality() {
    StringValueProfile profile = new StringValueProfile("testName", "testValue");
    StringValueProfile anotherProfile = new StringValueProfile("testName", "unmatchedValue");
    assertThat(profile.equals(anotherProfile), is(equalTo(false)));
  }

  @Test
  public void testStringValueProfileInequalityWithDifferentType() {
    StringValueProfile profile = new StringValueProfile("testName", "testValue");
    assertThat(profile.equals(new Object()), is(equalTo(false)));
  }

}
