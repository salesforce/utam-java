package selenium.element;

import framework.consumer.UtamError;
import org.testng.annotations.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.expectThrows;
import static selenium.element.LocatorImplTests.getParentLocator;
import static selenium.element.LocatorImplTestsUtilities.SELECTOR_STRING;
import static selenium.element.LocatorParameters.*;

/**
 * Provides tests for the LocatorParameters class
 *
 * @author james.evans
 */
public class LocatorParametersTests {

  /** A LocatorParameters object should be able to be created with string parameters */
  @Test
  public void testLocatorParametersCreationWithString() {
    LocatorParameters parameters = new LocatorParameters("string parameter");
    assertThat(parameters, is(not(nullValue())));
  }

  /** A LocatorParameters object should be able to be created with integer parameters */
  @Test
  public void testLocatorParametersCreationWithInteger() {
    LocatorParameters parameters = new LocatorParameters(1);
    assertThat(parameters, is(not(nullValue())));
  }

  /**
   * A LocatorParameters object created with an invalid parameter should throw the appropriate
   * exception
   */
  @Test
  public void testLocatorParametersCreationWithInvalidParameterThrows() {
    IllegalArgumentException e =
        expectThrows(IllegalArgumentException.class, () -> new LocatorParameters(true));
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(UNSUPPORTED_PARAMETER_TYPE, "java.lang.Boolean"))));
  }

  /**
   * The isEmpty method should return false when parameters have been added, and should return true
   * when there are no parameters
   */
  @Test
  public void testIsEmpty() {
    LocatorParameters parameters = new LocatorParameters("string parameter");
    assertThat(parameters.isEmpty(), is(equalTo(false)));

    parameters = new LocatorParameters();
    assertThat(parameters.isEmpty(), is(equalTo(true)));
  }

  /** The next method should allow getParameter to return the next parameter value */
  @Test
  public void testNext() {
    LocatorParameters parameters = new LocatorParameters("first parameter", "second parameter");
    assertThat(parameters.get(), is(equalTo("first parameter")));
    parameters.next();
    assertThat(parameters.get(), is(equalTo("second parameter")));
  }

  /**
   * The getParameter method should throw the appropriate exception when the parameter index is past
   * the end of the parameter list
   */
  @Test
  public void testGetParameterPastEndOfParameterListThrows() {
    LocatorParameters parameters = new LocatorParameters();
    parameters.next();
    IndexOutOfBoundsException e =
        expectThrows(IndexOutOfBoundsException.class, parameters::get);
    assertThat(e.getMessage(), is(equalTo(String.format(INDEX_OUT_OF_BOUNDS, 1, 0))));
  }

  /** The getParameter method should return a valid value with string parameters */
  @Test
  public void testGetParameterWithString() {
    LocatorParameters parameters = new LocatorParameters("string parameter");
    assertThat(parameters.get(), is(equalTo("string parameter")));
  }

  /** The parameter getObject method should return a valid value with string parameters */
  @Test
  public void testGetObjectWithStringParameter() {
    LocatorParameters parameters = new LocatorParameters("string parameter");
    assertThat(parameters.get(), is(equalTo("string parameter")));
  }

  /** The parameter getNumber method should throw the proper exception with string parameters */
  @Test
  public void testGetNumberWithStringParametersThrows() {
    IllegalArgumentException e =
        expectThrows(
            IllegalArgumentException.class, () -> new LocatorParameters(new Object()));
    assertThat(e.getMessage(), is(equalTo( String.format(UNSUPPORTED_PARAMETER_TYPE, "java.lang.Object"))));
  }

  /** The getParameter method should return a valid value with integer parameters */
  @Test
  public void testGetParameterWithInteger() {
    LocatorParameters parameters = new LocatorParameters(1);
    assertThat(parameters.get(), is(equalTo(1)));
  }

  /** The parameter getObject method should return a valid value with integer parameters */
  @Test
  public void testGetObjectWithIntegerParameter() {
    LocatorParameters parameters = new LocatorParameters(1);
    assertThat(parameters.get(), is(equalTo(1)));
  }

  /** The parameter getNumber method should throw the proper exception with string parameters */
  @Test
  public void testGetStringWithIntegerParametersThrows() {
    LocatorParameters parameters = new LocatorParameters(1);
    assertThat(parameters.get(), is(equalTo(1)));
  }

  /** The setParameters method should not modify locators for empty parameters */
  @Test
  public void testSetParametersWithEmptyParameters() {
    LocatorParameters parameters = new LocatorParameters();
    LocatorImpl locator = getParentLocator();
    LocatorImpl parameterLocator = parameters.setParameters(locator);
    assertThat(
        parameterLocator.getRoot().getSelectorString(),
        is(equalTo(SELECTOR_STRING)));
  }

  /** The setParameters method should modify locators with valid parameters */
  @Test
  public void testSetParameters() {
    LocatorParameters parameters = new LocatorParameters(1);
    String selector = ".fakeSelector::nth-of-type(%d)";
    String filteredSelector = String.format(selector, 1);
    LocatorImpl filteredLocator =
        parameters.setParameters(new LocatorImpl(new LocatorNodeImpl.Css(selector)));
    assertThat(filteredLocator.getRoot().getSelectorString(), is(equalTo(filteredSelector)));
  }

  /** The setParameters method should set the parameter index to an invalid value */
  @Test
  public void testSetParametersSetsIndexToInvalidValue() {
    LocatorParameters parameters = new LocatorParameters(1);
    String selector = ".fakeSelector::nth-of-type(%d)";
    parameters.setParameters(new LocatorImpl(new LocatorNodeImpl.Css(selector)));
    IndexOutOfBoundsException e =
        expectThrows(IndexOutOfBoundsException.class, parameters::get);
    assertThat(e.getMessage(), is(equalTo(String.format(INDEX_OUT_OF_BOUNDS, -1, 1))));
  }

  /**
   * The setParameters method should throw the appropriate exception when using a mismatched number
   * of parameters
   */
  @Test
  public void testSetParametersMismatchedParameterSizeThrows() {
    LocatorParameters parameters = new LocatorParameters("parameter", 2);
    LocatorImpl locator = new LocatorImpl(new LocatorNodeImpl.Css("selector"));
    UtamError e = expectThrows(UtamError.class, () -> parameters.setParameters(locator));
    assertThat(
        e.getMessage(), containsString(String.format(PARAMETERS_NUMBER_ERROR, "parameter,2", 0)));
  }

  @Test
  public void incorrectType() {
    IllegalArgumentException e =
        expectThrows(IllegalArgumentException.class, () -> new LocatorParameters(true));
    assertThat(
        e.getMessage(),
        containsString(String.format(UNSUPPORTED_PARAMETER_TYPE, "java.lang.Boolean")));
    NullPointerException npe =
        expectThrows(NullPointerException.class, () -> new LocatorParameters(1, null));
    assertThat(npe.getMessage(), containsString(NULL_PARAMETER));
    npe = expectThrows(NullPointerException.class, () -> new LocatorParameters((Object[]) null));
    assertThat(npe.getMessage(), containsString(NULL_PARAMETER));
  }

  @Test
  public void indexError() {
    LocatorParameters parameters = new LocatorParameters(1);
    parameters.next();
    IndexOutOfBoundsException e =
        expectThrows(IndexOutOfBoundsException.class, parameters::get);
    assertThat(e.getMessage(), containsString(String.format(INDEX_OUT_OF_BOUNDS, 1, 1)));
  }

  @Test
  public void setMultipleParametersString() {
    LocatorImpl testLocator =
        new LocatorImpl(new LocatorNodeImpl.Css("root"), new LocatorNodeImpl.Css("parent[%s]"));
    testLocator = new LocatorParameters("parent").setParameters(testLocator);
    assertEquals(testLocator.getSelectorString(), "root|parent[parent]");
    testLocator = testLocator.add(new LocatorNodeImpl.Css("child[%s]"));
    testLocator = new LocatorParameters("parent", "child").setParameters(testLocator);
    assertEquals(testLocator.getSelectorString(), "root|parent[parent]|child[child]");
  }

  @Test
  public void setMultipleParametersInteger() {
    LocatorImpl testLocator =
            new LocatorImpl(new LocatorNodeImpl.Css("root"), new LocatorNodeImpl.Css("parent[%d]"));
    testLocator = new LocatorParameters(1).setParameters(testLocator);
    assertEquals(testLocator.getSelectorString(), "root|parent[1]");
    testLocator = testLocator.add(new LocatorNodeImpl.Css("child[%d]"));
    testLocator = new LocatorParameters(1, 2).setParameters(testLocator);
    assertEquals(testLocator.getSelectorString(), "root|parent[1]|child[2]");
   }

  @Test
  public void testParametersCount() {
    assertThat(getAllParametersCount(""), is(equalTo(0)));
    assertThat(getAllParametersCount("testString"), is(equalTo(0)));
    assertThat(getAllParametersCount("testString[%s]"), is(equalTo(1)));
    assertThat(getAllParametersCount("%s"), is(equalTo(1)));
    assertThat(getAllParametersCount("testString[%d]"), is(equalTo(1)));
    assertThat(getAllParametersCount("testString[%f]"), is(equalTo(0)));
  }
}
