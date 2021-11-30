package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.testng.Assert.expectThrows;
import static utam.compiler.helpers.ReturnType.ERR_RETURN_ALL_REDUNDANT;
import static utam.compiler.helpers.ReturnType.ERR_UNSUPPORTED_RETURN_TYPE;

import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.core.framework.consumer.UtamError;

/**
 * @author elizaveta.ivanova
 * @since 236
 */
public class ReturnTypeTests {

  private final static String validationContextStr = "method 'test'";

  private static void test(String jsonFile, String expectedError) {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getContext("validate/return/" + jsonFile));
    assertThat(e.getMessage(), containsString(expectedError));
  }

  private static String getUnsupportedTypeError(String json) {
    return String.format(ERR_UNSUPPORTED_RETURN_TYPE, validationContextStr, json);
  }

  @Test
  public void testReturnThrowsForObject() {
    String expectedErr = getUnsupportedTypeError("{ }");
    test("returnObject", expectedErr);
  }

  @Test
  public void testReturnThrowsForArray() {
    String expectedErr = getUnsupportedTypeError("[ \"frame\" ]");
    test("returnArray", expectedErr);
  }

  @Test
  public void testReturnAllRedundant() {
    String err = String.format(ERR_RETURN_ALL_REDUNDANT, validationContextStr);
    test("returnAllRedundant", err);
  }

  @Test
  public void testReturnStringUnsupported() {
    String expectedErr = getUnsupportedTypeError("\"container\"");
    test("returnIncorrectString", expectedErr);
  }
}
