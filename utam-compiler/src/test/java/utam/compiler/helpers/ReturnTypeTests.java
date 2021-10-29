package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.testng.Assert.expectThrows;
import static utam.compiler.helpers.ReturnType.AbstractMethodReturnType.ERR_INVALID_ARRAY_VALUES;
import static utam.compiler.helpers.ReturnType.ERR_RETURN_ALL_REDUNDANT;
import static utam.compiler.helpers.ReturnType.ERR_RETURN_ALL_REDUNDANT_FOR_SELF;
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

  private static String getUnsupportedArrayTypeError(String json) {
    return String.format(ERR_INVALID_ARRAY_VALUES, validationContextStr, json);
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
  public void testReturnAllRedundantSelf() {
    String err = String.format(ERR_RETURN_ALL_REDUNDANT_FOR_SELF, validationContextStr);
    test("returnSelfAll", err);
  }

  @Test
  public void testReturnStringUnsupported() {
    String expectedErr = getUnsupportedTypeError("\"container\"");
    test("returnIncorrectString", expectedErr);
  }

  @Test
  public void testAbstractReturnArrayNonTextual() {
    String expectedErr = getUnsupportedArrayTypeError("[ 1, 2, 3 ]");
    test("abstractReturnArrayNonTextual", expectedErr);
  }

  @Test
  public void testAbstractReturnArrayEmpty() {
    String expectedErr = getUnsupportedArrayTypeError("[ ]");
    test("abstractReturnArrayEmpty", expectedErr);
  }

  @Test
  public void testAbstractReturnArrayNonBasic() {
    String expectedErr = getUnsupportedArrayTypeError("[ \"one\", \"two\" ]");
    test("abstractReturnArrayNonBasic", expectedErr);
  }
}
