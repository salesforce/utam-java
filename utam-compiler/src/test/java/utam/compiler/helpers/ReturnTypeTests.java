package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.testng.Assert.expectThrows;

import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.DeserializerUtilities;
import utam.core.framework.consumer.UtamError;

/**
 * @author elizaveta.ivanova
 * @since 236
 */
public class ReturnTypeTests {

  private static void test(String jsonFile, String expectedError) {
    UtamError e = expectThrows(UtamCompilationError.class,
        () -> new DeserializerUtilities().getContext("validate/return/" + jsonFile));
    assertThat(e.getMessage(), containsString(expectedError));
  }

  @Test
  public void testReturnThrowsForObject() {
    test("returnObject",
        "error 10: method \"test\": property \"returnType\" should be a non empty string, instead found object");
  }

  @Test
  public void testReturnAllRedundant() {
    test("returnAllRedundant",
        "error 603: method \"test\" statement: \"returnAll\" property can't be set without setting return type in a compose statement");
  }

  @Test
  public void testReturnStringUnsupported() {
    test("returnIncorrectString",
        "error 602: method \"test\" statement: return type \"container\" is not supported in a compose statement");
  }

  @Test
  public void testReturnAllRedundantForMethodThrows() {
    test("returnMethodAllRedundant",
        "error 402: abstract method \"test\": \"returnAll\" property can't be set without setting return type");
  }

  @Test
  public void testReturnTypeNotAllowedThrows() {
    test("returnTypeMethodNotAllowed",
        "error 500: incorrect format of compose method: \nUnrecognized field \"returnType\"");
  }

  @Test
  public void testReturnStringUnsupportedForMethodThrows() {
    test("returnMethodIncorrectString",
        "error 501: method \"test\": return type \"container\" is not supported");
  }
}
