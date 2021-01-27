package utam.compiler.grammar;

import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import static utam.compiler.grammar.TestUtilities.JACKSON_MISSING_REQUIRED_PROPERTY_ERROR;
import static utam.compiler.grammar.TestUtilities.getDeserializedObject;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

/**
 * Provides deserialization tests for the UtamMethodUtil class
 *
 * @author james.evans
 */
public class UtamMethodUtil_DeserializeTests {

  /** A UtamMethodUtil object should be able to be created through deserialization */
  @Test
  public void testDeserializationDefaultValues() {
    String json =
        "{"
            + "  \"apply\": \"testUtilityMethod\","
            + "  \"type\": \"utam-test/utils/test/testUtilClass\""
            + "}";
    UtamMethodUtil method = getDeserializedObject(json, UtamMethodUtil.class);
    assertThat(method, is(not(nullValue())));
    assertThat(method.apply, is(equalTo("testUtilityMethod")));
    assertThat(method.args, is(nullValue()));
    assertThat(method.type, is(equalTo("utam-test/utils/test/testUtilClass")));
  }

  /** A UtamMethodUtil object should not deserialize without the required apply property */
  @Test
  public void testDeserializationWithoutApplyThrows() {
    String json = "{" + "  \"type\": \"utam-test/utils/test/testUtilClass\"" + "}";
    UtamError e =
        expectThrows(UtamError.class, () -> getDeserializedObject(json, UtamMethodUtil.class));
    assertThat(e.getCause().getMessage(), containsString(JACKSON_MISSING_REQUIRED_PROPERTY_ERROR));
  }

  /** A UtamMethodUtil object should not deserialize without the required type property */
  @Test
  public void testDeserializationWithoutTypeThrows() {
    String json = "{" + "  \"apply\": \"testUtilityMethod\"" + "}";
    UtamError e =
        expectThrows(UtamError.class, () -> getDeserializedObject(json, UtamMethodUtil.class));
    assertThat(e.getCause().getMessage(), containsString(JACKSON_MISSING_REQUIRED_PROPERTY_ERROR));
  }
}
