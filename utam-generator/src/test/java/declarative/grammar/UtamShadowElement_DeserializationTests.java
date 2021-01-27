package declarative.grammar;

import org.testng.annotations.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

/**
 * Provides deserialization tests for the UtamShadowElement class
 *
 * @author james.evans
 */
public class UtamShadowElement_DeserializationTests {

  /** A UtamShadowElement object should be able to be created through deserialization */
  @Test
  public void testDeserialization() {
    String json = "{  \"elements\": [] }";
    UtamShadowElement shadow = TestUtilities.getDeserializedObject(json, UtamShadowElement.class);
    assertThat(shadow, is(not(nullValue())));
    assertThat(shadow.elements, is(not(nullValue())));
    assertThat(shadow.elements.length, is(equalTo(0)));
  }
}
