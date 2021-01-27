package utam.compiler.grammar;

import utam.compiler.helpers.PrimitiveType;
import declarative.representation.MethodParameter;
import org.testng.annotations.Test;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

/**
 * Provides deserialization tests for the UtamSelector class
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamSelector_DeserializeTests {

  /**
   * A UtamSelector object should be able to be created through deserialization
   */
  @Test
  public void testDeserialization() {
    String json = "{\n"
        + "  \"css\": \".fakeSelector\"\n"
        + "}\n";
    UtamSelector selector = TestUtilities.getDeserializedObject(json, UtamSelector.class);
    assertThat(selector.getSelector().getValue(), is(equalTo(".fakeSelector")));
    assertThat(selector.isReturnAll, is(equalTo(false)));
    assertThat(selector.args, is(nullValue()));
  }

  @Test
  public void testDeserializationArgs() {
    String json =
        "{"
            + "  \"css\": \"stringArgSelector[%s]\","
            + "    \"args\": [ {\"name\": \"text\", \"type\":\"string\" }]"
            + "}";
    UtamSelector instance = TestUtilities.getDeserializedObject(json, UtamSelector.class);
    assertThat(instance, is(not(nullValue())));
  }

  @Test
  public void testValidSimpleSelector() {
    String json = "{" + "  \"css\": \"simpleSelector\"" + "}";
    UtamSelector instance = TestUtilities.getDeserializedObject(json, UtamSelector.class);
    assertThat(instance.isReturnAll, is(equalTo(false)));
    assertThat(instance.getSelector().getValue(), is(equalTo("simpleSelector")));
    assertThat(instance.getParameters("test"), is(empty()));
  }

  @Test
  public void testValidSimpleListSelector() {
    String json = "{"
            + "  \"css\": \"simpleListSelector\","
            + "  \"returnAll\": true"
            + "}";
    UtamSelector node = TestUtilities.getDeserializedObject(json, UtamSelector.class);
    assertThat(node.isReturnAll, is(equalTo(true)));
    assertThat(node.getSelector().getValue(), is(equalTo("simpleListSelector")));
    assertThat(node.getParameters("test"), is(empty()));
  }

  @Test
  public void testStringArgSelector() {
    String json =
        "{"
            + "  \"css\": \"stringArgSelector[%s]\","
            + "    \"args\": [ {\"name\": \"text\", \"type\":\"string\" }]"
            + "}";
    UtamSelector node = TestUtilities.getDeserializedObject(json, UtamSelector.class);
    List<MethodParameter> parameters = node.getParameters("test");
    assertThat(node.isReturnAll, is(equalTo(false)));
    assertThat(node.getSelector().getValue(), is(equalTo("stringArgSelector[%s]")));
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
  }

  @Test
  public void testIntegerArgSelector() {
    String json =
        "{"
            + "  \"css\": \"integerArgSelector[%d]\","
            + "    \"args\": [ {\"name\": \"intArg\", \"type\":\"number\" }]"
            + "}";
    UtamSelector node = TestUtilities.getDeserializedObject(json, UtamSelector.class);
    List<MethodParameter> parameters = node.getParameters("test");
    assertThat(node.isReturnAll, is(equalTo(false)));
    assertThat(node.getSelector().getValue(), is(equalTo("integerArgSelector[%d]")));
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.NUMBER)));
  }
}
