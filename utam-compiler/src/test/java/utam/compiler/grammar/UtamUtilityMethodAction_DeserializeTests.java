package utam.compiler.grammar;

import org.testng.annotations.Test;
import utam.core.framework.consumer.UtamError;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.JACKSON_MISSING_REQUIRED_PROPERTY_ERROR;
import static utam.compiler.grammar.TestUtilities.getDeserializedObject;

public class UtamExternalMethodAction_DeserializeTests {

    /** A UtamExternalMethodAction object should be able to be created through deserialization */
    @Test
    public void testDeserializationDefaultValues() {
        String json =
                "{"
                        + "  \"type\": \"utam-test/utils/test/testUtilClass\","
                        + "  \"invoke\": \"testUtilityMethod\""
                        + "}";
        UtamExternalMethodAction utilityMethod = getDeserializedObject(json, UtamExternalMethodAction.class);
        assertThat(utilityMethod, is(not(nullValue())));
        assertThat(utilityMethod.methodName, is(equalTo("testUtilityMethod")));
        assertThat(utilityMethod.externalClassPath, is(equalTo("utam-test/utils/test/testUtilClass")));
        assertThat(utilityMethod.args, is(nullValue()));
    }

    /** A UtamExternalMethodAction object should not deserialize without the required type property */
    @Test
    public void testDeserializationWithoutTypeThrows() {
        String json = "{" + " \"invoke\": \"testUtilityMethod\"" + "}";
        UtamError e =
                expectThrows(UtamError.class, () -> getDeserializedObject(json, UtamExternalMethodAction.class));
        assertThat(e.getCause().getMessage(), containsString(JACKSON_MISSING_REQUIRED_PROPERTY_ERROR));
    }

    /** A UtamExternalMethodAction object should not deserialize without the required invoke property */
    @Test
    public void testDeserializationWithoutInvokeThrows() {
        String json = "{" + "  \"type\": \"utam-test/utils/test/testUtilClass\"" + "}";
        UtamError e =
                expectThrows(UtamError.class, () -> getDeserializedObject(json, UtamExternalMethodAction.class));
        assertThat(e.getCause().getMessage(), containsString(JACKSON_MISSING_REQUIRED_PROPERTY_ERROR));
    }
}
