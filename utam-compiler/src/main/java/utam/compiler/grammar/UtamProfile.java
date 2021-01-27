package utam.compiler.grammar;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import utam.compiler.helpers.TranslationContext;
import utam.core.framework.context.Profile;

import java.io.IOException;

/**
 * profile is used in root object
 * @author elizaveta.ivanova
 * @since 228
 */
class UtamProfile {

    private final String name;
    private final String value;

    UtamProfile(String name, String value) {
        this.name = name;
        this.value = value;
    }

    Profile getProfile(TranslationContext translationInstantContext) {
        return translationInstantContext.getProfile(name, value);
    }

    static class Deserializer extends StdDeserializer<UtamProfile> {

        Deserializer() {
            super(UtamProfile.class);
        }

        @Override
        public UtamProfile deserialize(JsonParser jp, DeserializationContext deserializationContext) throws IOException {
            JsonNode node = jp.getCodec().readTree(jp);
            String key = node.fieldNames().next();
            String value = node.get(key).asText();
            return new UtamProfile(key, value);
        }
    }
}
