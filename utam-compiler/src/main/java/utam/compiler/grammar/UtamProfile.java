/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.grammar.UtamProfile.Deserializer;
import utam.compiler.helpers.TranslationContext;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.Profile;

/**
 * Profile marks a Page Object to be applicable to a profile. Assigned profile can be one or
 * multiple as an array, ex: "platform":"android" or "device": ["phone", "tablet"]
 *
 * @author elizaveta.ivanova
 * @since 228
 */
@JsonDeserialize(using = Deserializer.class)
final class UtamProfile {

  static final String ERR_INVALID_ARRAY_TYPES =
      "Profile '%s': array of profile values must contain only string values";
  static final String ERR_DUPLICATE_PROFILE_VALUE =
      "Profile '%s': value '%s' already added";
  static final String ERR_PROFILE_VALUE_WRONG_FORMAT =
      "Profile '%s': value can only be string or string array";

  private final String name;
  private final List<String> values;

  UtamProfile(String name, String value) {
    this.name = name;
    this.values = Collections.singletonList(value);
  }

  UtamProfile(String name, List<String> values) {
    this.name = name;
    this.values = values;
  }

  static List<Profile> getPageObjectProfiles(UtamProfile[] profiles, TranslationContext context) {
    if (profiles == null) {
      return new ArrayList<>();
    }
    return Stream.of(profiles)
        .flatMap(profile -> profile.getProfiles(context).stream())
        .collect(Collectors.toList());
  }

  List<Profile> getProfiles(TranslationContext translationContext) {
    return values
        .stream()
        .map(value -> translationContext.getProfile(name, value))
        .collect(Collectors.toList());
  }

  static class Deserializer extends
      com.fasterxml.jackson.databind.JsonDeserializer<UtamProfile> {

    @Override
    public UtamProfile deserialize(JsonParser jp, DeserializationContext deserializationContext)
        throws IOException {
      JsonNode node = jp.getCodec().readTree(jp);
      String key = node.fieldNames().next();
      JsonNode valuesNode = node.get(key);
      if (valuesNode.isArray()) {
        List<String> values = new ArrayList<>();
        for (JsonNode valueNode : valuesNode) {
          if (!valueNode.isTextual()) {
            throw new UtamError(String.format(ERR_INVALID_ARRAY_TYPES, key));
          }
          String textValue = valueNode.textValue();
          if (values.contains(textValue)) {
            throw new UtamError(String.format(ERR_DUPLICATE_PROFILE_VALUE, key, textValue));
          }
          values.add(textValue);
        }
        return new UtamProfile(key, values);
      } else if (valuesNode.isTextual()) {
        return new UtamProfile(key, valuesNode.asText());
      } else {
        throw new UtamError(String.format(ERR_PROFILE_VALUE_WRONG_FORMAT, key));
      }
    }
  }
}
