/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.JsonDeserializer.isEmptyNode;
import static utam.compiler.grammar.JsonDeserializer.isNotArrayOrEmptyArray;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.framework.context.Profile;

/**
 * Profile marks a Page Object to be applicable to a profile. Assigned profile can be one or
 * multiple as an array, ex: "platform":"android" or "device": ["phone", "tablet"]
 *
 * @author elizaveta.ivanova
 * @since 228
 */
final class UtamProfile {

  private final String profileName;
  private final Set<String> values;

  /**
   * instance of the profile
   *
   * @param profileName name of the profile
   * @param values      profile values
   */
  private UtamProfile(String profileName, Set<String> values) {
    this.profileName = profileName;
    this.values = values;
  }

  /**
   * instance of the profile
   *
   * @param node        profiles node
   * @param profileName name of the profile
   * @param context     compilation context
   * @return instance of UtamProfile
   */
  private static UtamProfile processProfileNode(JsonNode node, String profileName,
      TranslationContext context) {
    Set<String> values = new HashSet<>();
    JsonNode valuesNode = node.get(profileName);
    String propertyName = String.format("profile \"%s\"", profileName);
    if (valuesNode.isArray()) {
      for (JsonNode valueNode : valuesNode) {
        if (!valueNode.isTextual() || valueNode.textValue().isEmpty()) {
          throw new UtamCompilationError(valueNode,
              context.getErrorMessage(11, propertyName, valuesNode.toPrettyString()));
        }
        String profileValue = valueNode.textValue();
        if (values.contains(profileValue)) {
          throw new UtamCompilationError(valueNode,
              context.getErrorMessage(802, profileName, profileValue));
        }
        values.add(profileValue);
      }
    } else if (valuesNode.isTextual()) {
      String profileValue = valuesNode.textValue();
      values.add(profileValue);
    } else {
      throw new UtamCompilationError(valuesNode,
          context.getErrorMessage(806, profileName));
    }
    return new UtamProfile(profileName, values);
  }

  /**
   * get list of profiles from the page object
   *
   * @param context compiler context
   * @return list of profiles
   */
  private List<Profile> getProfiles(TranslationContext context, JsonNode jsonNode) {
    List<Profile> profiles = new ArrayList<>();
    ProfileConfiguration configuration = context.getConfiguredProfile(profileName);
    if (configuration == null) {
      throw new UtamCompilationError(jsonNode, context.getErrorMessage(804, profileName));
    }
    values.forEach(profileValue -> {
      Profile profile = configuration.getFromString(profileValue);
      if (profile == null) {
        throw new UtamCompilationError(jsonNode,
            context.getErrorMessage(803, profileName, profileValue));
      }
      profiles.add(profile);
    });
    return profiles;
  }

  /**
   * Helper class that provides list of compiled profiles
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static class UtamProfileProvider {

    final JsonNode node;

    /**
     * deserialize profiles node
     *
     * @param node Json source
     */
    UtamProfileProvider(JsonNode node) {
      this.node = node;
    }

    /**
     * check if profiles are set
     *
     * @return true if set, used in PO validations
     */
    boolean isNotEmpty() {
      return !isEmptyNode(node);
    }

    /**
     * deserialize profiles node
     *
     * @param context translation context
     * @return list of Json objects
     */
    List<Profile> getProfiles(TranslationContext context) {
      // profiles can be empty for default implementation
      if (isEmptyNode(node)) {
        return new ArrayList<>();
      }
      if (isNotArrayOrEmptyArray(node)) {
        String message = context.getErrorMessage(12, "page object root", "profile");
        throw new UtamCompilationError(node, message);
      }
      Set<String> profilesNames = new HashSet<>();
      List<Profile> profiles = new ArrayList<>();
      for (JsonNode node : node) {
        String profileName = node.fieldNames().next();
        if (profilesNames.contains(profileName)) {
          throw new UtamCompilationError(this.node, context.getErrorMessage(801, profileName));
        }
        profilesNames.add(profileName);
        UtamProfile utamProfile = processProfileNode(node, profileName, context);
        profiles.addAll(utamProfile.getProfiles(context, node));
      }
      return profiles;
    }
  }
}
