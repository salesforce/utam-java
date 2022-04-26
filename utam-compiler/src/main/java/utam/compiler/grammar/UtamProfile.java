/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
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

  private final String name;
  private final List<String> values;

  private UtamProfile(String name, List<String> values) {
    this.name = name;
    this.values = values;
  }

  /**
   * check that profile is configured and transform into profile
   *
   * @param profilesNode json node with profiles, passed to print out error
   * @param context      translation context
   * @return function to get list of profiles or throw and error is profile is not configured
   */
  private Stream<Profile> getProfiles(JsonNode profilesNode, TranslationContext context) {
    return values.stream()
        .map(profileValue -> {
          ProfileConfiguration profileConfiguration = context.getConfiguredProfile(name);
          if (profileConfiguration == null) {
            throw new UtamCompilationError(profilesNode,
                context.getErrorMessage(803, name, profileValue));
          }
          return profileConfiguration.getFromString(profileValue);
        });
  }

  /**
   * provides instance of compiled profiles list
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static class UtamProfileProvider {

    private final JsonNode profilesNode;
    private final boolean isProfilesRequired;

    /**
     * deserialize profiles node
     *
     * @param profilesNode Json source
     * @param isImplOnly   true if page object has "implements" property
     */
    UtamProfileProvider(JsonNode profilesNode, boolean isImplOnly) {
      this.profilesNode = profilesNode;
      this.isProfilesRequired = isImplOnly;
    }

    /**
     * check if profiles are set
     *
     * @return true if set
     */
    boolean isProfilesSet() {
      return profilesNode != null && !profilesNode.isNull();
    }

    /**
     * translate declared profiles into configured
     *
     * @param context translation context
     * @return list of profiles
     */
    List<Profile> getProfiles(TranslationContext context) {
      return parseProfileNode(context)
          .stream()
          .flatMap(profile -> profile.getProfiles(profilesNode, context))
          .collect(Collectors.toList());
    }

    private Collection<UtamProfile> parseProfileNode(TranslationContext context) {
      if (!isProfilesSet()) {
        if (isProfilesRequired) {
          throw new UtamCompilationError(profilesNode, context.getErrorMessage(804));
        }
        return new ArrayList<>();
      }
      if (!isProfilesRequired) {
        throw new UtamCompilationError(profilesNode, context.getErrorMessage(805));
      }
      if (!profilesNode.isArray() || profilesNode.size() == 0) {
        throw new UtamCompilationError(profilesNode, context.getErrorMessage(13, "page object root", "profile"));
      }
      Map<String, UtamProfile> profiles = new HashMap<>();
      for (JsonNode node : profilesNode) {
        String profileName = node.fieldNames().next();
        JsonNode valuesNode = node.get(profileName);
        String propertyName = String.format("profile \"%s\"", profileName);
        if (profiles.containsKey(profileName)) {
          throw new UtamCompilationError(profilesNode,
              context.getErrorMessage(801, profileName));
        }
        if (valuesNode.isArray()) {
          List<String> values = new ArrayList<>();
          for (JsonNode valueNode : valuesNode) {
            if (!valueNode.isTextual() || valueNode.textValue().isEmpty()) {
              throw new UtamCompilationError(profilesNode,
                  context.getErrorMessage(11, propertyName, valuesNode.toPrettyString()));
            }
            String profileValue = valueNode.textValue();
            if (values.contains(profileValue)) {
              throw new UtamCompilationError(profilesNode,
                  context.getErrorMessage(802, profileName, profileValue));
            }
            values.add(profileValue);
          }
          profiles.put(profileName, new UtamProfile(profileName, values));
        } else if (valuesNode.isTextual()) {
          profiles.put(profileName,
              new UtamProfile(profileName, Collections.singletonList(valuesNode.textValue())));
        } else {
          throw new UtamCompilationError(profilesNode,
              context.getErrorMessage(806, profileName));
        }
      }
      return profiles.values();
    }
  }
}
