/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.grammar.JsonDeserializer.isEmptyNode;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
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
   * @param values profile values
   */
  private UtamProfile(String profileName, Set<String> values) {
    this.profileName = profileName;
    this.values = values;
  }

  static List<UtamProfile> processProfileNodes(JsonNode profilesNode) {
    List<UtamProfile> profiles = new ArrayList<>();
    VALIDATION.validateOptionalNotEmptyArray(profilesNode, "page object root", "profile");
    if (isEmptyNode(profilesNode)) {
      return profiles;
    }
    Set<String> profilesNames = new HashSet<>();
    for (JsonNode node : profilesNode) {
      Iterator<String> iterator = node.fieldNames();
      if (!iterator.hasNext()) {
        throw new UtamCompilationError(profilesNode, VALIDATION.getErrorMessage(800));
      }
      String profileName = iterator.next();
      if (profilesNames.contains(profileName)) {
        throw new UtamCompilationError(profilesNode, VALIDATION.getErrorMessage(801, profileName));
      }
      if (iterator.hasNext()) { // more fields?
        throw new UtamCompilationError(profilesNode, VALIDATION.getErrorMessage(800));
      }
      profilesNames.add(profileName);
      profiles.add(processProfileNode(node, profileName));
    }
    return profiles;
  }

  /**
   * instance of the profile
   *
   * @param node profiles node
   * @param profileName name of the profile
   * @return instance of UtamProfile
   */
  private static UtamProfile processProfileNode(JsonNode node, String profileName) {
    Set<String> values = new HashSet<>();
    JsonNode valuesNode = node.get(profileName);
    if (valuesNode.isArray()) {
      VALIDATION.validateNotEmptyArray(
          valuesNode, String.format("profile \"%s\"", profileName), "values");
      VALIDATION.validateArrayOfStrings(valuesNode, String.format("profile \"%s\"", profileName));
      for (JsonNode valueNode : valuesNode) {
        String profileValue = valueNode.textValue();
        if (values.contains(profileValue)) {
          throw new UtamCompilationError(
              valueNode, VALIDATION.getErrorMessage(802, profileName, profileValue));
        }
        values.add(profileValue);
      }
    } else if (valuesNode.isTextual()) {
      String profileValue = valuesNode.textValue();
      values.add(profileValue);
    } else {
      throw new UtamCompilationError(valuesNode, VALIDATION.getErrorMessage(806, profileName));
    }
    return new UtamProfile(profileName, values);
  }

  /**
   * get list of profiles from the page object
   *
   * @param context compiler context
   * @return list of profiles
   */
  private List<Profile> getProfiles(TranslationContext context) {
    List<Profile> profiles = new ArrayList<>();
    ProfileConfiguration configuration = context.getConfiguredProfile(profileName);
    if (configuration == null) {
      throw new UtamCompilationError(VALIDATION.getErrorMessage(804, profileName));
    }
    values.forEach(
        profileValue -> {
          Profile profile = configuration.getFromString(profileValue);
          if (profile == null) {
            throw new UtamCompilationError(
                VALIDATION.getErrorMessage(803, profileName, profileValue));
          }
          profiles.add(profile);
        });
    return profiles;
  }

  static List<Profile> getConfiguredProfiles(
      List<UtamProfile> profiles, TranslationContext context) {
    List<Profile> res = new ArrayList<>();
    for (UtamProfile profile : profiles) {
      res.addAll(profile.getProfiles(context));
    }
    return res;
  }
}
