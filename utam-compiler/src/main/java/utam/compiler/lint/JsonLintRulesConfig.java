/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.CollectionType;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import utam.core.declarative.lint.LintingError.ViolationLevel;

/**
 * Reads configured default linting rules from JSON file in resources (rules.config.json), same as
 * in utam-js
 *
 * @author elizaveta.ivanova
 * @since 246
 */
class JsonLintRulesConfig {

  private static final String RULES_CONFIG = "rules.config.json";
  static final String ERR_FINDING_LINT_RULES_CONFIG = "can't find lint rules config '%s'";
  static final String ERR_READING_LINT_RULES_CONFIG =
      "error while reading lint rules codes config '%s': ";
  static final String ERR_LINT_RULE_NOT_CONFIGURED = "error code %s is not configured";
  private static final Map<String, LintingRuleObject> CONFIGURED_LINT_RULES =
      getConfiguredLintRules();

  private final Map<String, LintingRuleObject> lintingRulesMap = new HashMap<>();

  static LintingRuleObject getConfiguredRule(String ruleId) {
    if (!CONFIGURED_LINT_RULES.containsKey(ruleId)) {
      throw new IllegalArgumentException(String.format(ERR_LINT_RULE_NOT_CONFIGURED, ruleId));
    }
    return CONFIGURED_LINT_RULES.get(ruleId);
  }

  static Map<String, LintingRuleObject> getConfiguredLintRules() {
    URL configPath = JsonLintRulesConfig.class.getClassLoader().getResource(RULES_CONFIG);
    if (configPath == null) {
      throw new IllegalArgumentException(
          String.format(ERR_FINDING_LINT_RULES_CONFIG, RULES_CONFIG));
    }
    ObjectMapper mapper = new ObjectMapper();
    try {
      CollectionType javaType =
          mapper.getTypeFactory().constructCollectionType(List.class, LintingRuleObject.class);
      List<LintingRuleObject> errors = mapper.readValue(configPath, javaType);
      JsonLintRulesConfig config = new JsonLintRulesConfig();
      errors.forEach(rule -> config.lintingRulesMap.put(rule.ruleId, rule));
      return config.lintingRulesMap;
    } catch (Exception e) {
      throw new IllegalStateException(
          String.format(ERR_READING_LINT_RULES_CONFIG, RULES_CONFIG), e);
    }
  }

  static class LintingRuleObject {
    final String ruleId;
    final int errorCode;
    final String name;
    final String description;
    final String help;
    final ViolationLevel violation;

    @JsonCreator
    LintingRuleObject(
        @JsonProperty(value = "ruleId", required = true) String ruleId,
        @JsonProperty(value = "errorCode", required = true) int errorCode,
        @JsonProperty(value = "name", required = true) String name,
        @JsonProperty(value = "description", required = true) String description,
        @JsonProperty(value = "help", required = true) String help,
        @JsonProperty(value = "violation", required = true) ViolationLevel violation) {
      this.ruleId = ruleId;
      this.errorCode = errorCode;
      this.name = name;
      this.description = description;
      this.help = help;
      this.violation = violation;
    }
  }

  static class LintRuleOverride {
    final Set<String> exceptions;
    final ViolationLevel violationLevel;
    final AdditionalLintRuleOverrideConfigValues additionalConfig;

    @JsonCreator
    LintRuleOverride(
        @JsonProperty(value = "violation") ViolationLevel violationLevel,
        @JsonProperty(value = "exclude") Set<String> exceptions,
        @JsonProperty(value = "additionalConfiguration") JsonNode additionalConfig) {
      this.exceptions = Objects.requireNonNullElse(exceptions, new HashSet<>());
      this.violationLevel = Objects.requireNonNullElse(violationLevel, ViolationLevel.warning);
      this.additionalConfig =
          additionalConfig != null
              ? new AdditionalLintRuleOverrideConfigValues(additionalConfig)
              : null;
    }
  }

  static class AdditionalLintRuleOverrideConfigValues {
    final Map<String, Object> additionalConfigValues;

    AdditionalLintRuleOverrideConfigValues(JsonNode node) {
      this.additionalConfigValues =
          new ObjectMapper().convertValue(node, new TypeReference<HashMap<String, Object>>() {});
    }

    Object getAdditionalConfigValue(String configValueName) {
      return this.additionalConfigValues.getOrDefault(configValueName, null);
    }
  }
}
