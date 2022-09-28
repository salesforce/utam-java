/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import static java.util.Objects.requireNonNullElse;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.lint.LintingRule.ElementsWithDifferentTypes;
import utam.compiler.lint.LintingRule.RequiredAuthor;
import utam.compiler.lint.LintingRule.RequiredMethodDescription;
import utam.compiler.lint.LintingRule.RequiredRootDescription;
import utam.compiler.lint.LintingRule.RootSelectorExistsForElement;
import utam.compiler.lint.LintingRule.SingleShadowBoundaryAllowed;
import utam.compiler.lint.LintingRule.UniqueRootSelector;
import utam.compiler.lint.LintingRule.UniqueSelectorInsidePageObject;
import utam.core.declarative.lint.LintingConfig;
import utam.core.declarative.lint.LintingContext;
import utam.core.declarative.lint.LintingError;
import utam.core.declarative.lint.PageObjectLinting;
import utam.core.framework.UtamLogger;

/**
 * JSON configuration for linting
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public class LintingConfigJson implements LintingConfig {

  static final String LINTING_EXCEPTION_PREFIX = "UTAM linting failures:\n";
  static final boolean DEFAULT_THROWS_ERROR = false;

  /**
   * default configuration when config is empty
   */
  static final LintingConfig DEFAULT_LINTING_CONFIG = new LintingConfigJson(
      DEFAULT_THROWS_ERROR,
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      null
  );

  private final List<LintingRule> localRules = new ArrayList<>();
  private final List<LintingRule> globalRules = new ArrayList<>();
  private final boolean isInterruptCompilation;

  @JsonCreator
  LintingConfigJson(
      @JsonProperty(value = "throwError") Boolean interruptCompilation,
      @JsonProperty(value = "duplicateSelectors") UniqueSelectorInsidePageObject uniqueSelectors,
      @JsonProperty(value = "requiredRootDescription") RequiredRootDescription requiredRootDescription,
      @JsonProperty(value = "requiredAuthor") RequiredAuthor requiredAuthor,
      @JsonProperty(value = "requiredMethodDescription") RequiredMethodDescription requiredMethodDescription,
      @JsonProperty(value = "requiredSingleShadowRoot") SingleShadowBoundaryAllowed singleShadowBoundaryAllowed,
      @JsonProperty(value = "duplicateRootSelectors") UniqueRootSelector uniqueRootSelectors,
      @JsonProperty(value = "elementCantHaveRootSelector") RootSelectorExistsForElement rootSelectorExists,
      @JsonProperty(value = "duplicateCustomSelectors") ElementsWithDifferentTypes customWrongType) {
    isInterruptCompilation = requireNonNullElse(interruptCompilation, DEFAULT_THROWS_ERROR);
    localRules.add(requireNonNullElse(uniqueSelectors, UniqueSelectorInsidePageObject.DEFAULT));
    localRules.add(requireNonNullElse(requiredRootDescription, RequiredRootDescription.DEFAULT));
    localRules.add(requireNonNullElse(requiredAuthor, RequiredAuthor.DEFAULT));
    localRules
        .add(requireNonNullElse(requiredMethodDescription, RequiredMethodDescription.DEFAULT));
    localRules
        .add(requireNonNullElse(singleShadowBoundaryAllowed, SingleShadowBoundaryAllowed.DEFAULT));
    globalRules.add(requireNonNullElse(uniqueRootSelectors, UniqueRootSelector.DEFAULT));
    globalRules.add(requireNonNullElse(rootSelectorExists, RootSelectorExistsForElement.DEFAULT));
    globalRules.add(requireNonNullElse(customWrongType, ElementsWithDifferentTypes.DEFAULT));
  }

  /**
   * Get default or configured linting for translator runner
   *
   * @param config JSON config, can be null
   * @return not null object
   */
  public static LintingConfig getLintingConfig(LintingConfigJson config) {
    return config == null ? DEFAULT_LINTING_CONFIG : config;
  }

  @Override
  public LintingContext start() {
    return new LintingContextImpl();
  }

  @Override
  public void lint(LintingContext context, PageObjectLinting pageObjectContext) {
    for (LintingRule rule : localRules) {
      if (rule.isEnabled()) {
        rule.validate(context.getErrors(), pageObjectContext);
      }
    }
    context.addPageObject(pageObjectContext);
  }

  @Override
  public List<LintingError> finish(LintingContext context) {
    List<LintingError> errors = context.getErrors();
    for (LintingRule rule : globalRules) {
      if (rule.isEnabled()) {
        // then against all previous page objects
        rule.validate(errors, context);
      }
    }
    String warningsMsg = errors.stream()
        .filter(err -> !err.isThrowError() && err.isReport())
        .map(LintingError::getMessage)
        .collect(Collectors.joining(" \n"));
    String errorsMsg = errors.stream()
        .filter(LintingError::isThrowError)
        .map(LintingError::getMessage)
        .collect(Collectors.joining(" \n"));
    String excludedMsg = errors.stream()
        .filter(err -> !err.isReport())
        .map(LintingError::getMessage)
        .collect(Collectors.joining(" \n"));
    // for exclusions - only log to console, do not return as error
    if (!excludedMsg.isEmpty()) {
      UtamLogger.warning("\n" + excludedMsg);
      // remove from returned errors
      errors.removeIf(err -> !err.isReport());
    }
    // log warnings
    if (!warningsMsg.isEmpty()) {
      UtamLogger.warning("\n" + warningsMsg);
    }
    // log or throw errors
    if (!errorsMsg.isEmpty()) {
      if (isInterruptCompilation) {
        throw new UtamLintingError(LINTING_EXCEPTION_PREFIX + errorsMsg);
      } else {
        UtamLogger.error("\n" + errorsMsg);
      }
    }
    return errors;
  }

}
