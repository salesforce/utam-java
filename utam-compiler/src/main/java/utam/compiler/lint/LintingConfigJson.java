/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import static java.util.Objects.requireNonNullElse;
import static utam.compiler.translator.DefaultTargetConfiguration.getWriterWithDir;
import static utam.core.framework.UtamLogger.info;

import com.contrastsecurity.sarif.SarifSchema210;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.lint.LintingRuleImpl.ElementsWithDifferentTypes;
import utam.compiler.lint.LintingRuleImpl.RequiredAuthor;
import utam.compiler.lint.LintingRuleImpl.RequiredMethodDescription;
import utam.compiler.lint.LintingRuleImpl.RequiredRootDescription;
import utam.compiler.lint.LintingRuleImpl.RootSelectorExistsForElement;
import utam.compiler.lint.LintingRuleImpl.SingleShadowBoundaryAllowed;
import utam.compiler.lint.LintingRuleImpl.UniqueRootSelector;
import utam.compiler.lint.LintingRuleImpl.UniqueSelectorInsidePageObject;
import utam.core.declarative.lint.LintingConfig;
import utam.core.declarative.lint.LintingContext;
import utam.core.declarative.lint.LintingError;
import utam.core.declarative.lint.LintingRule;
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
      null,
      null,
      null
  );
  private final List<LintingRuleImpl> localRules = new ArrayList<>();
  private final List<LintingRuleImpl> globalRules = new ArrayList<>();
  private final boolean isInterruptCompilation;
  private final boolean isPrintToConsole;
  private final SarifConverter sarifConverter;

  @JsonCreator
  LintingConfigJson(
      @JsonProperty(value = "throwError") Boolean interruptCompilation,
      @JsonProperty(value = "outputFile") String outputFileName,
      @JsonProperty(value = "printToConsole") Boolean isPrintToConsole,
      @JsonProperty(value = "duplicateSelectors") UniqueSelectorInsidePageObject uniqueSelectors,
      @JsonProperty(value = "requiredRootDescription") RequiredRootDescription requiredRootDescription,
      @JsonProperty(value = "requiredAuthor") RequiredAuthor requiredAuthor,
      @JsonProperty(value = "requiredMethodDescription") RequiredMethodDescription requiredMethodDescription,
      @JsonProperty(value = "requiredSingleShadowRoot") SingleShadowBoundaryAllowed singleShadowBoundaryAllowed,
      @JsonProperty(value = "duplicateRootSelectors") UniqueRootSelector uniqueRootSelectors,
      @JsonProperty(value = "elementCantHaveRootSelector") RootSelectorExistsForElement rootSelectorExists,
      @JsonProperty(value = "duplicateCustomSelectors") ElementsWithDifferentTypes customWrongType) {
    isInterruptCompilation = requireNonNullElse(interruptCompilation, DEFAULT_THROWS_ERROR);
    this.isPrintToConsole = requireNonNullElse(isPrintToConsole, true);
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
    List<LintingRule> rules = new ArrayList<>(localRules);
    rules.addAll(globalRules);
    sarifConverter = new SarifConverter(rules);
  }

  /**
   * Get default or configured linting for translator runner
   *
   * @param config JSON config, can be null
   * @return not null object
   */
  public static LintingConfig getLintingConfig(LintingConfig config) {
    return config == null ? DEFAULT_LINTING_CONFIG : config;
  }

  @Override
  public String toString() {
    String output = localRules.stream()
        .map(LintingRuleImpl::toString).collect(Collectors.joining("\n"));
    String outputGlobal = globalRules.stream()
        .map(LintingRuleImpl::toString).collect(Collectors.joining("\n"));
    return String.format("\n%s\n%s", output, outputGlobal);
  }

  @Override
  public LintingContext start() {
    return new LintingContextImpl();
  }

  @Override
  public void lint(LintingContext context, PageObjectLinting pageObjectContext) {
    for (LintingRuleImpl rule : localRules) {
      if (rule.isEnabled(pageObjectContext)) {
        rule.validate(context.getErrors(), pageObjectContext);
      }
    }
    context.addPageObject(pageObjectContext);
  }

  @Override
  public List<LintingError> finish(LintingContext context, String reportFilePath) {
    List<LintingError> errors = context.getErrors();
    for (LintingRuleImpl rule : globalRules) {
      rule.validate(errors, context);
    }
    writeSarifResults(reportFilePath, context, errors);
    reportToConsole(errors);
    return errors;
  }

  private void reportToConsole(List<LintingError> errors) {
    String warningsMsg = errors.stream()
        .filter(err -> err.getLevel() == LintingError.ViolationLevel.warning)
        .map(LintingError::getFullMessage)
        .collect(Collectors.joining(" \n"));
    String errorsMsg = errors.stream()
        .filter(err -> err.getLevel() == LintingError.ViolationLevel.error)
        .map(LintingError::getFullMessage)
        .collect(Collectors.joining(" \n"));
    // log warnings
    if (!warningsMsg.isEmpty() && isPrintToConsole) {
      UtamLogger.warning("\n" + warningsMsg);
    }
    // log or throw errors
    if (!errorsMsg.isEmpty()) {
      if (isInterruptCompilation) {
        throw new UtamLintingError(LINTING_EXCEPTION_PREFIX + errorsMsg);
      }
      if (isPrintToConsole) {
        UtamLogger.error("\n" + errorsMsg);
      }
    }
  }

  private void writeSarifResults(String reportFilePath, LintingContext context,
      List<LintingError> errors) {
    if (reportFilePath != null) { //can be null in tests
      try {
        Writer writer = getWriterWithDir(reportFilePath);
        ObjectMapper mapper = new ObjectMapper();
        DefaultPrettyPrinter formatter = new DefaultPrettyPrinter()
            .withObjectIndenter(new DefaultIndenter("  ", "\n"))
            .withArrayIndenter(new DefaultIndenter("  ", "\n"));
        SarifSchema210 sarifSchema210 = sarifConverter.convert(context, errors);
        UtamLogger.info(String.format("Write results of linting to %s", reportFilePath));
        mapper.writer(formatter).writeValue(writer, sarifSchema210);
      } catch (IOException e) {
        String err = String.format("error creating linting log %s", reportFilePath);
        throw new UtamLintingError(err, e);
      }
    }
  }
}
