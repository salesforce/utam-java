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

import com.contrastsecurity.sarif.SarifSchema210;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.lint.JsonLintRulesConfig.LintRuleOverride;
import utam.compiler.lint.LintingRuleImpl.ElementsWithDifferentTypes;
import utam.compiler.lint.LintingRuleImpl.RequiredAuthor;
import utam.compiler.lint.LintingRuleImpl.RequiredMetadata;
import utam.compiler.lint.LintingRuleImpl.RequiredMethodDescription;
import utam.compiler.lint.LintingRuleImpl.RequiredRootDescription;
import utam.compiler.lint.LintingRuleImpl.RootSelectorExistsForElement;
import utam.compiler.lint.LintingRuleImpl.UniqueRootSelector;
import utam.compiler.lint.LintingRuleImpl.UniqueSelectorInsidePageObject;
import utam.core.declarative.lint.LintingConfig;
import utam.core.declarative.lint.LintingContext;
import utam.core.declarative.lint.LintingError.ViolationLevel;
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

  /** default name for output sarif report */
  private static final String DEFAULT_SARIF_OUTPUT_FILE = "utam-lint.sarif";

  /**
   * default configuration when config is empty, public because used from different package by
   * runner
   */
  public static final LintingConfig DEFAULT_LINTING_CONFIG =
      new LintingConfigJson(
          false, false, DEFAULT_SARIF_OUTPUT_FILE, null, null, null, null, null, null, null, null);

  private final List<LintingRuleImpl> localRules = new ArrayList<>();
  private final List<LintingRuleImpl> globalRules = new ArrayList<>();
  private final boolean isInterruptCompilation;
  private final boolean isDisabled;
  private final String lintingOutputFile;
  private final SarifConverter sarifConverter;

  @JsonCreator
  LintingConfigJson(
      @JsonProperty(value = "disable") Boolean isDisabled,
      @JsonProperty(value = "throwError") Boolean interruptCompilation,
      @JsonProperty(value = "lintingOutputFile") String lintingOutputFile,
      @JsonProperty(value = "duplicateSelectors") LintRuleOverride uniqueSelectors,
      @JsonProperty(value = "requiredRootDescription") LintRuleOverride requiredRootDescription,
      @JsonProperty(value = "requiredAuthor") LintRuleOverride requiredAuthor,
      @JsonProperty(value = "requiredMethodDescription") LintRuleOverride requiredMethodDescription,
      @JsonProperty(value = "duplicateRootSelectors") LintRuleOverride uniqueRootSelectors,
      @JsonProperty(value = "elementCantHaveRootSelector") LintRuleOverride rootSelectorExists,
      @JsonProperty(value = "duplicateCustomSelectors") LintRuleOverride customWrongType,
      @JsonProperty(value = "requiredMetadata") LintRuleOverride requiredMetadata) {
    this.isDisabled = requireNonNullElse(isDisabled, false);
    this.lintingOutputFile = requireNonNullElse(lintingOutputFile, DEFAULT_SARIF_OUTPUT_FILE);
    this.isInterruptCompilation = requireNonNullElse(interruptCompilation, false);
    localRules.add(new UniqueSelectorInsidePageObject(uniqueSelectors));
    localRules.add(new RequiredRootDescription(requiredRootDescription));
    localRules.add(new RequiredAuthor(requiredAuthor));
    localRules.add(new RequiredMethodDescription(requiredMethodDescription));
    localRules.add(new RequiredMetadata(requiredMetadata));
    globalRules.add(new UniqueRootSelector(uniqueRootSelectors));
    globalRules.add(new RootSelectorExistsForElement(rootSelectorExists));
    globalRules.add(new ElementsWithDifferentTypes(customWrongType));
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
    String output =
        localRules.stream().map(LintingRuleImpl::toString).collect(Collectors.joining("\n"));
    String outputGlobal =
        globalRules.stream().map(LintingRuleImpl::toString).collect(Collectors.joining("\n"));
    return String.format("\n%s\n%s", output, outputGlobal);
  }

  @Override
  public LintingContext start() {
    return new LintingContextImpl();
  }

  @Override
  public void lint(LintingContext context, PageObjectLinting pageObjectContext) {
    if (!isDisabled) {
      for (LintingRuleImpl rule : localRules) {
        if (rule.isEnabled(pageObjectContext)) {
          rule.validate(context.getErrors(), pageObjectContext);
        }
      }
      context.addPageObject(pageObjectContext);
    }
  }

  @Override
  public void finish(LintingContext context) {
    if (!isDisabled) {
      List<PageObjectLinting> all = context.getAllPageObjects();
      for (int i = 0; i < all.size(); i++) {
        PageObjectLinting first = all.get(i);
        for (int j = i + 1; j < all.size(); j++) {
          PageObjectLinting second = all.get(j);
          for (LintingRuleImpl rule : globalRules) {
            rule.validate(first, second, context);
          }
        }
      }
    }
  }

  @Override
  public void writeReport(LintingContext context, String compilerRoot) {
    if (!isDisabled) {
      String reportFilePath = getSarifFilePath(compilerRoot);
      try {
        Writer writer = getWriterWithDir(reportFilePath);
        ObjectMapper mapper = new ObjectMapper();
        DefaultPrettyPrinter formatter =
            new DefaultPrettyPrinter()
                .withObjectIndenter(new DefaultIndenter("  ", "\n"))
                .withArrayIndenter(new DefaultIndenter("  ", "\n"));
        SarifSchema210 sarifSchema210 = sarifConverter.convert(context, context.getErrors());
        UtamLogger.info(String.format("Write results of linting to %s", reportFilePath));
        mapper.writer(formatter).writeValue(writer, sarifSchema210);
      } catch (IOException e) {
        String err = String.format("error creating linting log %s", reportFilePath);
        throw new IllegalStateException(err, e);
      }
      boolean hasErrors =
          context.getErrors().stream()
              .anyMatch(lintingError -> lintingError.getLevel().equals(ViolationLevel.error));
      if (hasErrors && isInterruptCompilation) {
        throw new UtamLintingError(
            "UTAM linting failed, please check SARIF report " + this.lintingOutputFile);
      }
    }
  }

  private String getSarifFilePath(String compilerRoot) {
    String targetPath = compilerRoot == null ? System.getProperty("user.dir") : compilerRoot;
    return targetPath.endsWith(File.separator)
        ? targetPath + lintingOutputFile
        : targetPath + File.separator + lintingOutputFile;
  }
}
