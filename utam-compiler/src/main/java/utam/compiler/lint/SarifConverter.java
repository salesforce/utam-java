/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import com.contrastsecurity.sarif.Artifact;
import com.contrastsecurity.sarif.ArtifactLocation;
import com.contrastsecurity.sarif.Fix;
import com.contrastsecurity.sarif.Location;
import com.contrastsecurity.sarif.Message;
import com.contrastsecurity.sarif.MultiformatMessageString;
import com.contrastsecurity.sarif.PhysicalLocation;
import com.contrastsecurity.sarif.Region;
import com.contrastsecurity.sarif.ReportingDescriptor;
import com.contrastsecurity.sarif.Result;
import com.contrastsecurity.sarif.Result.Kind;
import com.contrastsecurity.sarif.Result.Level;
import com.contrastsecurity.sarif.Run;
import com.contrastsecurity.sarif.SarifSchema210;
import com.contrastsecurity.sarif.SarifSchema210.Version;
import com.contrastsecurity.sarif.Tool;
import com.contrastsecurity.sarif.ToolComponent;
import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import utam.core.declarative.lint.LintingContext;
import utam.core.declarative.lint.LintingError;
import utam.core.declarative.lint.LintingError.ViolationLevel;
import utam.core.declarative.lint.LintingRule;

/**
 * Converts linting violations to SARIF format, see https://sarifweb.azurewebsites.net/
 *
 * @author elizaveta.ivanova
 * @since 242
 */
class SarifConverter {

  static final URI SARIF_SCHEMA = URI.create("https://json.schemastore.org/sarif-2.1.0.json");
  static final URI SARIF_INFORMATION_URI = URI.create("https://github.com/salesforce/utam-java");
  static final String SARIF_NAME = "UTAM";
  static final Version SARIF_SEMANTIC_VERSION = SarifSchema210.Version._2_1_0;
  static final String SARIF_BASE_URI = "%srcroot%";
  private final Set<ReportingDescriptor> rulesDescription;

  SarifConverter(List<LintingRule> rules) {
    rulesDescription = rules.stream()
        .map(SarifConverter::buildRule)
        .collect(Collectors.toSet());
  }

  private static ReportingDescriptor buildRule(LintingRule rule) {
    return new ReportingDescriptor()
        .withId(rule.getId())
        .withName(rule.getName())
        .withShortDescription(new MultiformatMessageString().withText(rule.getDescription()));
  }

  private static Level convertViolationLevel(ViolationLevel violationLevel) {
    if (violationLevel == LintingError.ViolationLevel.error) {
      return Level.ERROR;
    }
    if (violationLevel == LintingError.ViolationLevel.warning) {
      return Level.WARNING;
    }
    // never happens
    return Level.NONE;
  }

  private static Result buildResult(LintingError error) {
    ArtifactLocation artifactLocation = new ArtifactLocation()
        .withUriBaseId(SARIF_BASE_URI)
        .withIndex(0)
        .withUri(error.getSourceFilePath());
    int sourceCodeLine = error.getSourceLine();
    PhysicalLocation physicalLocation = new PhysicalLocation()
        .withArtifactLocation(artifactLocation);
    if(sourceCodeLine > 0) {
      physicalLocation.withRegion(new Region().withStartLine(sourceCodeLine));
    }
    Location location = new Location().withPhysicalLocation(physicalLocation);
    Message message = new Message()
        .withText(error.getMessage())
        .withId(error.getId());
    Fix fix = new Fix().withDescription(new Message().withText(error.getFixSuggestion()));
    return new Result()
        .withLevel(convertViolationLevel(error.getLevel()))
        .withKind(Kind.FAIL)
        .withFixes(Collections.singleton(fix))
        .withLocations(Collections.singletonList(location))
        .withRuleId(error.getRuleId())
        .withMessage(message);
  }

  private static Set<Artifact> buildArtifactsLocations(LintingContext lintingContext) {
    return lintingContext.getAllPageObjects()
        .stream()
        .map(po -> new Artifact()
            .withDescription(new Message().withText("page object " + po.getName()))
            .withLocation(new ArtifactLocation()
                .withIndex(0)
                .withUriBaseId(SARIF_BASE_URI)
                .withUri(po.getJsonFilePath())))
        .collect(Collectors.toSet());
  }

  /**
   * Convert linting errors to SARIF format
   *
   * @param lintingContext context with info about all page objects
   * @param errors         list of found linting violations
   * @return SARIF object that will be serialized to JSON
   */
  SarifSchema210 convert(LintingContext lintingContext, List<LintingError> errors) {
    List<Result> results = errors.stream()
        .map(SarifConverter::buildResult)
        .collect(Collectors.toList());
    Set<Artifact> artifacts = buildArtifactsLocations(lintingContext);
    Run run = new Run()
        .withArtifacts(artifacts)
        .withResults(results)
        .withTool(new Tool().withDriver(new ToolComponent()
            .withName(SARIF_NAME)
            .withInformationUri(SARIF_INFORMATION_URI)
            .withSemanticVersion(SARIF_SEMANTIC_VERSION.value())
            .withRules(rulesDescription)));
    return new SarifSchema210()
        .withVersion(SARIF_SEMANTIC_VERSION)
        .with$schema(SARIF_SCHEMA)
        .withRuns(List.of(run));
  }
}
