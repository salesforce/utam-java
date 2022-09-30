/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static utam.compiler.lint.LintingConfigJson.SARIF_OUTPUT_FOLDER;
import static utam.compiler.lint.LintingConfigJsonTests.getRunner;
import static utam.compiler.lint.SarifConverter.SARIF_BASE_URI;
import static utam.compiler.lint.SarifConverter.SARIF_INFORMATION_URI;
import static utam.compiler.lint.SarifConverter.SARIF_NAME;
import static utam.compiler.lint.SarifConverter.SARIF_SCHEMA;
import static utam.compiler.lint.SarifConverter.SARIF_SEMANTIC_VERSION;

import com.contrastsecurity.sarif.Artifact;
import com.contrastsecurity.sarif.ArtifactLocation;
import com.contrastsecurity.sarif.PhysicalLocation;
import com.contrastsecurity.sarif.ReportingDescriptor;
import com.contrastsecurity.sarif.Result;
import com.contrastsecurity.sarif.Result.Kind;
import com.contrastsecurity.sarif.Run;
import com.contrastsecurity.sarif.SarifSchema210;
import com.contrastsecurity.sarif.ToolComponent;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import utam.compiler.lint.LintingRuleImpl.RequiredRootDescription;
import utam.core.declarative.lint.LintingError;

/**
 * Test that SARIF file output has correct values
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public class SarifConverterTests {

  private SarifSchema210 sarifObject;

  private static void assertLocation(ArtifactLocation location) {
    assertThat(location, notNullValue());
    assertThat(location.getUriBaseId(), equalTo(SARIF_BASE_URI));
    assertThat(location.getIndex(), equalTo(0));
    assertThat(location.getUri(),
        equalTo("src/test/resources/lint/changeDefaultConfig/test.utam.json"));
  }

  @BeforeClass
  void setUp() throws IOException {
    getRunner("changeDefaultConfig").run();
    String outputFile = SARIF_OUTPUT_FOLDER + "test.sarif.json";
    File file = new File(outputFile);
    assertThat(String.format("linting SARIF output %s is missing", outputFile), file.exists(),
        is(true));
    Reader reader = new FileReader(file);
    sarifObject = new ObjectMapper().readValue(reader, SarifSchema210.class);
  }

  @Test
  public void testSarifOutputCorrectValues() {
    assertThat(sarifObject, notNullValue());

    // root properties
    assertThat(sarifObject, notNullValue());
    assertThat(sarifObject.get$schema(), equalTo(SARIF_SCHEMA));
    assertThat(sarifObject.getVersion(), equalTo(SARIF_SEMANTIC_VERSION));
    assertThat(sarifObject.getRuns(), hasSize(1));

    Run run = sarifObject.getRuns().get(0);
    assertThat(run.getTool(), notNullValue());
    assertThat(run.getTool().getDriver(), notNullValue());
    assertThat(run.getArtifacts(), hasSize(1));

    // artifacts
    Artifact artifact = run.getArtifacts().iterator().next();
    assertThat(artifact.getDescription().getText(), equalTo("page object utam/pageObjects/test"));
    assertLocation(artifact.getLocation());

    // rules info
    ToolComponent toolComponent = run.getTool().getDriver();
    assertThat(toolComponent.getName(), equalTo(SARIF_NAME));
    assertThat(toolComponent.getInformationUri(), equalTo(SARIF_INFORMATION_URI));
    assertThat(toolComponent.getSemanticVersion(), equalTo(SARIF_SEMANTIC_VERSION.value()));
    assertThat(toolComponent.getRules(), hasSize(8));

    // rule properties
    String ruleId = RequiredRootDescription.RULE_ID;
    ReportingDescriptor rule = toolComponent.getRules().stream()
        .filter(r -> r.getId().equals(ruleId)).findAny().orElse(null);
    assertThat(rule, notNullValue());
    assertThat(rule.getName(), equalTo(RequiredRootDescription.NAME));
    assertThat(rule.getShortDescription().getText(), equalTo(RequiredRootDescription.DESCRIPTION));

    // results
    assertThat(run.getResults(), hasSize(1));
    Result result = run.getResults().get(0);
    assertThat(result, notNullValue());
    assertThat(result.getRuleId(), equalTo(ruleId));
    assertThat(result.getLevel().value(), equalTo(LintingError.ViolationLevel.warning.name()));
    assertThat(result.getKind(), equalTo(Kind.FAIL));
    assertThat(result.getMessage().getId(), equalTo("2002"));
    assertThat(result.getMessage().getText(), equalTo("root description is missing"));

    // source location
    assertThat(result.getLocations(), hasSize(1));
    PhysicalLocation physicalLocation = result.getLocations().get(0).getPhysicalLocation();
    assertThat(physicalLocation, notNullValue());
    assertLocation(physicalLocation.getArtifactLocation());
  }

}
