/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static utam.compiler.lint.LintingConfigJson.DEFAULT_LINTING_CONFIG;
import static utam.compiler.lint.LintingConfigJson.DEFAULT_THROWS_ERROR;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.lint.LintingRule.RequiredMethodDescription;
import utam.compiler.lint.LintingRule.RequiredRootDescription;
import utam.compiler.lint.LintingRule.SingleShadowBoundaryAllowed;
import utam.compiler.lint.LintingRule.UniqueSelectorInsidePageObject;
import utam.compiler.lint.LintingRule.ViolationType;
import utam.core.declarative.lint.LintingConfig;
import utam.core.declarative.lint.LintingContext;
import utam.core.declarative.lint.LintingError;

/**
 * Test linting functionality
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public class LintingRuleTests {

  public static final LintingConfig TEST_DEFAULT_LINTING_CONFIG = DEFAULT_LINTING_CONFIG;

  private static List<LintingError> test(String[] jsonFiles, LintingConfig linting) {
    LintingContext context = linting.start();
    for (String jsonFile : jsonFiles) {
      TranslationContext translationContext = new DeserializerUtilities("test/" + jsonFile)
          .getContext(jsonFile);
      linting.lint(context, translationContext.getLintingObject());
    }
    return linting.finish(context);
  }

  private static List<LintingError> test(String jsonFile, LintingConfig linting) {
    String[] files = new String[]{jsonFile};
    return test(files, linting);
  }

  @Test
  public void testDefaultConfigForOneFile() {
    List<LintingError> errors = test("lint/defaultConfig", TEST_DEFAULT_LINTING_CONFIG);
    assertThat(errors, hasSize(5));
    assertThat(errors.get(0).getMessage(), containsString(
        "linting error 2001: page object test/lint/defaultConfig: "
            + "duplicate selector \"By.cssSelector: :scope > *:first-child\" for the elements \"container2\" and \"container1\""));
    assertThat(errors.get(1).getMessage(), containsString(
        "linting error 2001: page object test/lint/defaultConfig: "
            + "duplicate selector \"By.cssSelector: .two\" for the elements \"three\" and \"two\""));
    assertThat(errors.get(2).getMessage(), containsString(
        "linting warning 2002: page object test/lint/defaultConfig: root description is missing"));
    assertThat(errors.get(3).getMessage(), containsString(
        "linting warning 2003: page object test/lint/defaultConfig: method \"nodescription\" does not have description"));
    assertThat(errors.get(4).getMessage(), containsString(
        "linting error 2004: page object test/lint/defaultConfig: "
            + "only root shadow boundary is allowed, please create another page object for the element \"three\""));
  }

  @Test
  public void testElementDescriptionCanBeEmpty() {
    List<LintingError> errors = test("lint/elementDescription", TEST_DEFAULT_LINTING_CONFIG);
    assertThat(errors, hasSize(0));
  }

  @Test
  public void testAuthorCantBeEmpty() {
    List<LintingError> errors = test("lint/rootNoAuthor", TEST_DEFAULT_LINTING_CONFIG);
    assertThat(errors, hasSize(1));
    assertThat(errors.get(0).getMessage(),
        containsString("linting warning 2005: page object test/lint/rootNoAuthor: "
            + "property \"author\" is missing in the root description"));
  }

  @Test
  public void testRulesAppliedToInterface() {
    List<LintingError> errors = test("lint/interface", TEST_DEFAULT_LINTING_CONFIG);
    assertThat(errors, hasSize(2));
  }

  @Test
  public void testExceptionsAreApplied() {
    Set<String> exceptions = Collections.singleton("test/lint/defaultConfig");
    LintingConfig configuration = new LintingConfigJson(
        DEFAULT_THROWS_ERROR,
        new UniqueSelectorInsidePageObject(ViolationType.warning, exceptions),
        new RequiredRootDescription(ViolationType.warning, exceptions),
        null,
        new RequiredMethodDescription(ViolationType.warning, exceptions),
        new SingleShadowBoundaryAllowed(ViolationType.warning, exceptions),
        null,
        null,
        null
    );
    List<LintingError> errors = test("lint/defaultConfig", configuration);
    assertThat(errors, hasSize(0));
  }


  @Test
  public void testMultipleFilesWithDefaultConfig() {
    String[] files = new String[]{
        "lint/hasDifferentRootSelector",
        "lint/hasRootSelector",
        "lint/hasSameRootSelector",
        "lint/hasAnotherSameRootSelector"
    };
    List<LintingError> errors = test(files, TEST_DEFAULT_LINTING_CONFIG);
    assertThat(errors, hasSize(5));
    assertThat(errors.get(0).getMessage(), containsString(
        "linting error 3001: page object test/lint/hasRootSelector: "
            + "same root selector \"By.cssSelector: root\" is used as a root selector in the page object test/lint/hasSameRootSelector"));
    assertThat(errors.get(1).getMessage(), containsString(
        "linting error 3001: page object test/lint/hasSameRootSelector: "
            + "same root selector \"By.cssSelector: root\" is used as a root selector in the page object test/lint/hasAnotherSameRootSelector"));
    assertThat(errors.get(2).getMessage(), containsString(
        "linting error 3002: page object test/lint/hasDifferentRootSelector: "
            + "element \"sameAsRootBasic\" should have type \"test.lint.HasRootSelector\" because it uses its root selector"));
    assertThat(errors.get(3).getMessage(), containsString(
        "linting error 3003: page object test/lint/hasRootSelector: "
            + "custom selector \"By.cssSelector: custom-duplicate\" of the element \"custom\" is used for an element \"basic\" in the page object test/lint/hasSameRootSelector, but has a different type"));
    assertThat(errors.get(4).getMessage(), containsString(
        "linting error 3003: page object test/lint/hasRootSelector: "
            + "custom selector \"By.cssSelector: custom-duplicate\" of the element \"custom\" is used for an element \"customDuplicate\" in the page object test/lint/hasAnotherSameRootSelector, but has a different type"));

  }
}
