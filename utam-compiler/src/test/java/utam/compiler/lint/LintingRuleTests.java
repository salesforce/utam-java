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
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static utam.compiler.lint.LintingConfigJson.DEFAULT_LINTING_CONFIG;
import static utam.compiler.lint.LintingConfigJson.DEFAULT_THROWS_ERROR;
import static utam.compiler.lint.LintingErrorImpl.buildFullErrorMessage;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.compiler.helpers.TranslationContext;
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
    String fileName = "test/lint/defaultConfig";
    assertThat(errors, hasSize(5));
    LintingError error = errors.get(0);
    assertThat(error.getFullMessage(), containsString(
        buildFullErrorMessage(fileName, error,
            "duplicate selector \"By.cssSelector: :scope > *:first-child\" for the elements \"container2\" and \"container1\"")));
    assertThat(error.getRuleId(), equalTo(UniqueSelectorInsidePageObject.RULE_ID));
    error = errors.get(1);
    assertThat(error.getMessage(), containsString(
        "duplicate selector \"By.cssSelector: .two\" for the elements \"three\" and \"two\""));
    error = errors.get(2);
    assertThat(error.getFullMessage(), containsString(
        buildFullErrorMessage(fileName, error, "root description is missing")));
    assertThat(error.getRuleId(), equalTo(RequiredRootDescription.RULE_ID));
    error = errors.get(3);
    assertThat(error.getFullMessage(), containsString(
        buildFullErrorMessage(fileName, error, "method \"nodescription\" does not have description")));
    assertThat(error.getRuleId(), equalTo(RequiredMethodDescription.RULE_ID));
    error = errors.get(4);
    assertThat(error.getFullMessage(), containsString(
        buildFullErrorMessage(fileName, error,
            "only root shadow boundary is allowed, please create another page object for the element \"three\"")));
    assertThat(error.getRuleId(), equalTo(SingleShadowBoundaryAllowed.RULE_ID));
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
    LintingError error = errors.get(0);
    assertThat(error.getFullMessage(), containsString(
        buildFullErrorMessage("test/lint/rootNoAuthor", errors.get(0),
            "property \"author\" is missing in the root description")));
    assertThat(error.getRuleId(), equalTo(RequiredAuthor.RULE_ID));
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
        null,
        null,
        new UniqueSelectorInsidePageObject(LintingError.ViolationLevel.warning, exceptions),
        new RequiredRootDescription(LintingError.ViolationLevel.warning, exceptions),
        null,
        new RequiredMethodDescription(LintingError.ViolationLevel.warning, exceptions),
        new SingleShadowBoundaryAllowed(LintingError.ViolationLevel.warning, exceptions),
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

    LintingError error = errors.get(0);
    assertThat(error.getFullMessage(), containsString(
        buildFullErrorMessage("test/lint/hasRootSelector", error,
            "same root selector \"By.cssSelector: root\" is used as a root selector in the page object test/lint/hasSameRootSelector")));
    assertThat(error.getRuleId(), equalTo(UniqueRootSelector.RULE_ID));
    assertThat(errors.get(1).getMessage(), containsString("same root selector"));

    error = errors.get(2);
    assertThat(error.getFullMessage(), containsString(
        buildFullErrorMessage("test/lint/hasDifferentRootSelector", error,
            "element \"sameAsRootBasic\" should have type \"test.lint.HasRootSelector\" because it uses its root selector")));
    assertThat(error.getRuleId(), equalTo(RootSelectorExistsForElement.RULE_ID));

    assertThat(errors.get(3).getMessage(), containsString(
        "custom selector \"By.cssSelector: custom-duplicate\" of the element \"custom\" is used for an element \"basic\" in the page object test/lint/hasSameRootSelector, but has a different type"));
    error = errors.get(4);
    assertThat(error.getFullMessage(), containsString(
        buildFullErrorMessage("test/lint/hasRootSelector", error,
            "custom selector \"By.cssSelector: custom-duplicate\" of the element \"custom\" is used for an element \"customDuplicate\" "
                + "in the page object test/lint/hasAnotherSameRootSelector, but has a different type")));
    assertThat(error.getRuleId(), equalTo(ElementsWithDifferentTypes.RULE_ID));
  }
}
