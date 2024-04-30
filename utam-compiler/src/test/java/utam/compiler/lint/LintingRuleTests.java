/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static utam.compiler.lint.LintingConfigJson.DEFAULT_LINTING_CONFIG;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.translator.JsonCompilerConfig;
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
  private static final String ROOT_DIR =
      String.join(
          File.separator, System.getProperty("user.dir"), "src", "test", "resources", "lint");

  private static LintingConfig getLintingConfigFromFile(String folder, String fileName) {
    String relativeRoot = String.join(File.separator, ROOT_DIR, folder);
    String configFile = String.join(File.separator, "lint", folder, fileName);
    File config =
        new File(LintingRuleTests.class.getClassLoader().getResource(configFile).getFile());
    try {
      JsonCompilerConfig jsonCompilerConfig =
          new JsonCompilerConfig(config, new File(relativeRoot), new ArrayList<>());
      return jsonCompilerConfig.getTranslatorConfig().getLintingConfig();
    } catch (IOException e) {
      throw new AssertionError(e);
    }
  }

  private static List<LintingError> test(String[] jsonFiles) {
    return test(DEFAULT_LINTING_CONFIG, jsonFiles);
  }

  private static List<LintingError> test(LintingConfig linting, String[] jsonFiles) {
    LintingContext context = linting.start();
    for (String jsonFile : jsonFiles) {
      TranslationContext translationContext =
          new DeserializerUtilities("test/" + jsonFile).getContextWithPath(jsonFile);
      linting.lint(context, translationContext.getLintingObject());
    }
    linting.finish(context);
    linting.writeReport(context, null);
    return context.getErrors();
  }

  private static List<LintingError> test(String jsonFile) {
    String[] files = new String[] {jsonFile};
    return test(files);
  }

  @Test
  public void testDuplicateSelectorsInsideOneFile() {
    List<LintingError> errors = test("lint/rules/defaultConfig");
    assertThat(errors, hasSize(4));
    LintingError error = errors.get(0);
    assertThat(
        error.getFullMessage(),
        equalTo(
            "lint rule ULR01 failure in page object test/lint/rules/defaultConfig: warning 2001:"
                + " duplicate selector \".two\" for the elements \"three\" and \"two\"; remove"
                + " duplicate elements: \"two\" or \"three\""));
    assertThat(error.getRuleId(), equalTo("ULR01"));
    assertThat(
        error.getFixSuggestion(), equalTo("remove duplicate elements: \"two\" or \"three\""));
    assertThat(error.getSourceLine(), equalTo(21));
    error = errors.get(1);
    assertThat(
        error.getFullMessage(),
        equalTo(
            "lint rule ULR01 failure in page object test/lint/rules/defaultConfig: warning 2001:"
                + " duplicate selector \":scope > *:first-child\" for the elements \"container2\""
                + " and \"container1\"; remove duplicate elements: \"container1\" or"
                + " \"container2\""));
  }

  @Test
  public void testTwoContainersWithSameEmptySelector() {
    List<LintingError> errors = test("lint/rules/twoContainers");
    assertThat(errors, hasSize(1));
    assertThat(
        errors.get(0).getFullMessage(),
        equalTo(
            "lint rule ULR01 failure in page object test/lint/rules/twoContainers: warning 2001:"
                + " duplicate selector \":scope > *:first-child\" for the elements \"container2\""
                + " and \"container1\"; remove duplicate elements: \"container1\" or"
                + " \"container2\""));
  }

  @Test
  public void testListCanHaveSameSelector() {
    List<LintingError> errors = test("lint/rules/listSelector");
    assertThat(errors, hasSize(1));
    assertThat(
        errors.get(0).getFullMessage(),
        equalTo(
            "lint rule ULR01 failure in page object test/lint/rules/listSelector: warning 2001:"
                + " duplicate selector \"css\" for the elements \"list2\" and \"list1\"; remove"
                + " duplicate elements: \"list1\" or \"list2\""));
  }

  @Test
  public void testListCanHaveSameSelectorWithFilters() {
    List<LintingError> errors = test("lint/rules/listSelectorWithFilters");
    assertThat(errors, hasSize(1));
    assertThat(
        errors.get(0).getFullMessage(),
        equalTo(
            "lint rule ULR01 failure in page object test/lint/rules/listSelectorWithFilters:"
                + " warning 2001: duplicate selector \"css\" for the elements \"list4\" and"
                + " \"list2\"; remove duplicate elements: \"list2\" or \"list4\""));
  }

  @Test
  public void testRequiredDescription() {
    List<LintingError> errors = test("lint/rules/defaultConfig");
    assertThat(errors, hasSize(4));
    LintingError error = errors.get(2);
    assertThat(
        error.getFullMessage(),
        equalTo(
            "lint rule ULR02 failure in page object test/lint/rules/defaultConfig: "
                + "warning 2002: root description is missing; "
                + "add \"description\" property at the root"));
    assertThat(error.getRuleId(), equalTo("ULR02"));
    assertThat(error.getFixSuggestion(), equalTo("add \"description\" property at the root"));
    assertThat(error.getSourceLine(), equalTo(1));
    error = errors.get(3);
    assertThat(
        error.getFullMessage(),
        equalTo(
            "lint rule ULR04 failure in page object test/lint/rules/defaultConfig: "
                + "warning 2003: method \"nodescription\" does not have description; "
                + "add \"description\" property to the method \"nodescription\""));
    assertThat(error.getRuleId(), equalTo("ULR04"));
    assertThat(
        error.getFixSuggestion(),
        equalTo("add \"description\" property to the method \"nodescription\""));
    assertThat(error.getSourceLine(), equalTo(47));
  }

  @Test
  public void testElementDescriptionCanBeEmpty() {
    List<LintingError> errors = test("lint/rules/elementDescription");
    assertThat(errors, hasSize(0));
  }

  @Test
  public void testAuthorCantBeEmpty() {
    List<LintingError> errors = test("lint/rules/rootNoAuthor");
    assertThat(errors, hasSize(1));
    LintingError error = errors.get(0);
    assertThat(
        error.getFullMessage(),
        equalTo(
            "lint rule ULR03 failure in page object test/lint/rules/rootNoAuthor: "
                + "warning 2005: property \"author\" is missing in the root description; "
                + "add \"author\" property to the root description"));
    assertThat(error.getRuleId(), equalTo("ULR03"));
    assertThat(error.getSourceLine(), equalTo(2));
  }

  @Test
  public void testRulesAppliedToInterface() {
    List<LintingError> errors = test("lint/rules/interface");
    assertThat(errors, hasSize(2));
  }

  @Test
  public void testDuplicateRootSelectors() {
    String[] files =
        new String[] {
          "lint/rootSelectors/one", "lint/rootSelectors/two", "lint/rootSelectors/three"
        };
    List<LintingError> errors = test(files);
    assertThat(errors, hasSize(3));
    LintingError error = errors.get(0);
    assertThat(
        error.getFullMessage(),
        equalTo(
            "lint rule ULR06 failure in page object test/lint/rootSelectors/one: warning 3001: same"
                + " root selector \"root-selector\" is used as a root selector in the page object"
                + " test/lint/rootSelectors/two; remove one of the page objects with same root"
                + " selector: \"test/lint/rootSelectors/one\" or \"test/lint/rootSelectors/two\""));
    assertThat(error.getRuleId(), equalTo("ULR06"));
    assertThat(error.getSourceLine(), equalTo(9));
    assertThat(
        errors.get(1).getFullMessage(),
        equalTo(
            "lint rule ULR06 failure in page object test/lint/rootSelectors/one: warning 3001: same"
                + " root selector \"root-selector\" is used as a root selector in the page object"
                + " test/lint/rootSelectors/three; remove one of the page objects with same root"
                + " selector: \"test/lint/rootSelectors/one\" or"
                + " \"test/lint/rootSelectors/three\""));
    assertThat(
        errors.get(2).getFullMessage(),
        equalTo(
            "lint rule ULR06 failure in page object test/lint/rootSelectors/two: warning 3001: same"
                + " root selector \"root-selector\" is used as a root selector in the page object"
                + " test/lint/rootSelectors/three; remove one of the page objects with same root"
                + " selector: \"test/lint/rootSelectors/two\" or"
                + " \"test/lint/rootSelectors/three\""));
  }

  @Test
  public void testElementTypeSameAsRoot() {
    String[] files = new String[] {"lint/rootType/root", "lint/rootType/elements"};
    List<LintingError> errors = test(files);
    assertThat(errors, hasSize(1));
    assertThat(
        errors.get(0).getFullMessage(),
        equalTo(
            "lint rule ULR07 failure in page object test/lint/rootType/elements: warning 3002:"
                + " element \"customDifferentType\" should have type \"test.lint.roottype.Root\""
                + " because it uses its root selector; change the element \"customDifferentType\""
                + " type to the type of the page object \"test.lint.roottype.Root\""));
  }

  @Test
  public void testElementsOfSameTypeProduceNoError() {
    String[] files =
        new String[] {"lint/elementTypes/customType", "lint/elementTypes/anotherCustomType"};
    List<LintingError> errors = test(files);
    assertThat(errors, hasSize(0));
  }

  @Test
  public void testBasicAndCustomTypesViolation() {
    String[] files = new String[] {"lint/elementTypes/basicType", "lint/elementTypes/customType"};
    List<LintingError> errors = test(files);
    assertThat(errors, hasSize(1));
    assertThat(
        errors.get(0).getFullMessage(),
        equalTo(
            "lint rule ULR08 failure in page object test/lint/elementTypes/customType: warning"
                + " 3003: custom selector \"my-test-custom\" of the element \"test\" is used for an"
                + " element \"basic\" in the page object test/lint/elementTypes/customType, but has"
                + " a different type \"my.test.Custom\"; change the element \"test\" type to the"
                + " same type as the element \"basic\" in page object"
                + " \"test/lint/elementTypes/customType\""));
  }

  @Test
  public void testContainerAndCustomType() {
    String[] files =
        new String[] {"lint/elementTypes/customType", "lint/elementTypes/containerType"};
    List<LintingError> errors = test(files);
    assertThat(errors, hasSize(1));
    assertThat(
        errors.get(0).getFullMessage(),
        equalTo(
            "lint rule ULR08 failure in page object test/lint/elementTypes/containerType: warning"
                + " 3003: custom selector \"my-test-custom\" of the element \"container\" is used"
                + " for an element \"test\" in the page object"
                + " test/lint/elementTypes/containerType, but has a different type \"container\";"
                + " change the element \"container\" type to the same type as the element \"test\""
                + " in page object \"test/lint/elementTypes/containerType\""));
  }

  @Test
  public void testRequiredMetadata() {
    LintingConfig linting = getLintingConfigFromFile("metadata", "presence.config.json");
    String[] files = new String[] {"lint/metadata/pageWithMetadata"};
    List<LintingError> errors = test(linting, files);
    assertThat(errors, hasSize(0));
  }

  @Test
  public void testMissingMetadata() {
    LintingConfig linting = getLintingConfigFromFile("metadata", "presence.config.json");
    String[] files = new String[] {"lint/metadata/pageWithoutMetadata"};
    List<LintingError> errors = test(linting, files);
    assertThat(errors, hasSize(1));
    assertThat(
        errors.get(0).getFullMessage(),
        equalTo(
            "lint rule ULR09 failure in page object test/lint/metadata/pageWithoutMetadata: warning"
                + " 2006: property \"metadata\" is missing or misconfigured in the root; add"
                + " \"metadata\" property whose value is a non-empty object to the root"));
  }

  @Test
  public void testMetadataWithRequiredProperties() {
    LintingConfig linting = getLintingConfigFromFile("metadata", "structure.config.json");
    String[] files = new String[] {"lint/metadata/pageWithMetadata"};
    List<LintingError> errors = test(linting, files);
    assertThat(errors, hasSize(0));
  }

  @Test
  public void testMetadataWithMissingRequiredProperty() {
    LintingConfig linting = getLintingConfigFromFile("metadata", "structure.config.json");
    String[] files = new String[] {"lint/metadata/pageWithMissingMetadataProperty"};
    List<LintingError> errors = test(linting, files);
    assertThat(errors, hasSize(1));
    assertThat(
        errors.get(0).getFullMessage(),
        equalTo(
            "lint rule ULR09 failure in page object"
                + " test/lint/metadata/pageWithMissingMetadataProperty: warning 2006: property"
                + " \"metadata\" is missing or misconfigured in the root; add a property named"
                + " \"status\" to the metadata object in the page object"));
  }

  @Test
  public void testMetadataWithInvalidRequiredPropertyValue() {
    LintingConfig linting = getLintingConfigFromFile("metadata", "structure.config.json");
    String[] files = new String[] {"lint/metadata/pageWithInvalidMetadataProperty"};
    List<LintingError> errors = test(linting, files);
    assertThat(errors, hasSize(1));
    assertThat(
        errors.get(0).getFullMessage(),
        equalTo(
            "lint rule ULR09 failure in page object"
                + " test/lint/metadata/pageWithInvalidMetadataProperty: warning 2006: property"
                + " \"metadata\" is missing or misconfigured in the root; set the \"status\""
                + " metadata property to one of the following values: public, internal, private"));
  }

  @Test
  public void testMetadataWithEmptyRequiredPropertyValue() {
    LintingConfig linting = getLintingConfigFromFile("metadata", "structure.config.json");
    String[] files = new String[] {"lint/metadata/pageWithEmptyMetadataProperty"};
    List<LintingError> errors = test(linting, files);
    assertThat(errors, hasSize(1));
    assertThat(
        errors.get(0).getFullMessage(),
        equalTo(
            "lint rule ULR09 failure in page object"
                + " test/lint/metadata/pageWithEmptyMetadataProperty: warning 2006: property"
                + " \"metadata\" is missing or misconfigured in the root; set the \"scrumTeam\""
                + " metadata property a non-empty value"));
  }
}
