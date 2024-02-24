/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.translator;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import utam.core.declarative.representation.TypeProvider;

/**
 * configuration of source with JSON files and target for generated Java files
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface TranslatorTargetConfig {

  /**
   * writer for generated type
   *
   * @param typeProvider type of the PO, can be interface or class
   * @return writer for class
   * @throws IOException if the file does not exist
   */
  Writer getClassWriter(TypeProvider typeProvider) throws IOException;

  /**
   * get unit test writer to write unit test generated for new page object
   *
   * @param typeProvider type of the Page Object to generate test for
   * @return writer
   * @throws IOException if file does not exist
   */
  Writer getUnitTestWriter(TypeProvider typeProvider) throws IOException;

  /**
   * get writer for PO JSON files stored as resources in the resources directory
   *
   * @param pageObjectUri URI of the page object for which to get the writer
   * @return writer for JSON source file
   * @throws IOException if the file does not exist
   */
  Writer getResourceWriter(String pageObjectUri) throws IOException;

  /**
   * get configured type for unit test generator
   *
   * @return type of the unit tests to generate
   */
  UnitTestRunner getUnitTestRunnerType();

  /**
   * full path to the injection configuration files directory <br>
   * translator will write into dependency configs <br>
   * then Page Objects Provider will read from those to configure dependencies
   *
   * @return string with full path to resources folder with configs
   */
  String getInjectionConfigRootFilePath();

  /**
   * Absolute path to put linting report
   *
   * @return string with path
   */
  String getLintReportPath();

  /**
   * Absolute path to put errors report
   *
   * @return string with path or null
   */
  String getErrorsReportPath();

  /**
   * Get path of a JSON file in the resources of the archive corresponding to a Page Object URI
   *
   * @param pageObjectUri the URI for which to get the path
   * @return the path to the JSON file in the resources of the archive corresponding to the URI
   */
  default String getJsonResourcePathForUri(String pageObjectUri) {
    return Pattern.compile("/pageobjects", Pattern.CASE_INSENSITIVE)
            .matcher(pageObjectUri)
            .replaceAll("")
            .replaceAll(Pattern.quote("-"), Matcher.quoteReplacement("/"))
            .replaceAll(Pattern.quote("/"), Matcher.quoteReplacement(File.separator))
        + ".utam.json";
  }
}
