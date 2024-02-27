/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.translator;

import java.io.IOException;
import java.util.List;
import utam.core.declarative.lint.LintingError;

/**
 * translation runner
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface TranslatorRunner {

  /**
   * first scan all contexts and create generation order based on dependencies <br>
   * generation order is list of Page Object names<br>
   * then for each PO from list de-serialize and create representation of interface and class <br>
   *
   * @return RunnerOutput information for analysing results
   */
  RunnerOutput run();

  /**
   * Executed after run method to write objects to files for each representation for and interface
   * and class
   *
   * @throws IOException if an error is encountered in writing files to disk
   */
  void write() throws IOException;

  /**
   * After translator generated code, dependencies that do not follow default injection rule will be
   * written into configuration files later used by Page Objects Provider
   */
  void writeDependenciesConfigs();

  /**
   * Write manifest file that contains path information about all PO JSON files included in the
   * compiled artifact
   */
  void writeManifest();

  /**
   * Container for runner output information
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  interface RunnerOutput {

    /**
     * Get list of linting errors
     *
     * @return list of linting errors (except excluded)
     */
    List<LintingError> getLintingErrors();
  }
}
