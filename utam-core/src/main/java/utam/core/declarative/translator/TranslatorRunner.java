/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.translator;

import java.io.IOException;

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
   * @throws IOException if an error is encountered in translation
   */
  void run() throws IOException;

  /**
   * can only be executed after run method to write objects to files <br>
   * for each representation created after "run" write files with interface and class <br>
   * @throws IOException if an error is encountered in writing files to disk
   */
  void write() throws IOException;

  /**
   * after translator generated code, dependencies that do not follow default injection rule
   * will be written into configuration files later used by Page Objects Provider
   */
  void writeDependenciesConfigs();
}
