/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.translator;

import java.io.IOException;
import java.io.Reader;
import java.util.Collection;

/**
 * configuration of source with JSON files
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface TranslatorSourceConfig {

  /**
   * read declarative file by name of the PO
   *
   * @param pageObjectURI name of the page object
   * @return reader for file with json declaration
   * @throws IOException if the file does not exist
   */
  Reader getDeclarationReader(String pageObjectURI) throws IOException;

  /**
   * get all configured JSON source files with page objects as string with Page Object URI ex.
   * utam-global/pageObjects/global/Name
   *
   * @return collection with unique Page Objects URIs
   */
  Collection<String> getPageObjects();

  /** traverse and scan input folder to find all files with declarative POs */
  void recursiveScan();

  /**
   * get path to JSON with source
   *
   * @param pageObjectURI name of the page object
   * @return JSON path, will be used in generated JavaDoc
   */
  String getSourcePath(String pageObjectURI);
}
