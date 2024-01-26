/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler;

import picocli.CommandLine;
import utam.compiler.translator.TranslatorGenerationCommand;

/** The entry point for the compiler when invoked programmatically */
public class EntryPoint {

  /**
   * The main method for the command line entry point
   *
   * @param args the list of arguments passed in via the command line
   * @throws Exception thrown if the command executed does not return a success status
   */
  public static void main(String[] args) throws Exception {
    TranslatorGenerationCommand command = new TranslatorGenerationCommand();
    int exitCode = new CommandLine(command).setCaseInsensitiveEnumValuesAllowed(true).execute(args);
    if (exitCode != CommandLine.ExitCode.OK) {
      throw command.getThrownError();
    }
  }
}
