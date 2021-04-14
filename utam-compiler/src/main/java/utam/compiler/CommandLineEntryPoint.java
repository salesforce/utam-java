/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler;

import utam.compiler.translator.TranslatorGenerationCommand;
import picocli.CommandLine;

public class CommandLineEntryPoint {

  public static void main(String[] args) {
    TranslatorGenerationCommand command = new TranslatorGenerationCommand();
    int exitCode = new CommandLine(command)
        .setCaseInsensitiveEnumValuesAllowed(true)
        .execute(args);
    if (exitCode != CommandLine.ExitCode.OK) {
      System.out.println(command.getThrownError().getMessage());
    }
    System.exit(exitCode);
  }
}
