package utam.generator;

import declarative.translator.TranslatorGenerationCommand;
import picocli.CommandLine;

public class Main {

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
