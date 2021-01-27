package utam.compiler;

import utam.compiler.translator.TranslatorGenerationCommand;
import picocli.CommandLine;

public class EntryPoint {

  public static void main(String[] args) throws Exception {
    TranslatorGenerationCommand command = new TranslatorGenerationCommand();
    int exitCode = new CommandLine(command)
        .setCaseInsensitiveEnumValuesAllowed(true)
        .execute(args);
    if (exitCode != CommandLine.ExitCode.OK) {
      throw command.getThrownError();
    }
  }
}
