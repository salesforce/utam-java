package utam.compiler.translator;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.Callable;

import utam.core.declarative.translator.TranslatorRunner;
import utam.core.declarative.translator.UnitTestRunner;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;

@Command(name = "generatePageObjects", mixinStandardHelpOptions = true, description = "A command to generate UTAM Page Objects")
public class TranslatorGenerationCommand implements Callable<Integer> {

  private static final String MISSING_INPUT =
      "You must specify an input directory with the --inputDirectory argument or a list of files";
  private static final String TOO_MANY_INPUTS =
      "You cannot specify both an input directory with the --inputDirectory argument and a list of files";
  private static final String INVALID_UNIT_TEST_CONFIG =
      "You cannot specify a unit test runner without a destination directory for unit tests";

  @Option(names = {"-d", "-destinationDirectory", "--destinationDirectory"}, required = true,
          description = "Destination directory to which generated Page Object files will be written.")
  private File destinationDirectory;

  @Option(names = {"-m", "-packageMappingFile", "--packageMappingFile"}, required = true,
          description = "File containing mapping between directories and package names.")
  private File packageMappingFile;

  @Option(names = {"-p", "-profileDirectory", "--profileDirectory"}, required = true,
          description = "Destination directory to which profile information will be written.")
  private File profileDirectory;

  @Option(names = {"-r", "-unitTestRunner", "--unitTestRunner"}, defaultValue = "NONE",
          description = "Unit test runner to use for generated unit tests for Page Objects. Valid values: ${COMPLETION-CANDIDATES} (default: ${DEFAULT-VALUE})")
  private UnitTestRunner testRunner;

  @Option(names = {"-u", "-unitTestDirectory", "--unitTestDirectory"},
          description = "Destination directory to which generated unit tests will be written.")
  private File unitTestDirectory;

  @Option(names = {"-i", "-inputDirectory", "--inputDirectory"},
          description = "Input directory to be recursively scanned for utam.core.declarative Page Object description files. Cannot be used with an explicit file list.")
  private File inputDirectory;

  @Parameters(description = "Explicit list of utam.core.declarative Page Object description files to generate. Cannot be used with the --inputDirectory option.")
  private List<File> inputFiles;

  private Exception thrownError;

  public Exception getThrownError() {
    return thrownError;
  }

  @Override
  public Integer call() {
    if (inputDirectory == null && (inputFiles == null || inputFiles.size() == 0)) {
      // Must specify either an input directory or a list of files
      thrownError = new UnsupportedOperationException(MISSING_INPUT);
      return CommandLine.ExitCode.USAGE;
    }

    if (inputDirectory != null && inputFiles != null && inputFiles.size() > 0) {
      // Cannot specify both input directory and list of files.
      thrownError = new UnsupportedOperationException(TOO_MANY_INPUTS);
      return CommandLine.ExitCode.USAGE;
    }

    if (testRunner != UnitTestRunner.NONE && unitTestDirectory == null) {
      // If specifying a unit test runner, you must specify a unit test directory.
      thrownError = new UnsupportedOperationException(INVALID_UNIT_TEST_CONFIG);
      return CommandLine.ExitCode.USAGE;
    }

    try {
      String unitTestDirectoryPath = "";
      if (unitTestDirectory != null) {
        unitTestDirectoryPath = unitTestDirectory.toString();
      }

      TranslatorRunner translator;
      if (inputFiles != null && inputFiles.size() > 0) {
        translator = new DefaultTranslatorRunner(
            new DefaultTranslatorConfiguration(
                inputFiles,
                destinationDirectory.toString(),
                unitTestDirectoryPath,
                testRunner.toString(),
                packageMappingFile.toString(),
                profileDirectory.toString(),
                new HashMap<>()));
      } else {
        translator = new DefaultTranslatorRunner(
            new DefaultTranslatorConfiguration(
                inputDirectory.toString(),
                destinationDirectory.toString(),
                unitTestDirectoryPath,
                testRunner.toString(),
                packageMappingFile.toString(),
                profileDirectory.toString(),
                new HashMap<>()));
      }
      translator.run();
      translator.write();
      translator.writeDependenciesConfigs();
    } catch (IOException e) {
      thrownError = e;
      return CommandLine.ExitCode.SOFTWARE;
    }
    return CommandLine.ExitCode.OK;
  }
}

