package utam.compiler.translator;

import com.google.common.io.Files;
import declarative.representation.TypeProvider;
import declarative.translator.TranslatorTargetConfig;

import java.io.*;
import java.util.regex.Pattern;

import static framework.UtamLogger.info;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class DefaultTargetConfiguration implements TranslatorTargetConfig {

  private static final String SRC_DIRECTORY_MARKER = File.separator + "main" + File.separator;
  private static final String TEST_DIRECTORY_MARKER = File.separator + "test" + File.separator;

  private final String resourcesHomePath;
  private final String targetPath;
  private final String unitTestDirectory;

  public DefaultTargetConfiguration(String targetPath, String resourcesHomePath) {
    this(targetPath, resourcesHomePath, null);
  }

  public DefaultTargetConfiguration(String targetPath, String resourcesHomePath,
      String unitTestDirectory) {
    this.resourcesHomePath = resourcesHomePath;
    this.targetPath = targetPath;
    if (unitTestDirectory == null || unitTestDirectory.isEmpty()) {
      this.unitTestDirectory = targetPath.replace(SRC_DIRECTORY_MARKER, TEST_DIRECTORY_MARKER);
    } else {
      this.unitTestDirectory = unitTestDirectory;
    }
  }

  @SuppressWarnings("UnstableApiUsage")
  static FileWriter getWriter(String fullPath) throws IOException {
    try {
      return new FileWriter(fullPath);
    } catch (FileNotFoundException notFound) {
      File file = new File(fullPath);
      try {
        Files.createParentDirs(file);
        Files.touch(file);
        return new FileWriter(file);
      } catch (IOException cantCreate) {
        info(String.format("could not create file '%s' : %s", fullPath, cantCreate.getMessage()));
        throw notFound;
      }
    }
  }

  private static String replaceWithPath(String in) {
    return in.replaceAll(Pattern.quote("."), File.separator);
  }

  @Override
  public String getInjectionConfigRootFilePath() {
    return resourcesHomePath;
  }

  @Override
  public Writer getClassWriter(TypeProvider pageObjectType) throws IOException {
    String fullPath = targetPath
        + File.separator
        + replaceWithPath(pageObjectType.getFullName()) + ".java";
    return getWriter(fullPath);
  }

  @Override
  public Writer getUnitTestWriter(TypeProvider pageObjectType) throws IOException {
    String fullPath = unitTestDirectory
            + File.separator
            + replaceWithPath(pageObjectType.getFullName())
            + "Tests.java";
    if (new File(fullPath).exists()) {
      // Important: If the path exists, we do not want to overwrite existing
      // test files.
      return null;
    }
    return getWriter(fullPath);
  }
}
