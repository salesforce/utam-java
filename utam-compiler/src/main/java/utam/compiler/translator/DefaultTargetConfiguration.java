/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import com.google.common.io.Files;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import utam.core.declarative.representation.PageObjectClass;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.representation.PageObjectInterface;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.translator.TranslatorTargetConfig;
import utam.core.declarative.translator.UnitTestRunner;

/**
 * compiler output configuration
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class DefaultTargetConfiguration implements TranslatorTargetConfig {
  private static final String SRC_DIRECTORY_MARKER = File.separator + "main" + File.separator;
  private static final String TEST_DIRECTORY_MARKER = File.separator + "test" + File.separator;
  private final String resourcesHomePath;
  private final String targetPath;
  private final String unitTestDirectory;
  private final UnitTestRunner unitTestRunner;
  private final String compilerRoot;
  private final String compilerErrorsReportFile;
  private final boolean incremental;

  /**
   * compiler output configuration
   *
   * @param compilerRoot root of the project
   * @param targetPath the root output directory where the generated Page Object source files will
   *     be written
   * @param resourcesHomePath the output directory in which to write dependencies information
   * @param unitTestRunner the test runner to use when generating unit tests
   * @param unitTestDirectory the root output directory where generated unit tests for generated
   *     Page Objects will be written
   * @param compilerErrorsFile name of the file to write compilation errors if configured
   */
  public DefaultTargetConfiguration(
      String compilerRoot,
      String targetPath,
      String resourcesHomePath,
      UnitTestRunner unitTestRunner,
      String unitTestDirectory,
      String compilerErrorsFile) {
    this(
        compilerRoot,
        targetPath,
        resourcesHomePath,
        unitTestRunner,
        unitTestDirectory,
        compilerErrorsFile,
        false);
  }

  /**
   * compiler output configuration with incremental flag
   *
   * @param compilerRoot root of the project
   * @param targetPath the root output directory where the generated Page Object source files will
   *     be written
   * @param resourcesHomePath the output directory in which to write dependencies information
   * @param unitTestRunner the test runner to use when generating unit tests
   * @param unitTestDirectory the root output directory where generated unit tests for generated
   *     Page Objects will be written
   * @param compilerErrorsFile name of the file to write compilation errors if configured
   * @param incremental when true, skip Page Objects whose generated artifacts are newer than the
   *     source JSON
   */
  public DefaultTargetConfiguration(
      String compilerRoot,
      String targetPath,
      String resourcesHomePath,
      UnitTestRunner unitTestRunner,
      String unitTestDirectory,
      String compilerErrorsFile,
      boolean incremental) {
    this.resourcesHomePath = resourcesHomePath;
    this.targetPath = targetPath;
    if (unitTestDirectory == null || unitTestDirectory.isEmpty()) {
      this.unitTestDirectory = targetPath.replace(SRC_DIRECTORY_MARKER, TEST_DIRECTORY_MARKER);
    } else {
      this.unitTestDirectory = unitTestDirectory;
    }
    this.unitTestRunner = unitTestRunner == null ? UnitTestRunner.NONE : unitTestRunner;
    this.compilerRoot = compilerRoot;
    this.compilerErrorsReportFile = getErrorsReportPath(resourcesHomePath, compilerErrorsFile);
    this.incremental = incremental;
  }

  /**
   * this constructor can be used for distribution repo where we do not generate unit tests
   *
   * @param compilerRoot root of the project
   * @param targetPath the root output directory where the generated Page Object source files will
   *     be written
   * @param resourcesHomePath the output directory in which to write profile information
   */
  public DefaultTargetConfiguration(
      String compilerRoot, String targetPath, String resourcesHomePath) {
    this(compilerRoot, targetPath, resourcesHomePath, null, null, null);
  }

  private static String getErrorsReportPath(String resourcesOutputDir, String errorsReportFile) {
    if (errorsReportFile == null) {
      return null;
    }
    String targetPath =
        resourcesOutputDir == null ? System.getProperty("user.dir") : resourcesOutputDir;
    return targetPath.endsWith(File.separator)
        ? targetPath + errorsReportFile
        : targetPath + File.separator + errorsReportFile;
  }

  public static FileWriter getWriterWithDir(String fullPath) throws IOException {
    try {
      return new FileWriter(fullPath);
    } catch (FileNotFoundException notFound) {
      File file = new File(fullPath);
      try {
        Files.createParentDirs(file);
        Files.touch(file);
        return new FileWriter(file);
      } catch (IOException cantCreate) {
        String err = String.format("could not create file %s", fullPath);
        throw new UtamRunnerError(err, cantCreate);
      }
    }
  }

  private static String replaceWithPath(String in) {
    return in.replaceAll(Pattern.quote("."), Matcher.quoteReplacement(File.separator))
        .replaceAll(Pattern.quote("/"), Matcher.quoteReplacement(File.separator));
  }

  @Override
  public String getInjectionConfigRootFilePath() {
    return resourcesHomePath;
  }

  String getPageObjectClassPath(TypeProvider pageObjectType) {
    return targetPath + File.separator + replaceWithPath(pageObjectType.getFullName()) + ".java";
  }

  @Override
  public Writer getClassWriter(TypeProvider pageObjectType) throws IOException {
    String fullPath = getPageObjectClassPath(pageObjectType);
    return getWriterWithDir(fullPath);
  }

  String getPageObjectTestClassPath(TypeProvider pageObjectType) {
    return unitTestDirectory
        + File.separator
        + replaceWithPath(pageObjectType.getFullName())
        + "Tests.java";
  }

  @Override
  public Writer getUnitTestWriter(TypeProvider pageObjectType) throws IOException {
    String fullPath = getPageObjectTestClassPath(pageObjectType);
    if (new File(fullPath).exists()) {
      // Important: If the path exists, we do not want to overwrite existing
      // test files.
      return null;
    }
    return getWriterWithDir(fullPath);
  }

  @Override
  public Writer getResourceWriter(String pageObjectUri) throws IOException {
    String fullPath = resourcesHomePath + File.separator + getJsonResourcePathForUri(pageObjectUri);
    if (new File(fullPath).exists()) {
      // Important: If the path exists, we do not want to overwrite existing
      // JSON source files.
      return null;
    }
    return getWriterWithDir(fullPath);
  }

  @Override
  public UnitTestRunner getUnitTestRunnerType() {
    return unitTestRunner;
  }

  @Override
  public boolean isUpToDate(PageObjectDeclaration object, long sourceLastModifiedMillis) {
    if (!incremental || sourceLastModifiedMillis <= 0) {
      return false;
    }
    PageObjectInterface pageObjectInterface = object.getInterface();
    // Only the JSON that defines the interface (i.e. its own class implements that interface)
    // can declare itself up-to-date for the interface artifact. JSON files that reference an
    // interface declared elsewhere don't write the interface file.
    if (object.isClassWithInterface()
        && !isTargetNewer(
            getPageObjectClassPath(pageObjectInterface.getInterfaceType()),
            sourceLastModifiedMillis)) {
      return false;
    }
    if (!object.isInterfaceOnly()) {
      PageObjectClass impl = object.getImplementation();
      if (!isTargetNewer(getPageObjectClassPath(impl.getClassType()), sourceLastModifiedMillis)) {
        return false;
      }
    }
    return true;
  }

  private static boolean isTargetNewer(String fullPath, long sourceLastModifiedMillis) {
    File target = new File(fullPath);
    if (!target.exists()) {
      return false;
    }
    return target.lastModified() >= sourceLastModifiedMillis;
  }

  @Override
  public String getLintReportPath() {
    return compilerRoot;
  }

  @Override
  public String getErrorsReportPath() {
    return compilerErrorsReportFile;
  }
}
