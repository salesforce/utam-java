/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static utam.compiler.translator.JsonCompilerConfig.Module.DEFAULT_JSON_FILE_MASK_REGEX;
import static utam.core.framework.UtamLogger.info;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import utam.compiler.UtamCompilationError;
import utam.core.declarative.translator.TranslatorSourceConfig;
import utam.core.framework.consumer.UtamError;

/**
 * configuration of page objects sources
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class DefaultSourceConfiguration implements TranslatorSourceConfig {

  static final String ERR_MISSING_SOURCE_PATH = "source path for Page Object '%s' is not configured";
  static final String ERR_DUPLICATE_PAGE_OBJECT = "source for Page Object '%s' is already configured";
  static final String ERR_IO_DURING_SCAN = "Error while scanning file(s) path %s";
  private static final String PAGE_OBJECT_URI_FORMAT = "%s/pageObjects/%s";
  private final Map<String, String> sourcePath = new HashMap<>();
  private final RecursiveScanner scanner;
  private final ScannerConfig scannerConfig;

  /**
   * @param scannerConfig      configuration to scan for page object sources
   * @param scanner         scanner for page object sources Page Objects will be written
   */
  public DefaultSourceConfiguration(ScannerConfig scannerConfig, RecursiveScanner scanner) {
    this.scanner = scanner;
    this.scannerConfig = scannerConfig;
  }

  // used in tests
  DefaultSourceConfiguration() {
    this(new ScannerConfig(new HashMap<>()), new RecursiveScanner(null));
  }

  /**
   * parse file path to extract PO name
   *
   * @param packageName   name of package is used in matcher
   * @param filePath      full file path
   * @param fileMaskRegex regex with mask for JSON to extract page object name
   */
  void preProcess(String packageName, Path filePath, String fileMaskRegex) {
    info(String.format("found Page Object '%s'", filePath.toString()));
    String pageObjectURI = getPageObjectURI(packageName, filePath, fileMaskRegex);
    if (sourcePath.containsKey(pageObjectURI)) {
      throw new UtamCompilationError(String.format(ERR_DUPLICATE_PAGE_OBJECT, pageObjectURI));
    }
    sourcePath.put(pageObjectURI, filePath.toString());
  }

  String getPageObjectURI(String packageName, Path filePath, String fileMaskRegex) {
    final Pattern relativePattern = Pattern.compile(fileMaskRegex);
    Matcher matcher = relativePattern.matcher(filePath.toString());
    // gets text inside () of the mask, usually PO file name
    final String relativePath = matcher.find() ? matcher.group(1) : "";
    final String relativePageObjectName = relativePath.replaceAll(Pattern.quote("."), File.separator);
    return String.format(PAGE_OBJECT_URI_FORMAT, packageName, relativePageObjectName);
  }

  final String getPageObjectFileSourcePath(String pageObjectURI) {
    if (!sourcePath.containsKey(pageObjectURI)) {
      throw new UtamError(String.format(ERR_MISSING_SOURCE_PATH, pageObjectURI));
    }
    return sourcePath.get(pageObjectURI);
  }

  @Override
  public Reader getDeclarationReader(String pageObjectURI) throws IOException {
    String path = getPageObjectFileSourcePath(pageObjectURI);
    return new InputStreamReader(new FileInputStream(path));
  }

  @Override
  public Collection<String> getPageObjects() {
    return sourcePath.keySet();
  }

  @Override
  public void recursiveScan() {
    for (String packageName : scannerConfig.getAllPackages()) {
      String fileMaskRegex = scannerConfig.getFileMask(packageName);
      scanner.scan(this, packageName, fileMaskRegex);
    }
  }

  /**
   * scan files recursively to find JSON files with Page Objects,
   * public because used in downstream projects
   *
   * @author elizaveta.ivanova
   */
  public static class RecursiveScanner {

    final String rootFolder;

    public RecursiveScanner(String rootFolder) {
      this.rootFolder = rootFolder;
    }

    void scan(DefaultSourceConfiguration config, String packageName, String fileMaskRegex) {
      PathMatcher matcher = FileSystems.getDefault().getPathMatcher("regex:" + fileMaskRegex);
      try {
        Files.walkFileTree(Paths.get(rootFolder), new SimpleFileVisitor<>() {
          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
            if (matcher.matches(file)) {
              config.preProcess(packageName, file, fileMaskRegex);
            }
            return FileVisitResult.CONTINUE;
          }
        });
      } catch (IOException | NullPointerException e) {
        throw new UtamCompilationError(String.format(ERR_IO_DURING_SCAN, rootFolder), e);
      }
    }
  }

  /**
   * unlike regular scanner, accepts list of files instead traversing,
   * public because used in downstream projects
   *
   * @author elizaveta.ivanova
   */
  public static class FilesScanner extends RecursiveScanner {

    final List<File> inputFiles;

    public FilesScanner(List<File> inputFiles) {
      super(null);
      this.inputFiles = inputFiles;
      if (inputFiles == null) {
        throw new UtamCompilationError(String.format(ERR_IO_DURING_SCAN, "null"));
      }
    }

    @Override
    void scan(DefaultSourceConfiguration config, String packageName, String fileMaskRegex) {
      Pattern relativePattern = Pattern.compile(fileMaskRegex);
      inputFiles.stream()
          .filter(file -> relativePattern.matcher(file.toString()).matches())
          .forEach(file -> config.preProcess(packageName, file.toPath(), fileMaskRegex));
    }

  }

  /**
   * configuration for files traversal that includes packages mapping and file mask to identify page
   * objects, public because used in downstream projects
   *
   * @author elizaveta.ivanova
   */
  public static class ScannerConfig {

    private final Map<String, String> packagesMapping = new HashMap<>();

    public ScannerConfig(String pageObjectFileMask, Map<String, String> packagesMapping) {
      packagesMapping.forEach((key, value) -> this.packagesMapping
          .put(key, value + File.separator + pageObjectFileMask));
    }

    public ScannerConfig(Map<String, String> packagesMapping) {
      this(DEFAULT_JSON_FILE_MASK_REGEX, packagesMapping);
    }

    Collection<String> getAllPackages() {
      return packagesMapping.keySet();
    }

    String getFileMask(String packageName) {
      return packagesMapping.get(packageName);
    }
  }

  /**
   * Source configuration for files traversal without configured packages
   *
   * @author elizaveta.ivanova
   */
  static class SourceWithoutPackages extends DefaultSourceConfiguration {

    private final String pageObjectFileMask;
    private final String rootFolder;
    private static final String DEFAULT_PACKAGE_NAME = "utam";

    SourceWithoutPackages(String rootFolder, String pageObjectsFileMask) {
      super(new ScannerConfig(pageObjectsFileMask, new HashMap<>()), new RecursiveScanner(rootFolder));
      this.pageObjectFileMask = pageObjectsFileMask;
      this.rootFolder = rootFolder;
    }

    @Override
    public void recursiveScan() {
      PathMatcher matcher = FileSystems.getDefault().getPathMatcher("regex:" + pageObjectFileMask);
      try {
        Files.walkFileTree(Paths.get(rootFolder), new SimpleFileVisitor<>() {
          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
            if (matcher.matches(file)) {
              preProcess(DEFAULT_PACKAGE_NAME, file, pageObjectFileMask);
            }
            return FileVisitResult.CONTINUE;
          }
        });
      } catch (IOException | NullPointerException e) {
        throw new UtamCompilationError(String.format(ERR_IO_DURING_SCAN, rootFolder), e);
      }
    }

    @Override
    String getPageObjectURI(String packageName, Path filePath, String fileMaskRegex) {
      // this gets filename without file extension
      String fileName = filePath.getFileName().toString().split(Pattern.quote("."))[0];
      return String.format(PAGE_OBJECT_URI_FORMAT, DEFAULT_PACKAGE_NAME, fileName);
    }
  }
}
