package utam.compiler.translator;

import utam.core.declarative.translator.TranslatorSourceConfig;
import utam.core.framework.UtamLogger;
import utam.core.framework.consumer.UtamError;

import java.io.*;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static utam.core.framework.UtamLogger.info;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class DefaultSourceConfiguration implements TranslatorSourceConfig {

  static final String ERR_MISSING_SOURCE_PATH = "source path for Page Object '%s' is not configured";
  static final String ERR_DUPLICATE_PAGE_OBJECT = "source for Page Object '%s' is already configured";

  private static final String JSON_FILE_MASK = "utam.json";
  private static final String JSON_FILE_MASK_REGEX = "(.*)\\.utam\\.json$";
  private final Map<String, String> sourcePath = new HashMap<>();

  public DefaultSourceConfiguration(String sourcePath, Map<String, String> packagesMapping)
      throws IOException {
    initialize(getInfoList(sourcePath, packagesMapping));
  }

  public DefaultSourceConfiguration(List<File> inputFiles, Map<String, String> packagesMapping) {
    initialize(getInfoList(inputFiles, packagesMapping));
  }

  // used in tests
  DefaultSourceConfiguration() {
  }

  private void initialize(List<PageObjectInfo> inputInfoList) {
    inputInfoList.forEach(this::preProcess);
  }

  private static List<PageObjectInfo> getInfoList(
      String sourcePath, Map<String, String> packagesMapping) throws IOException {
    List<PageObjectInfo> pageObjectInfoList = new ArrayList<>();
    for(Map.Entry<String, String> entry : packagesMapping.entrySet()) {
      final String namespace = entry.getKey();
      final String fileMatchRegex = entry.getValue() + File.separator + JSON_FILE_MASK_REGEX;
      final Pattern relativePattern = Pattern.compile(fileMatchRegex);
      final PathMatcher matcher = FileSystems.getDefault().getPathMatcher("regex:" + fileMatchRegex);
      Files.walkFileTree(Paths.get(sourcePath), new SimpleFileVisitor<>() {
        @Override
        public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
          if (matcher.matches(file)) {
            pageObjectInfoList.add(getPageObjectInfo(namespace, file, relativePattern));
          }
          return FileVisitResult.CONTINUE;
        }
      });
    }
    return pageObjectInfoList;
  }

  private List<PageObjectInfo> getInfoList(
      List<File> fileList, Map<String, String> packagesMapping) {
    List<PageObjectInfo> pageObjectInfoList = new ArrayList<>();
    for(Map.Entry<String, String> entry : packagesMapping.entrySet()) {
      final String namespace = entry.getKey();
      final String fileMatchRegex = entry.getValue() + File.separator + JSON_FILE_MASK_REGEX;
      final Pattern relativePattern = Pattern.compile(fileMatchRegex);
      pageObjectInfoList.addAll(fileList.stream()
          .filter(file -> relativePattern.matcher(file.toString()).matches())
          .map(file -> getPageObjectInfo(namespace, file.toPath(), relativePattern))
          .collect(Collectors.toList()));
    }
    return pageObjectInfoList;
  }

  private static PageObjectInfo getPageObjectInfo(
      String namespace, Path filePath, Pattern relativePattern) {
    String relative = "";
    Matcher matcher = relativePattern.matcher(filePath.toString());
    if (matcher.find()) {
      relative = matcher.group(1);
    }
    return new PageObjectInfo(namespace, filePath, relative);
  }

  private static String replaceWithPath(String in) {
    return in.replaceAll(Pattern.quote("."), File.separator);
  }

  void preProcess(PageObjectInfo inputInfo) {
    if(inputInfo == null) {
      return;
    }
    String folder = inputInfo.filePath.getParent().toString();
    String fileName = inputInfo.filePath.getFileName().toString();
    if (fileName.startsWith("test")) {
      info(String.format("ignore file '%s' from folder '%s'", fileName, folder));
      return;
    }
    if(!fileName.contains(JSON_FILE_MASK)) {
      UtamLogger.warning(String.format("incorrect file name '%s' from folder '%s'", fileName, folder));
      return;
    }
    info(String.format("pre-process file '%s' from folder '%s'", fileName, folder));
    String pageObjectURI =
        String.format("%s/pageObjects/%s", inputInfo.namespace, replaceWithPath(inputInfo.relativePath));
    if (sourcePath.containsKey(pageObjectURI)) {
      throw new UtamError(String.format(ERR_DUPLICATE_PAGE_OBJECT, pageObjectURI));
    }
    sourcePath.put(pageObjectURI, inputInfo.filePath.toString());
  }

  final String getSourcePath(String pageObjectURI) {
    if (!sourcePath.containsKey(pageObjectURI)) {
      throw new UtamError(String.format(ERR_MISSING_SOURCE_PATH, pageObjectURI));
    }
    return sourcePath.get(pageObjectURI);
  }

  @Override
  public Reader getDeclarationReader(String pageObjectURI) throws IOException {
    String path = getSourcePath(pageObjectURI);
    return new InputStreamReader(new FileInputStream(path));
  }

  @Override
  public Collection<String> getPageObjects() {
    return sourcePath.keySet();
  }

  static class PageObjectInfo {
    private final String namespace;
    private final Path filePath;
    private final String relativePath;

    public PageObjectInfo(String namespace, Path filePath, String relativePath) {
      this.namespace = namespace;
      this.filePath = filePath;
      this.relativePath = relativePath;
    }
  }
}
