package utam.compiler.translator;

import utam.core.declarative.translator.TranslatorSourceConfig;
import utam.core.framework.UtamLogger;
import utam.core.framework.consumer.UtamError;

import java.io.*;
import java.util.*;
import java.util.function.BiFunction;
import java.util.regex.Pattern;

import static utam.core.framework.UtamLogger.info;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class DefaultSourceConfiguration implements TranslatorSourceConfig {

  public static final String ERR_MISSING_PACKAGE_MAPPING = "missing mapping of folder '%s' to package";
  static final String ERR_MISSING_SOURCE_PATH = "source path for Page Object '%s' is not configured";
  static final String ERR_DUPLICATE_PAGE_OBJECT = "source for Page Object '%s' is already configured";
  static final String ERR_SOURCE_FILES_NULL = "can't scan sources: path '%s' has no JSON files";

  private static final String JSON_FILE_MASK = "utam.json";
  private static final FileFilter SUBFOLDER =
      file ->
          file.isDirectory()
              && !file.getPath().contains("target")
              && !file.getPath().contains("node_modules")
              && Pattern.compile("[-,\\w]+").matcher(file.getName()).matches();
  private static final FilenameFilter JSON_FILTER =
      (dir, name) ->
          Pattern.compile("[a-z]{1}[\\w+].*\\." + JSON_FILE_MASK).matcher(name).matches();
  private final Map<String, String> sourcePath = new HashMap<>();

  private final Map<String, String> packagesMapping;

  private final BiFunction<String, String, String> relativePackageTransformer =
      (parentFolder, fileName) -> String.format("%s.%s", parentFolder, fileName);
  private final BiFunction<String, String, String> collectingPackageTransformer =
      (parentFolder, fileName) -> fileName;

  public DefaultSourceConfiguration(String sourcePath, Map<String, String> packagesMapping) {
    this.packagesMapping = packagesMapping;
    List<File[]> res = processSourcePath(sourcePath);
    res.forEach(this::preProcess);
  }

  public DefaultSourceConfiguration(List<File> inputFiles, Map<String, String> packagesMapping) {
    this.packagesMapping = packagesMapping;
    List<File[]> res = processFileList(inputFiles);
    res.forEach(this::preProcess);
  }

  // used in tests
  DefaultSourceConfiguration() {
    packagesMapping = new HashMap<>();
  }

  static void recursiveScan(Collection<File[]> res, File path) {
    File[] files = path.listFiles(JSON_FILTER);
    if(files == null) {
      throw new UtamError(String.format(ERR_SOURCE_FILES_NULL, path));
    }
    info(
        String.format(
            "scan folder %s, found %d files",
            path.getPath(), Objects.requireNonNull(files).length));
    for (File file : files) {
      info(String.format("found matching file %s/%s", path.getPath(), file.getName()));
      res.add(new File[] {path, file});
    }
    for (File dir : Objects.requireNonNull(path.listFiles(SUBFOLDER))) {
      recursiveScan(res, dir);
    }
  }

  private static String replaceWithPath(String in) {
    return in.replaceAll(Pattern.quote("."), File.separator);
  }

  private static List<File[]> processSourcePath(String sourcePath) {
    List<File[]> processedFiles = new ArrayList<>();
    recursiveScan(processedFiles, new File(sourcePath));
    return processedFiles;
  }

  private static List<File[]> processFileList(List<File> inputFiles) {
    List<File[]> processedFiles = new ArrayList<>();
    for (File file : inputFiles) {
      processedFiles.add(new File[] {new File(file.getParent()), file});
    }
    return processedFiles;
  }

  void preProcess(File[] file) {
    if(file == null || file.length == 0) {
      return;
    }
    String folder = file[0].getName();
    String fileName = file[1].getName();
    if (fileName.startsWith("test")) {
      info(String.format("ignore file '%s' from folder '%s'", fileName, folder));
      return;
    }
    if(!fileName.contains(JSON_FILE_MASK)) {
      UtamLogger.warning(String.format("incorrect file name '%s' from folder '%s'", fileName, folder));
      return;
    }
    info(String.format("pre-process file '%s' from folder '%s'", fileName, folder));
    BiFunction<String, String, String> pathTransformer = relativePackageTransformer;
    if (!packagesMapping.containsKey(folder) && packagesMapping.containsKey("_default")) {
      pathTransformer = collectingPackageTransformer;
    }
    String path =
        pathTransformer.apply(folder, fileName.substring(0, fileName.indexOf(JSON_FILE_MASK) - 1));
    String pageObjectURI =
        String.format("%s/pageObjects/%s", getPackageMapping(folder), replaceWithPath(path));
    if (sourcePath.containsKey(pageObjectURI)) {
      throw new UtamError(String.format(ERR_DUPLICATE_PAGE_OBJECT, pageObjectURI));
    }
    sourcePath.put(pageObjectURI, file[1].getPath());
  }

  @Override
  public String getPackageMapping(String folder) {
    if (folder == null ||
        (!packagesMapping.containsKey(folder) && !packagesMapping.containsKey("_default"))) {
      throw new UtamError(String.format(ERR_MISSING_PACKAGE_MAPPING, folder));
    }
    if (packagesMapping.containsKey(folder)) {
      return packagesMapping.get(folder);
    }
    return packagesMapping.get("_default");
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
}
