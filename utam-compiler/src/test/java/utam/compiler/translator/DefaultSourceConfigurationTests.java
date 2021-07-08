/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;
import static utam.compiler.translator.DefaultSourceConfiguration.DEFAULT_JSON_FILE_MASK_REGEX;
import static utam.compiler.translator.DefaultSourceConfiguration.ERR_DUPLICATE_PAGE_OBJECT;
import static utam.compiler.translator.DefaultSourceConfiguration.ERR_IO_DURING_SCAN;
import static utam.compiler.translator.DefaultSourceConfiguration.ERR_MISSING_SOURCE_PATH;
import static utam.compiler.translator.DefaultSourceConfiguration.ScannerConfig;
import static utam.compiler.translator.DefaultTranslatorRunner.DUPLICATE_PAGE_OBJECT_NAME;
import static utam.compiler.translator.TranslatorMockUtilities.IMPL_ONLY_URI;
import static utam.compiler.translator.TranslatorMockUtilities.INTERFACE_ONLY_URI;
import static utam.compiler.translator.TranslatorMockUtilities.PAGE_OBJECT_SOURCE;
import static utam.compiler.translator.TranslatorMockUtilities.PAGE_OBJECT_URI;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import org.hamcrest.CoreMatchers;
import org.testng.annotations.Test;
import utam.compiler.translator.DefaultSourceConfiguration.FilesScanner;
import utam.compiler.translator.DefaultSourceConfiguration.RecursiveScanner;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.declarative.translator.TranslatorSourceConfig;
import utam.core.framework.consumer.UtamError;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class DefaultSourceConfigurationTests {

  private static final String FAKE_IO_EXCEPTION_MESSAGE = "throwing fake IO exception";
  private static final String INTERFACE_ONLY_SOURCE =
      "{"
          + "  \"interface\" : true,\n"
          + "  \"methods\": [\n"
          + "    {\n"
          + "      \"name\" : \"testMethod\",\n"
          + "      \"return\" : \"string\"\n"
          + "    }"
          + "  ]\n"
          + "}";
  private static final String IMPL_ONLY_SOURCE =
      "{"
          + "  \"implements\": \"utam-test/pageObjects/test/testAbstractObject\",\n"
          + "  \"methods\": [\n"
          + "    {\n"
          + "      \"name\" : \"testMethod\",\n"
          + "      \"compose\": [{\"element\":\"root\", \"apply\" : \"getText\"}]"
          + "    }"
          + "  ]\n"
          + "}";

  public static TranslatorSourceConfig getSourceConfig(String jsonString) throws IOException {
    TranslatorSourceConfig mockConfig = mock(TranslatorSourceConfig.class);
    when(mockConfig.getPageObjects()).thenReturn(Collections.singletonList("test"));
    when(mockConfig.getDeclarationReader(any())).thenReturn(new JsonStringReaderMock(jsonString));
    return mockConfig;
  }

  @Test
  public void testRunWithDuplicatePageObjectsThrows() {
    TranslatorConfig configuration = new DefaultTranslatorConfiguration(new DuplicatePageObjects(),
        new DefaultTargetConfiguration());
    DefaultTranslatorRunner translator = new DefaultTranslatorRunner(configuration);
    UtamError e = expectThrows(UtamError.class, translator::run);
    assertThat(
        e.getMessage(), containsString(String.format(DUPLICATE_PAGE_OBJECT_NAME, PAGE_OBJECT_URI)));
  }

  @Test
  public void testMissingPageObjectThrows() {
    final String PAGE_OBJECT = "error";
    UtamError e =
        expectThrows(
            UtamError.class, () -> new DefaultSourceConfiguration() {
            }.getPageObjectFileSourcePath(PAGE_OBJECT));
    assertThat(e.getMessage(), containsString(String.format(ERR_MISSING_SOURCE_PATH, PAGE_OBJECT)));
  }

  @Test
  public void testRecursiveScan() {
    ScannerConfig scannerConfig = new ScannerConfig(Collections.singletonMap("package", ".*/one"));
    RecursiveScanner scanner = new RecursiveScanner(
        System.getProperty("user.dir") + "/src/test/resources/spec");
    DefaultSourceConfiguration config = new DefaultSourceConfiguration(scannerConfig, scanner);
    config.recursiveScan();
    assertThat(config.getPageObjectFileSourcePath("package/pageObjects/first"),
        is(CoreMatchers.notNullValue()));
  }

  @Test
  public void testRecursiveScanPreProcess() throws IOException {
    ScannerConfig scannerConfig = new ScannerConfig(Collections.singletonMap("utam-one", ".*/one"));
    RecursiveScanner scanner = new RecursiveScanner(null);
    DefaultSourceConfiguration config = new DefaultSourceConfiguration(scannerConfig, scanner);
    String pathString = "folder/one/test.utam.json";
    Consumer<String> test = str -> config.preProcess("utam-one", Paths.get(str),
        ".*/one/" + DEFAULT_JSON_FILE_MASK_REGEX);
    test.accept(pathString);
    String expectedURI = "utam-one/pageObjects/test";
    assertThat(config.getPageObjects().iterator().next(), is(equalTo(expectedURI)));
    assertThat(config.getPageObjectFileSourcePath(expectedURI), is(notNullValue()));
    assertThat(config.getDeclarationReader(expectedURI), is(notNullValue()));
    // duplicate throws!
    UtamError e = expectThrows(UtamError.class, () -> test.accept(pathString));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_DUPLICATE_PAGE_OBJECT, expectedURI))));
  }

  @Test
  public void testRecursiveScanThrows() {
    ScannerConfig scannerConfig = new ScannerConfig(new HashMap<>());
    RecursiveScanner scanner = new RecursiveScanner(null);
    DefaultSourceConfiguration config = new DefaultSourceConfiguration(scannerConfig, scanner);
    UtamError e = expectThrows(UtamError.class,
        () -> scanner.scan(config, "wrongPackage", "wrongMask"));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_IO_DURING_SCAN, "null"))));
  }

  @Test
  public void testFileScanThrows() {
    UtamError e = expectThrows(UtamError.class,
        () -> new FilesScanner(null));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_IO_DURING_SCAN, "null"))));
  }

  @Test
  public void testFileScanner() {
    List<File> filesToScan = new ArrayList<>();
    filesToScan.add(
        new File(getClass().getClassLoader().getResource("spec/one/first.utam.json").getFile()));
    FilesScanner filesScanner = new FilesScanner(filesToScan);
    ScannerConfig scannerConfig = new ScannerConfig(Collections.singletonMap("package", ".*/one"));
    DefaultSourceConfiguration configuration = new DefaultSourceConfiguration(
        scannerConfig, filesScanner);
    configuration.recursiveScan();
    assertThat(configuration.getPageObjectFileSourcePath("package/pageObjects/first"),
        is(CoreMatchers.notNullValue()));
  }

  static class Mock extends DefaultSourceConfiguration implements TranslatorSourceConfig {

    private final Map<String, String> pageObjectsJSONString = new HashMap<>();

    @Override
    public Reader getDeclarationReader(String pageObjectURI) {
      return new JsonStringReaderMock(pageObjectsJSONString.get(pageObjectURI));
    }

    @Override
    public Collection<String> getPageObjects() {
      return pageObjectsJSONString.keySet();
    }

    final void setJSONSource(String pageObject, String path) {
      pageObjectsJSONString.put(pageObject, path);
    }

    final void setSources() {
      setJSONSource(PAGE_OBJECT_URI, PAGE_OBJECT_SOURCE);
      setJSONSource(INTERFACE_ONLY_URI, INTERFACE_ONLY_SOURCE);
      setJSONSource(IMPL_ONLY_URI, IMPL_ONLY_SOURCE);
    }
  }

  private static class DuplicatePageObjects extends DefaultSourceConfigurationTests.Mock {

    DuplicatePageObjects() {
      setSources();
    }

    @Override
    public Collection<String> getPageObjects() {
      List<String> pageObjectList = new ArrayList<>(super.getPageObjects());
      pageObjectList.add(PAGE_OBJECT_URI); // add duplicate
      return pageObjectList;
    }
  }

  static class JsonStringReaderMock extends Reader {

    final int currentPosition = 0;
    private final String jsonString;
    boolean isAtEndOfStream = false;

    JsonStringReaderMock(String jsonString) {
      this.jsonString = jsonString;
    }

    @SuppressWarnings("NullableProblems")
    @Override
    public int read(char[] cbuf, int off, int len) throws IOException {
      if (jsonString == null) {
        throw new IOException(FAKE_IO_EXCEPTION_MESSAGE);
      }

      if (isAtEndOfStream) {
        return -1;
      }

      int copied = 0;
      int availableLength = cbuf.length < off + len ? cbuf.length - off : len - off;
      for (int i = currentPosition; i < availableLength; i++) {
        if (i >= jsonString.length()) {
          isAtEndOfStream = true;
          break;
        }
        cbuf[i + off] = jsonString.charAt(i);
        copied++;
      }
      return copied;
    }

    @Override
    public void close() {
    }
  }
}
