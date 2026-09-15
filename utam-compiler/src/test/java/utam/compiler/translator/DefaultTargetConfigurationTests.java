/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.compiler.translator.DefaultTargetConfiguration.getWriterWithDir;
import static utam.compiler.translator.DefaultTranslatorRunnerTests.IMPL_ONLY_CLASS_NAME;
import static utam.compiler.translator.DefaultTranslatorRunnerTests.INTERFACE_ONLY_CLASS_NAME;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import org.testng.annotations.Test;
import utam.compiler.helpers.TypeUtilities.FromString;
import utam.compiler.translator.DefaultSourceConfigurationTests.TranslatorConfigWithProfile;
import utam.core.declarative.representation.PageObjectClass;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.representation.PageObjectInterface;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.declarative.translator.TranslatorRunner;
import utam.core.declarative.translator.TranslatorTargetConfig;
import utam.core.declarative.translator.UnitTestRunner;

public class DefaultTargetConfigurationTests {

  static final String PAGE_OBJECT_INTERFACE_CLASS_NAME =
      "utam.test.pageobjects.test.TestPageObject";
  static final String PAGE_OBJECT_IMPL_CLASS_NAME =
      "utam.test.pageobjects.test.impl.TestPageObjectImpl";
  static final String PAGE_OBJECT_URI = "utam-test/pageObjects/test/testPageObject";
  static final String INTERFACE_ONLY_PAGE_OBJECT_URI =
      "utam-test/pageObjects/test/testAbstractObject";
  static final String IMPL_ONLY_PAGE_OBJECT_URI = "utam-test/pageObjects/test/testImplObject";
  private static final String FAKE_IO_EXCEPTION_MESSAGE = "throwing fake IO exception";

  @Test
  public void testWriteWithWriterErrorThrows() {
    TranslatorRunner translator = new ClassWriterError().getRunner();
    translator.run();
    RuntimeException e = expectThrows(RuntimeException.class, translator::write);
    assertThat(e.getCause(), is(instanceOf(IOException.class)));
    assertThat(e.getCause().getMessage(), is(equalTo(FAKE_IO_EXCEPTION_MESSAGE)));
  }

  @Test
  public void testWriteWithUnitTestWriterErrorThrows() {
    TranslatorRunner translator = new UnitTestWriterThrowsError(UnitTestRunner.TESTNG).getRunner();
    translator.run();
    RuntimeException e = expectThrows(RuntimeException.class, translator::write);
    assertThat(e.getCause(), is(instanceOf(IOException.class)));
    assertThat(e.getCause().getMessage(), is(equalTo(FAKE_IO_EXCEPTION_MESSAGE)));
  }

  @Test
  public void testUnitTestsWriterWriteThrows() {
    TranslatorRunner translator = new UnitTestWriterNotConfigured(UnitTestRunner.JUNIT).getRunner();
    translator.run();
    IOException e = expectThrows(IOException.class, translator::write);
    assertThat(e.getMessage(), is(equalTo(UnitTestWriterNotConfigured.ERROR)));
  }

  @Test
  public void testWriteWithNoUnitTestRunner() throws IOException {
    Mock configuration = new Mock(UnitTestRunner.NONE);
    TranslatorRunner translator = configuration.getRunner();
    translator.run();
    translator.write();
    assertThat(configuration.writers.keySet(), hasSize(7));
    assertThat(
        configuration.writers.keySet(),
        containsInAnyOrder(
            PAGE_OBJECT_INTERFACE_CLASS_NAME,
            PAGE_OBJECT_IMPL_CLASS_NAME,
            INTERFACE_ONLY_CLASS_NAME,
            IMPL_ONLY_CLASS_NAME,
            PAGE_OBJECT_URI,
            INTERFACE_ONLY_PAGE_OBJECT_URI,
            IMPL_ONLY_PAGE_OBJECT_URI));
  }

  @Test
  public void testWriteWithNullUnitTestRunner() throws IOException {
    Mock configuration = new Mock();
    TranslatorConfig translatorConfig = configuration.getConfig();
    TranslatorRunner translator = new DefaultTranslatorRunner(translatorConfig);
    translator.run();
    translator.write();
    assertThat(configuration.writers.keySet(), hasSize(7));
    assertThat(
        configuration.writers.keySet(),
        containsInAnyOrder(
            PAGE_OBJECT_INTERFACE_CLASS_NAME,
            PAGE_OBJECT_IMPL_CLASS_NAME,
            INTERFACE_ONLY_CLASS_NAME,
            IMPL_ONLY_CLASS_NAME,
            PAGE_OBJECT_URI,
            INTERFACE_ONLY_PAGE_OBJECT_URI,
            IMPL_ONLY_PAGE_OBJECT_URI));
  }

  @Test
  public void testWriteNullUnitTestRunner() throws IOException {
    Mock configuration = new NullUnitTestWriter(UnitTestRunner.TESTNG);
    TranslatorRunner translator = configuration.getRunner();
    translator.run();
    translator.write();
    assertThat(configuration.writers.keySet(), hasSize(7));
    assertThat(
        configuration.writers.keySet(),
        containsInAnyOrder(
            PAGE_OBJECT_INTERFACE_CLASS_NAME,
            PAGE_OBJECT_IMPL_CLASS_NAME,
            INTERFACE_ONLY_CLASS_NAME,
            IMPL_ONLY_CLASS_NAME,
            PAGE_OBJECT_URI,
            INTERFACE_ONLY_PAGE_OBJECT_URI,
            IMPL_ONLY_PAGE_OBJECT_URI));
  }

  @Test
  public void testGetWriterError() {
    assertThrows(UtamRunnerError.class, () -> getWriterWithDir(""));
  }

  @Test
  public void testConstructor() {
    String currentDir = System.getProperty("user.dir");
    DefaultTargetConfiguration targetConfig =
        new DefaultTargetConfiguration(
            currentDir, currentDir, currentDir, UnitTestRunner.JUNIT, currentDir, null);
    assertThat(targetConfig.getUnitTestRunnerType(), is(equalTo(UnitTestRunner.JUNIT)));
    assertThat(targetConfig.getInjectionConfigRootFilePath(), is(equalTo(currentDir)));
    String typeName = "utam/MyPage";
    TypeProvider type = new FromString(typeName);
    assertThat(
        targetConfig.getPageObjectClassPath(type),
        is(equalTo(currentDir + File.separator + "utam" + File.separator + "MyPage.java")));
    assertThat(
        targetConfig.getPageObjectTestClassPath(type),
        is(equalTo(currentDir + File.separator + "utam" + File.separator + "MyPageTests.java")));
    assertThat(targetConfig.getLintReportPath(), is(equalTo(currentDir)));
  }

  private static PageObjectDeclaration mockPageObject(
      String interfaceTypeName,
      String classTypeName,
      boolean isInterfaceOnly,
      boolean isClassWithInterface) {
    PageObjectDeclaration object = mock(PageObjectDeclaration.class);
    PageObjectInterface intMock = mock(PageObjectInterface.class);
    when(intMock.getInterfaceType()).thenReturn(new FromString(interfaceTypeName));
    when(object.getInterface()).thenReturn(intMock);
    when(object.isInterfaceOnly()).thenReturn(isInterfaceOnly);
    when(object.isClassWithInterface()).thenReturn(isClassWithInterface);
    if (!isInterfaceOnly) {
      PageObjectClass classMock = mock(PageObjectClass.class);
      when(classMock.getClassType()).thenReturn(new FromString(classTypeName));
      when(object.getImplementation()).thenReturn(classMock);
    }
    return object;
  }

  private static String typeToPath(Path targetRoot, String fullName) {
    return targetRoot + File.separator + fullName.replace('.', File.separatorChar) + ".java";
  }

  private static void touch(Path file, long lastModifiedMillis) throws IOException {
    Files.createDirectories(file.getParent());
    if (!Files.exists(file)) {
      Files.createFile(file);
    }
    if (!file.toFile().setLastModified(lastModifiedMillis)) {
      throw new IOException("could not set last modified for " + file);
    }
  }

  private static DefaultTargetConfiguration newIncrementalTargetConfig(
      Path targetRoot, boolean incremental) {
    return new DefaultTargetConfiguration(
        targetRoot.toString(),
        targetRoot.toString(),
        targetRoot.toString(),
        UnitTestRunner.NONE,
        targetRoot.toString(),
        null,
        incremental);
  }

  @Test
  public void testIsUpToDateReturnsFalseWhenIncrementalDisabled() throws IOException {
    Path tmp = Files.createTempDirectory("utam-incremental");
    DefaultTargetConfiguration target = newIncrementalTargetConfig(tmp, false);
    PageObjectDeclaration object =
        mockPageObject(PAGE_OBJECT_INTERFACE_CLASS_NAME, PAGE_OBJECT_IMPL_CLASS_NAME, false, true);
    long now = System.currentTimeMillis();
    touch(Paths.get(typeToPath(tmp, PAGE_OBJECT_INTERFACE_CLASS_NAME)), now);
    touch(Paths.get(typeToPath(tmp, PAGE_OBJECT_IMPL_CLASS_NAME)), now);
    assertThat(target.isUpToDate(object, now - 5_000), is(equalTo(false)));
  }

  @Test
  public void testIsUpToDateReturnsFalseWhenSourceTimestampIsZero() throws IOException {
    Path tmp = Files.createTempDirectory("utam-incremental");
    DefaultTargetConfiguration target = newIncrementalTargetConfig(tmp, true);
    PageObjectDeclaration object =
        mockPageObject(PAGE_OBJECT_INTERFACE_CLASS_NAME, PAGE_OBJECT_IMPL_CLASS_NAME, false, true);
    assertThat(target.isUpToDate(object, 0L), is(equalTo(false)));
  }

  @Test
  public void testIsUpToDateReturnsFalseWhenInterfaceFileMissing() throws IOException {
    Path tmp = Files.createTempDirectory("utam-incremental");
    DefaultTargetConfiguration target = newIncrementalTargetConfig(tmp, true);
    PageObjectDeclaration object =
        mockPageObject(PAGE_OBJECT_INTERFACE_CLASS_NAME, PAGE_OBJECT_IMPL_CLASS_NAME, false, true);
    long now = System.currentTimeMillis();
    touch(Paths.get(typeToPath(tmp, PAGE_OBJECT_IMPL_CLASS_NAME)), now);
    assertThat(target.isUpToDate(object, now - 5_000), is(equalTo(false)));
  }

  @Test
  public void testIsUpToDateReturnsFalseWhenClassFileMissing() throws IOException {
    Path tmp = Files.createTempDirectory("utam-incremental");
    DefaultTargetConfiguration target = newIncrementalTargetConfig(tmp, true);
    PageObjectDeclaration object =
        mockPageObject(PAGE_OBJECT_INTERFACE_CLASS_NAME, PAGE_OBJECT_IMPL_CLASS_NAME, false, true);
    long now = System.currentTimeMillis();
    touch(Paths.get(typeToPath(tmp, PAGE_OBJECT_INTERFACE_CLASS_NAME)), now);
    assertThat(target.isUpToDate(object, now - 5_000), is(equalTo(false)));
  }

  @Test
  public void testIsUpToDateReturnsFalseWhenSourceNewerThanTargets() throws IOException {
    Path tmp = Files.createTempDirectory("utam-incremental");
    DefaultTargetConfiguration target = newIncrementalTargetConfig(tmp, true);
    PageObjectDeclaration object =
        mockPageObject(PAGE_OBJECT_INTERFACE_CLASS_NAME, PAGE_OBJECT_IMPL_CLASS_NAME, false, true);
    long base = System.currentTimeMillis();
    touch(Paths.get(typeToPath(tmp, PAGE_OBJECT_INTERFACE_CLASS_NAME)), base - 10_000);
    touch(Paths.get(typeToPath(tmp, PAGE_OBJECT_IMPL_CLASS_NAME)), base - 10_000);
    assertThat(target.isUpToDate(object, base), is(equalTo(false)));
  }

  @Test
  public void testIsUpToDateReturnsTrueWhenTargetsNewerThanSource() throws IOException {
    Path tmp = Files.createTempDirectory("utam-incremental");
    DefaultTargetConfiguration target = newIncrementalTargetConfig(tmp, true);
    PageObjectDeclaration object =
        mockPageObject(PAGE_OBJECT_INTERFACE_CLASS_NAME, PAGE_OBJECT_IMPL_CLASS_NAME, false, true);
    long base = System.currentTimeMillis();
    touch(Paths.get(typeToPath(tmp, PAGE_OBJECT_INTERFACE_CLASS_NAME)), base);
    touch(Paths.get(typeToPath(tmp, PAGE_OBJECT_IMPL_CLASS_NAME)), base);
    assertThat(target.isUpToDate(object, base - 5_000), is(equalTo(true)));
  }

  @Test
  public void testIsUpToDateInterfaceOnlyPageObject() throws IOException {
    Path tmp = Files.createTempDirectory("utam-incremental");
    DefaultTargetConfiguration target = newIncrementalTargetConfig(tmp, true);
    PageObjectDeclaration object = mockPageObject(INTERFACE_ONLY_CLASS_NAME, null, true, true);
    long base = System.currentTimeMillis();
    touch(Paths.get(typeToPath(tmp, INTERFACE_ONLY_CLASS_NAME)), base);
    assertThat(target.isUpToDate(object, base - 5_000), is(equalTo(true)));
  }

  @Test
  public void testIsUpToDateImplOnlyPageObject() throws IOException {
    // Impl-only PO ("implements" referencing an interface from another module) should only need
    // its own impl class file to be up-to-date; the external interface is owned elsewhere.
    Path tmp = Files.createTempDirectory("utam-incremental");
    DefaultTargetConfiguration target = newIncrementalTargetConfig(tmp, true);
    PageObjectDeclaration object =
        mockPageObject(INTERFACE_ONLY_CLASS_NAME, IMPL_ONLY_CLASS_NAME, false, false);
    long base = System.currentTimeMillis();
    touch(Paths.get(typeToPath(tmp, IMPL_ONLY_CLASS_NAME)), base);
    assertThat(target.isUpToDate(object, base - 5_000), is(equalTo(true)));
  }

  @Test
  public void testConstructorForDistribution() {
    String currentDir = System.getProperty("user.dir");
    DefaultTargetConfiguration targetConfig =
        new DefaultTargetConfiguration(currentDir, currentDir, currentDir);
    assertThat(targetConfig.getUnitTestRunnerType(), is(equalTo(UnitTestRunner.NONE)));
    assertThat(targetConfig.getInjectionConfigRootFilePath(), is(equalTo(currentDir)));
    String typeName = "utam/MyPage";
    TypeProvider type = new FromString(typeName);
    assertThat(
        targetConfig.getPageObjectClassPath(type),
        is(equalTo((currentDir + "/utam/MyPage.java").replace("/", File.separator))));
    assertThat(
        targetConfig.getPageObjectTestClassPath(type),
        is(equalTo((currentDir + "/utam/MyPageTests.java").replace("/", File.separator))));
  }

  public static class Mock implements TranslatorTargetConfig {

    final Map<String, Writer> writers = new HashMap<>();
    private final UnitTestRunner unitTestRunnerType;
    private String configPath;

    Mock(UnitTestRunner unitTestRunnerType) {
      this.unitTestRunnerType = unitTestRunnerType;
    }

    public Mock() {
      this(UnitTestRunner.NONE);
    }

    DefaultTranslatorRunner getRunner() {
      return new DefaultTranslatorRunner(getConfig());
    }

    TranslatorConfig getConfig() {
      DefaultSourceConfigurationTests.Mock sources = new DefaultSourceConfigurationTests.Mock();
      sources.setSources();
      return new TranslatorConfigWithProfile(sources, this);
    }

    @Override
    public UnitTestRunner getUnitTestRunnerType() {
      return unitTestRunnerType;
    }

    @Override
    public Writer getClassWriter(TypeProvider typeProvider) {
      Writer poWriter = new StringWriterMock();
      writers.put(typeProvider.getFullName(), poWriter);
      return poWriter;
    }

    @Override
    public Writer getResourceWriter(String pageObjectUri) {
      Writer resourceWriter = new StringWriterMock();
      writers.put(pageObjectUri, resourceWriter);
      return resourceWriter;
    }

    @Override
    public Writer getUnitTestWriter(TypeProvider typeProvider) throws IOException {
      Writer unitTestWriter = new StringWriterMock();
      writers.put(typeProvider.getFullName() + "Tests", unitTestWriter);
      return unitTestWriter;
    }

    @Override
    public String getInjectionConfigRootFilePath() {
      return configPath;
    }

    @Override
    public String getLintReportPath() {
      return configPath;
    }

    final void setConfigPath(String path) {
      this.configPath = path;
    }

    @Override
    public String getErrorsReportPath() {
      return null;
    }
  }

  private static class UnitTestWriterThrowsError extends Mock {

    UnitTestWriterThrowsError(UnitTestRunner unitTestRunnerType) {
      super(unitTestRunnerType);
    }

    @Override
    public Writer getUnitTestWriter(TypeProvider typeProvider) {
      Writer unitTestWriter = new WriterThrowsIOException();
      writers.put(typeProvider.getFullName() + "Tests", unitTestWriter);
      return unitTestWriter;
    }
  }

  private static class UnitTestWriterNotConfigured extends Mock {

    private static final String ERROR = "no unit test writer created in runner configuration";

    UnitTestWriterNotConfigured(UnitTestRunner unitTestRunnerType) {
      super(unitTestRunnerType);
    }

    @Override
    public Writer getUnitTestWriter(TypeProvider typeProvider) throws IOException {
      throw new IOException(ERROR);
    }
  }

  static class ClassWriterError extends Mock {

    @Override
    public Writer getClassWriter(TypeProvider typeProvider) {
      Writer poWriter = new WriterThrowsIOException();
      writers.put(typeProvider.getFullName(), poWriter);
      return poWriter;
    }
  }

  static class WriterThrowsIOException extends Writer {

    @SuppressWarnings("NullableProblems")
    @Override
    public void write(char[] cbuf, int off, int len) throws IOException {
      throw new IOException(FAKE_IO_EXCEPTION_MESSAGE);
    }

    @Override
    public void flush() {}

    @Override
    public void close() {}
  }

  static class StringWriterMock extends Writer {

    private final StringBuilder written = new StringBuilder();

    @Override
    public String toString() {
      return written.toString();
    }

    @SuppressWarnings("NullableProblems")
    @Override
    public void write(char[] cbuf, int off, int len) {
      written.append(String.copyValueOf(cbuf, off, len));
    }

    @Override
    public void flush() {}

    @Override
    public void close() {}
  }

  private static class NullUnitTestWriter extends Mock {

    NullUnitTestWriter(UnitTestRunner unitTestRunnerType) {
      super(unitTestRunnerType);
    }

    @Override
    public Writer getUnitTestWriter(TypeProvider typeProvider) {
      return null;
    }
  }
}
