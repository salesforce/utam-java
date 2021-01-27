package utam.compiler.translator;

import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.declarative.translator.TranslatorRunner;
import utam.core.declarative.translator.TranslatorTargetConfig;
import utam.core.declarative.translator.UnitTestRunner;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;

import static utam.compiler.translator.DefaultTranslatorRunner.ERR_PROFILE_PATH_DOES_NOT_EXIST;
import static utam.compiler.translator.DefaultTranslatorRunner.ERR_PROFILE_PATH_NOT_CONFIGURED;
import static utam.compiler.translator.TranslatorMockUtilities.*;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;

public class DefaultTargetConfigurationTests {

  private static final String FAKE_IO_EXCEPTION_MESSAGE = "throwing fake IO exception";

  @Test
  public void testProfilesOutputConfigErr() {
    Mock targetConfig = new Mock();
    TranslatorConfig translatorConfig = new AbstractTranslatorConfigurationTests.Mock(targetConfig);
    TranslatorRunner runner = new DefaultTranslatorRunnerTests.Mock(translatorConfig);
    UtamError e = expectThrows(UtamError.class, runner::writeDependenciesConfigs);
    assertThat(e.getMessage(), is(equalTo(ERR_PROFILE_PATH_NOT_CONFIGURED)));
    targetConfig.setConfigPath("");
    e = expectThrows(UtamError.class, runner::writeDependenciesConfigs);
    assertThat(e.getMessage(), is(equalTo(ERR_PROFILE_PATH_NOT_CONFIGURED)));
    targetConfig.setConfigPath("err.err");
    e = expectThrows(UtamError.class, runner::writeDependenciesConfigs);
    assertThat(
        e.getMessage(), is(equalTo(String.format(ERR_PROFILE_PATH_DOES_NOT_EXIST, "err.err"))));
  }

  @Test
  public void testWriteWithWriterErrorThrows() throws IOException {
    TranslatorRunner translator = new ClassWriterError().getRunner();
    translator.run();
    RuntimeException e = expectThrows(RuntimeException.class, translator::write);
    assertThat(e.getCause(), is(instanceOf(IOException.class)));
    assertThat(e.getCause().getMessage(), is(equalTo(FAKE_IO_EXCEPTION_MESSAGE)));
  }

  @Test
  public void testWriteWithUnitTestWriterErrorThrows() throws IOException {
    TranslatorRunner translator = new UnitTestWriterThrowsError().getRunner();
    translator.run();
    RuntimeException e = expectThrows(RuntimeException.class, translator::write);
    assertThat(e.getCause(), is(instanceOf(IOException.class)));
    assertThat(e.getCause().getMessage(), is(equalTo(FAKE_IO_EXCEPTION_MESSAGE)));
  }

  @Test
  public void testWriteWithNoDefinedUnitTestWriterThrows() throws IOException {
    TranslatorRunner translator = new UnitTestWriterNotConfigured().getRunner();
    translator.run();
    IOException e = expectThrows(IOException.class, translator::write);
    assertThat(e.getMessage(), is(equalTo(UnitTestWriterNotConfigured.ERROR)));
  }

  @Test
  public void testWriteWithNoDefinedUnitTestWriter() throws IOException {
    TranslatorRunner translator =
        new UnitTestWriterNotConfigured().getRunner(UnitTestRunner.NONE);
    translator.run();
    translator.write(); // nothing thrown because unit test writer is NONE
  }

  @Test
  public void testWriteWithNoUnitTestRunner() throws IOException {
    Mock configuration = new Mock();
    TranslatorConfig translatorConfig = configuration.getConfig(UnitTestRunner.NONE);
    TranslatorRunner translator = new DefaultTranslatorRunnerTests.Mock(translatorConfig);
    translator.run();
    translator.write();
    assertThat(configuration.writers.keySet(), hasSize(4));
    assertThat(
        configuration.writers.keySet(),
        containsInAnyOrder(
            PAGE_OBJECT_INTERFACE_CLASS_NAME,
            PAGE_OBJECT_IMPL_CLASS_NAME,
            INTERFACE_ONLY_CLASS_NAME,
            IMPL_ONLY_CLASS_NAME));
  }

  @Test
  public void testWriteWithNullUnitTestRunner() throws IOException {
    Mock configuration = new Mock();
    TranslatorConfig translatorConfig = configuration.getConfig(null);
    TranslatorRunner translator = new DefaultTranslatorRunnerTests.Mock(translatorConfig);
    translator.run();
    translator.write();
    assertThat(configuration.writers.keySet(), hasSize(4));
    assertThat(
        configuration.writers.keySet(),
        containsInAnyOrder(
            PAGE_OBJECT_INTERFACE_CLASS_NAME,
            PAGE_OBJECT_IMPL_CLASS_NAME,
            INTERFACE_ONLY_CLASS_NAME,
            IMPL_ONLY_CLASS_NAME));
  }

  @Test
  public void testWriteNullUnitTestRunner() throws IOException {
    Mock configuration = new NullUnitTestWriter();
    TranslatorConfig translatorConfig = configuration.getConfig(UnitTestRunner.TESTNG);
    TranslatorRunner translator = new DefaultTranslatorRunnerTests.Mock(translatorConfig);
    translator.run();
    translator.write();
    assertThat(configuration.writers.keySet(), hasSize(4));
    assertThat(
        configuration.writers.keySet(),
        containsInAnyOrder(
            PAGE_OBJECT_INTERFACE_CLASS_NAME,
            PAGE_OBJECT_IMPL_CLASS_NAME,
            INTERFACE_ONLY_CLASS_NAME,
            IMPL_ONLY_CLASS_NAME));
  }

  @Test
  public void testGetWriterError() {
    assertThrows(FileNotFoundException.class, () -> DefaultTargetConfiguration.getWriter(""));
  }

  static class Mock implements TranslatorTargetConfig {

    final Map<String, Writer> writers = new HashMap<>();
    private String configPath;

    TranslatorRunner getRunner() {
      return getRunner(UnitTestRunner.TESTNG);
    }

    TranslatorRunner getRunner(UnitTestRunner unitTestRunnerType) {
      return new DefaultTranslatorRunnerTests.Mock(getConfig(unitTestRunnerType));
    }

    TranslatorConfig getConfig(UnitTestRunner unitTestRunnerType) {
      DefaultSourceConfigurationTests.Mock sourceConfig =
          new DefaultSourceConfigurationTests.Mock();
      sourceConfig.setSources();
      return new AbstractTranslatorConfigurationTests.Mock(unitTestRunnerType, this, sourceConfig);
    }

    @Override
    public Writer getClassWriter(TypeProvider typeProvider) {
      Writer poWriter = new StringWriterMock();
      writers.put(typeProvider.getFullName(), poWriter);
      return poWriter;
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

    final void setConfigPath(String path) {
      this.configPath = path;
    }
  }

  private static class UnitTestWriterThrowsError extends Mock {

    @Override
    public Writer getUnitTestWriter(TypeProvider typeProvider) {
      Writer unitTestWriter = new WriterThrowsIOException();
      writers.put(typeProvider.getFullName() + "Tests", unitTestWriter);
      return unitTestWriter;
    }
  }

  private static class UnitTestWriterNotConfigured extends Mock {

    private static final String ERROR = "no unit test writer created in runner configuration";

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

    @Override
    public Writer getUnitTestWriter(TypeProvider typeProvider) {
      return null;
    }
  }
}
