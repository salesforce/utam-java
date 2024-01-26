/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static utam.compiler.translator.DefaultTargetConfiguration.getWriterWithDir;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import utam.core.declarative.errors.CompilerErrorsConfig;
import utam.core.declarative.errors.CompilerErrorsContext;
import utam.core.declarative.errors.CompilerErrorsContext.CompilerError;
import utam.core.framework.UtamLogger;

/**
 * Class with possible configurations and contexts for compiler errors
 *
 * @author elizaveta.ivanova
 * @since 244
 */
public abstract class CompilerErrors {

  static final String ERR_REPORT_FILE = "utam.errors.txt";

  /**
   * By default, compiler should throw an error for the first incorrect page object
   *
   * @author elizaveta.ivanova
   * @since 244
   */
  public static class Throws implements CompilerErrorsConfig {

    @Override
    public CompilerErrorsContext newContext() {
      return new Inactive();
    }
  }

  /**
   * If configured, conpiler will collect errors for all page objects into a single report
   *
   * @author elizaveta.ivanova
   * @since 244
   */
  static class Report implements CompilerErrorsConfig {

    private final String absoluteErrorsReportPath;

    Report(String absoluteErrorsReportPath) {
      this.absoluteErrorsReportPath = absoluteErrorsReportPath;
    }

    @Override
    public boolean isInterrupt() {
      return false;
    }

    @Override
    public CompilerErrorsContext newContext() {
      return new Active();
    }

    @Override
    public String report(CompilerErrorsContext context) {
      List<CompilerError> errors = context.getCompilerReport();
      if (!errors.isEmpty()) {
        UtamLogger.info(
            String.format("Writing compiler errors report to %s", absoluteErrorsReportPath));
        String report =
            errors.stream()
                .map(err -> String.format("____________________________\n%s", err.toString()))
                .collect(Collectors.joining("\n"));
        UtamLogger.error(report);
        Writer writer;
        try {
          writer = getWriterWithDir(absoluteErrorsReportPath);
          writer.write(report);
          writer.flush();
          return report;
        } catch (IOException e) {
          throw new IllegalStateException("Could not write compiler errors report", e);
        }
      }
      return null;
    }
  }

  /**
   * If combining errors is not configured, runner just throws exception
   *
   * @author elizaveta.ivanova
   * @since 244
   */
  static class Inactive implements CompilerErrorsContext {}

  /**
   * If combining errors is configured, runner collects all errors
   *
   * @author elizaveta.ivanova
   * @since 244
   */
  static class Active implements CompilerErrorsContext {

    private final List<CompilerError> errors = new ArrayList<>();

    @Override
    public void setError(CompilerError error) {
      errors.add(error);
    }

    @Override
    public List<CompilerError> getCompilerReport() {
      return errors;
    }
  }

  /**
   * Simple compilation error
   *
   * @author elizaveta.ivanova
   * @since 244
   */
  public static class StringError implements CompilerError {
    private final String message;

    public StringError(String message) {
      this.message = message;
    }

    @Override
    public String toString() {
      return message;
    }
  }
}
