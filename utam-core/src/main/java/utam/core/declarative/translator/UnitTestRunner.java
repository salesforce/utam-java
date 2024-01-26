/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.translator;

import utam.core.framework.UtamLogger;
import utam.core.framework.consumer.UtamError;

/**
 * type of the unit test runner used by unit test generator
 *
 * @author jim evans
 * @since 226
 */
public enum UnitTestRunner {
  /** Do not use a unit test runner */
  NONE,

  /** Generated unit test to use JUnit */
  JUNIT,

  /** Generated unit tests to use TestNG */
  TESTNG;

  /**
   * Gets the appropriate UnitTestRunner value from a string
   *
   * @param unitTestRunner the string to interpret
   * @return the UnitTestRunner value represented by the string
   */
  public static UnitTestRunner fromString(String unitTestRunner) {
    UnitTestRunner runner;
    if (unitTestRunner != null && !unitTestRunner.isEmpty()) {
      try {
        runner = valueOf(unitTestRunner.toUpperCase());
      } catch (IllegalArgumentException iae) {
        UtamLogger.error(
            String.format(
                "Unsupported runner type: '%s' is not supported, " + "check your configuration",
                unitTestRunner));
        throw new UtamError(iae.getMessage());
      }
    } else {
      UtamLogger.error("Illegal argument: The String is null or empty, check your input");
      throw new UtamError("Illegal argument, cannot continue");
    }
    return runner;
  }

  /**
   * Validates that the unit test directory is correct
   *
   * @param unitTestRunner the unit test runner to use
   * @param unitTestDirectory the directory containing unit tests
   * @return the unit test directory
   */
  public static String validateUnitTestDirectory(
      UnitTestRunner unitTestRunner, String unitTestDirectory) {
    if (unitTestRunner == null || unitTestRunner == NONE) {
      return "";
    }
    return unitTestDirectory;
  }
}
