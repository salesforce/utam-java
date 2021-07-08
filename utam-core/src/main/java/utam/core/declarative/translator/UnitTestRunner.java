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
  NONE,
  JUNIT,
  TESTNG;

  public static UnitTestRunner fromString(String unitTestRunner) {
    UnitTestRunner runner;
    if (unitTestRunner != null && !unitTestRunner.isEmpty()) {
      try {
        runner = valueOf(unitTestRunner.toUpperCase());
      } catch (IllegalArgumentException iae) {
        UtamLogger.error(String.format("Unsupported runner type: '%s' is not supported, " +
                "check your configuration", unitTestRunner));
        throw new UtamError(iae.getMessage());
      }
    } else {
      UtamLogger.error("Illegal argument: The String is null or empty, check your input");
      throw new UtamError("Illegal argument, cannot continue");
    }
    return runner;
  }

  public static String validateUnitTestDirectory(UnitTestRunner unitTestRunner, String unitTestDirectory) {
    if(unitTestRunner == null || unitTestRunner == NONE) {
      return "";
    }
    return unitTestDirectory;
  }
}
