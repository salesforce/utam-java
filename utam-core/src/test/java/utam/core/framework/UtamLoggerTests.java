/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework;

import java.util.Arrays;
import java.util.List;
import org.slf4j.event.Level;
import org.testng.annotations.Test;

/**
 * Provides tests for the UtamLogger class
 *
 * @author james.evans
 */
public class UtamLoggerTests {

  /** The static info method should not throw an exception */
  @Test
  public void testInfo() {
    UtamLogger.info("test message");
  }

  /** The static info method should not throw an exception with a null message */
  @Test
  public void testInfoWithNullMessage() {
    UtamLogger.info((String) null);
  }

  /** The static info method should not throw an exception with an empty message */
  @Test
  public void testInfoWithEmptyMessage() {
    UtamLogger.info("");
  }

  /** The static info method should not throw an exception with list of messages */
  @Test
  public void testInfoWithStringList() {
    List<String> messages = Arrays.asList("first test message", "second test message");
    UtamLogger.info(messages);
  }

  /** The static warning method should not throw an exception */
  @Test
  public void testWarning() {
    UtamLogger.warning("test message");
  }

  /** The static warning method should not throw an exception with a null message */
  @Test
  public void testWarningWithNullMessage() {
    UtamLogger.warning(null);
  }

  /** The static warning method should not throw an exception with an empty message */
  @Test
  public void testWarningWithEmptyMessage() {
    UtamLogger.warning("");
  }

  /** The static error method should not throw an exception */
  @Test
  public void testError() {
    UtamLogger.error("test message");
  }

  /** The static error method should not throw an exception with a null message */
  @Test
  public void testErrorWithNullMessage() {
    UtamLogger.error((String) null);
  }

  /** The static error method should not throw an exception with an empty message */
  @Test
  public void testErrorWithEmptyMessage() {
    UtamLogger.error("");
  }

  /** The static error method should not throw an exception with an exception */
  @Test
  public void testErrorWithExceptionObject() {
    UtamLogger.error(new UnsupportedOperationException("test message"));
  }

  /** The static log method should not throw an exception */
  @Test
  public void testLog() {
    UtamLogger.log(Level.INFO, "test message");
  }

  /** The static log method should not throw an exception with a null message */
  @Test
  public void testLogWithNullMessage() {
    UtamLogger.log(Level.INFO, null);
  }

  /** The static error method should not throw an exception with an empty message */
  @Test
  public void testLogWithEmptyMessage() {
    UtamLogger.log(Level.INFO, "");
  }
}
