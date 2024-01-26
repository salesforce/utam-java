/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework;

import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.event.Level;

/** Logger for UTAM operations */
public class UtamLogger {

  /** Logger used to log messages */
  static Logger UTAM_LOGGER = null;

  /**
   * Static initializer used to create logger and set property for formatting log messages. There
   * may be other ways to accomplish this without using such a little-used language construct. Those
   * should be investigated for the best approach. Note that for further control over formatting of
   * log messages, one might extend the Formatter class, and set the property to use that class for
   * formatting log messages.
   */
  static {
    System.setProperty(
        "java.util.logging.SimpleFormatter.format", "%1$tT %3$s %4$-7s: %5$s %6$s%n");
    UTAM_LOGGER = LoggerFactory.getLogger("utam");
  }

  /**
   * Log message at info level
   *
   * @param message message to log
   */
  public static void info(String message) {
    if (message != null && !message.isEmpty()) {
      UTAM_LOGGER.info(message);
    }
  }

  /**
   * Log a set of messages at info level
   *
   * @param messages messages to log
   */
  public static void info(List<String> messages) {
    messages.forEach(UtamLogger::info);
  }

  /**
   * Log a message at an arbitrary level
   *
   * @param priority level to use to log the message
   * @param message messages to log
   */
  public static void log(Level priority, String message) {
    if (message != null && !message.isEmpty()) {
      if (priority == Level.INFO) {
        UTAM_LOGGER.info(message);
      } else if (priority == Level.DEBUG) {
        UTAM_LOGGER.debug(message);
      } else if (priority == Level.ERROR) {
        UTAM_LOGGER.error(message);
      } else if (priority == Level.TRACE) {
        UTAM_LOGGER.trace(message);
      }
    }
  }

  /**
   * Log an exception at error level
   *
   * @param t exception to log
   */
  public static void error(Throwable t) {
    UTAM_LOGGER.error("Unexpected exception", t);
  }

  /**
   * Log message at error level
   *
   * @param message message to log
   */
  public static void error(String message) {
    if (message != null && !message.isEmpty()) {
      UTAM_LOGGER.error(message);
    }
  }

  /**
   * Log message at warning level
   *
   * @param message message to log
   */
  public static void warning(String message) {
    if (message != null && !message.isEmpty()) {
      UTAM_LOGGER.warn(message);
    }
  }
}
