/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import java.util.List;

/**
 * Logger for UTAM operations
 */
public interface UtamLogger {

  /**
   * Logger used to log messages
   */
  Logger UTAM_LOGGER = Logger.getLogger("utam");

  /**
   * Log message at info level
   *
   * @param message message to log
   */
  static void info(String message) {
    if (message != null && !message.isEmpty()) {
      UTAM_LOGGER.info(message);
    }
  }

  /**
   * Log a set of messages at info level
   *
   * @param messages messages to log
   */
  static void info(List<String> messages) {
    messages.forEach(UtamLogger::info);
  }

  /**
   * Log a message at an arbitrary level
   *
   * @param priority level to use to log the message
   * @param message  messages to log
   */
  static void log(Level priority, String message) {
    if (message != null && !message.isEmpty()) {
      UTAM_LOGGER.log(priority, message);
    }
  }

  /**
   * Log an exception at error level
   *
   * @param t exception to log
   */
  static void error(Throwable t) {
    UTAM_LOGGER.error(t);
  }

  /**
   * Log message at error level
   *
   * @param message message to log
   */
  static void error(String message) {
    if (message != null && !message.isEmpty()) {
      UTAM_LOGGER.error(message);
    }
  }

  /**
   * Log message at warning level
   *
   * @param message message to log
   */
  static void warning(String message) {
    if (message != null && !message.isEmpty()) {
      UTAM_LOGGER.warn(message);
    }
  }
}
