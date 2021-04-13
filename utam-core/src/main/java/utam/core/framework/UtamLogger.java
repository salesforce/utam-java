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

public interface UtamLogger {

  Logger UTAM_LOGGER = Logger.getLogger("utam");

  static void info(String message) {
    if (message != null && !message.isEmpty()) {
      UTAM_LOGGER.info(message);
    }
  }

  static void info(List<String> messages) {
    messages.forEach(UtamLogger::info);
  }

  static void log(Level priority, String message) {
    if (message != null && !message.isEmpty()) {
      UTAM_LOGGER.log(priority, message);
    }
  }

  static void error(Throwable t) {
    UTAM_LOGGER.error(t);
  }

  static void error(String message) {
    if (message != null && !message.isEmpty()) {
      UTAM_LOGGER.error(message);
    }
  }

  static void warning(String message) {
    if (message != null && !message.isEmpty()) {
      UTAM_LOGGER.warn(message);
    }
  }
}
