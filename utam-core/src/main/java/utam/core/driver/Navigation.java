/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.driver;

/**
 * Navigation interface exposing commands to navigate browsing web and mobile contexts
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public interface Navigation {

  /**
   * Command to navigate one step backward in the browsing context
   */
  void back();

  /**
   * Command to navigate one step forward in the browsing context
   */
  void forward();
}
