/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.utilities;

public class ThrowingConstructorFactory extends TestObjectFactory {
  public ThrowingConstructorFactory() {
    throw new RuntimeException("constructor exception");
  }
}
