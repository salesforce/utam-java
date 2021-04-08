/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.representation;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public interface PageObjectDeclaration {

  boolean isInterfaceOnly();

  boolean isClassWithInterface();

  boolean isClassWithProfiles();

  PageObjectClass getImplementation();

  PageObjectInterface getInterface();
}
