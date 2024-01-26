/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.translator;

import utam.core.declarative.representation.TypeProvider;

/**
 * creates type of PO based on URI
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface TranslationTypesConfig {

  /**
   * generate class type from URI
   *
   * @param pageObjectURI unique PO URI
   * @return type for class
   */
  TypeProvider getClassType(String pageObjectURI);

  /**
   * generate interface type from URI
   *
   * @param pageObjectURI po uri
   * @return type of the interface
   */
  TypeProvider getInterfaceType(String pageObjectURI);

  /**
   * get utility type
   *
   * @param utilityURI string with utility type name
   * @return type of the utility
   */
  TypeProvider getUtilityType(String utilityURI);
}
