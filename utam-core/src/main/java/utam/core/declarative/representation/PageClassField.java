/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.representation;

import java.util.List;

/**
 * class field <br>
 * cab be elements and inner components
 *
 * @author elizaveta.ivanova
 * @since 224
 */
public interface PageClassField {

  /**
   * name of the field
   *
   * @return name of element
   */
  String getName();

  /**
   * list of field annotations
   *
   * @return field annotations
   */
  List<AnnotationProvider> getAnnotations();

  /**
   * type provider of field
   *
   * @return field type provider
   */
  TypeProvider getType();

  /**
   * declaration of the field
   *
   * @return declaration of the field
   */
  String getDeclaration();
}
