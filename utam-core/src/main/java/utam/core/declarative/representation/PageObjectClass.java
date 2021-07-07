/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.representation;

import java.util.List;
import utam.core.declarative.translator.UnitTestRunner;
import utam.core.framework.context.Profile;

/**
 * representation of PO class to generate
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface PageObjectClass {

  /**
   * class fields for internal page elements NEEDS TO BE ORDERED
   *
   * @return list of PO elements
   */
  List<PageClassField> getFields();

  /**
   * list of methods including component getters
   *
   * @return all methods
   */
  List<PageObjectMethod> getMethods();

  /**
   * class level annotations such as selector, shadow host, place...
   *
   * @return annotations at class level
   */
  List<AnnotationProvider> getClassAnnotations();

  /**
   * class type
   *
   * @return self type
   */
  TypeProvider getClassType();

  /**
   * PO can extend from other PO class <br>
   * by default extending BasePageObject
   *
   * @return type to extend
   */
  TypeProvider getBaseClassType();

  String getImplCode();
  
  String getGeneratedUnitTestCode(UnitTestRunner unitTestRunner);

  PageObjectInterface getImplementedType();

  Profile[] getProfiles();

  String getComments();

  List<TypeProvider> getDeclaredElementTypes(boolean isPublicElements);
}
