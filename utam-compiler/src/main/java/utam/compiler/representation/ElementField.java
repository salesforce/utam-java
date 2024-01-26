/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.TypeUtilities.ELEMENT_FIELD;

import java.util.Collections;
import java.util.List;
import utam.core.declarative.representation.AnnotationProvider;
import utam.core.declarative.representation.PageClassField;
import utam.core.declarative.representation.TypeProvider;

/**
 * representation of the page object element
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public final class ElementField implements PageClassField {

  private final String name;
  private final List<AnnotationProvider> annotations;

  /**
   * Initializes a new instance of the ElementField class
   *
   * @param name the name of the element
   * @param annotation the annotation provider for generating the field annotation
   */
  public ElementField(String name, AnnotationProvider annotation) {
    this.name = name;
    this.annotations = Collections.singletonList(annotation);
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public List<AnnotationProvider> getAnnotations() {
    return annotations;
  }

  @Override
  public TypeProvider getType() {
    return ELEMENT_FIELD;
  }

  @Override
  public String getDeclaration() {
    return String.format("private %s %s", getType().getSimpleName(), getName());
  }
}
