/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import utam.core.declarative.representation.AnnotationProvider;
import utam.core.declarative.representation.PageClassField;
import utam.core.declarative.representation.TypeProvider;

import java.util.List;

/**
 * representation of the page object element
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public final class ElementField implements PageClassField {

  private final String name;
  private final List<AnnotationProvider> annotations;
  private final TypeProvider type;
  private final String comments;

  public ElementField(String name, TypeProvider type, List<AnnotationProvider> annotations, String comments) {
    this.name = name;
    this.annotations = annotations;
    this.type = type;
    this.comments = comments;
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
    return type;
  }

  @Override
  public String getDeclaration() {
    return String.format("private %s %s", getType().getSimpleName(), getName());
  }

  @Override
  public String getComments() {
    return comments;
  }
}
