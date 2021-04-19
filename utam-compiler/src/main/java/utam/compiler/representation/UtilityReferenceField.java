/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

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
public final class UtilityReferenceField implements PageClassField {

  private final String name;
  private final TypeProvider type;

  public UtilityReferenceField(TypeProvider type) {
    this.name = "util" + type.getSimpleName();
    this.type = type;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public List<AnnotationProvider> getAnnotations() {
    return Collections.EMPTY_LIST;
  }

  @Override
  public TypeProvider getType() {
    return type;
  }

  @Override
  public String getDeclaration() {
    String typeName = getType().getSimpleName();
    return String
        .format("private final %s %s = getUtility(%s.class)", typeName, getName(), typeName);
  }
}
