/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.UtamCompilationError;
import utam.core.declarative.representation.TypeProvider;

/**
 * Draggable element action types
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public enum DraggableActionType implements ActionType {
  /** drag and drop current element to the location provided as an element parameter */
  dragAndDrop,

  /** drag and drop current element to the location provided with coordinates */
  dragAndDropByOffset;

  private static final List<TypeProvider> ELEMENT_PARAMETERS =
      Collections.singletonList(BASIC_ELEMENT);
  private static final List<TypeProvider> ELEMENT_WITH_DURATION_PARAMETERS =
      Stream.of(BASIC_ELEMENT, PrimitiveType.NUMBER).collect(Collectors.toList());
  private static final List<TypeProvider> OFFSET_PARAMETERS =
      Stream.of(PrimitiveType.NUMBER, PrimitiveType.NUMBER).collect(Collectors.toList());
  private static final List<TypeProvider> OFFSET_WITH_DURATION_PARAMETERS =
      Stream.of(PrimitiveType.NUMBER, PrimitiveType.NUMBER, PrimitiveType.NUMBER)
          .collect(Collectors.toList());

  @Override
  public TypeProvider getReturnType() {
    return VOID;
  }

  @Override
  public List<TypeProvider> getParametersTypes(String parserContext, int parametersCount) {
    if (this == dragAndDropByOffset) {
      if (parametersCount == 3) {
        return OFFSET_WITH_DURATION_PARAMETERS;
      }
      if (parametersCount == 2) {
        return OFFSET_PARAMETERS;
      }
    } else {
      if (parametersCount == 2) {
        return ELEMENT_WITH_DURATION_PARAMETERS;
      }
      if (parametersCount == 1) {
        return ELEMENT_PARAMETERS;
      }
    }
    String contextStr = String.format("%s action \"%s\"", parserContext, this.name());
    throw new UtamCompilationError(
        VALIDATION.getErrorMessage(
            108, contextStr, "(check documentation)", String.valueOf(parametersCount)));
  }

  @Override
  public String getApplyString() {
    return this.name();
  }
}
