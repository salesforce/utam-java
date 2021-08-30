/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import utam.compiler.grammar.UtamArgument;
import utam.core.declarative.representation.TypeProvider;

/**
 * Draggable element action types
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public enum DraggableActionType implements ActionType {
  /**
   * drag and drop current element to the location provided as an element parameter
   */
  dragAndDrop,

  /**
   * drag and drop current element to the location provided with coordinates
   */
  dragAndDropByOffset;

  private static final List<List<TypeProvider>> POSSIBLE_DRAG_AND_DROP_PARAMETERS = new ArrayList<>();
  private static final List<List<TypeProvider>> POSSIBLE_DRAG_AND_DROP_OFFSET_PARAMETERS = new ArrayList<>();
  static final String ERR_MULTIPLE_EXPECTED_ARGS_TYPES = "Draggable method supports more than one parameters combination";

  static {
    // with element
    final List<TypeProvider> ELEMENT_PARAMETERS = Collections
        .singletonList(TypeUtilities.BASIC_ELEMENT);
    final List<TypeProvider> ELEMENT_WITH_DURATION_PARAMETERS = new ArrayList<>(
        ELEMENT_PARAMETERS);
    ELEMENT_WITH_DURATION_PARAMETERS.add(PrimitiveType.NUMBER);
    POSSIBLE_DRAG_AND_DROP_PARAMETERS.add(ELEMENT_PARAMETERS);
    POSSIBLE_DRAG_AND_DROP_PARAMETERS.add(ELEMENT_WITH_DURATION_PARAMETERS);

    // with offset
    final List<TypeProvider> OFFSET_PARAMETERS = new ArrayList<>();
    OFFSET_PARAMETERS.add(PrimitiveType.NUMBER);
    OFFSET_PARAMETERS.add(PrimitiveType.NUMBER);
    final List<TypeProvider> OFFSET_WITH_DURATION_PARAMETERS = new ArrayList<>(OFFSET_PARAMETERS);
    OFFSET_WITH_DURATION_PARAMETERS.add(PrimitiveType.NUMBER);
    POSSIBLE_DRAG_AND_DROP_OFFSET_PARAMETERS.add(OFFSET_PARAMETERS);
    POSSIBLE_DRAG_AND_DROP_OFFSET_PARAMETERS.add(OFFSET_WITH_DURATION_PARAMETERS);
  }

  @Override
  public TypeProvider getReturnType() {
    return VOID;
  }

  @Override
  public List<TypeProvider> getParametersTypes() {
    throw new IllegalStateException(ERR_MULTIPLE_EXPECTED_ARGS_TYPES);
  }

  @Override
  public List<List<TypeProvider>> getParametersTypesOptions() {
    if(this == dragAndDropByOffset) {
      return POSSIBLE_DRAG_AND_DROP_OFFSET_PARAMETERS;
    }
    return POSSIBLE_DRAG_AND_DROP_PARAMETERS;
  }

  @Override
  public String getApplyString() {
    return this.name();
  }

  @Override
  public UtamArgument[] getTransformedArgs(UtamArgument[] args) {
    if (this == dragAndDrop && args.length == 1) {
      // add default duration as 0
      return new UtamArgument[]{args[0], new UtamArgument.UtamArgumentLiteral(0)};
    }
    if (this == dragAndDropByOffset && args.length == 2) {
      // add default duration as 0
      return new UtamArgument[]{args[0], args[1], new UtamArgument.UtamArgumentLiteral(0)};
    }
    return args;
  }
}
