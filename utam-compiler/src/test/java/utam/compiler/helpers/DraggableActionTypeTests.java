/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.compiler.helpers.BasicElementActionType.getActionType;
import static utam.compiler.helpers.DraggableActionType.ERR_MULTIPLE_EXPECTED_ARGS_TYPES;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.BasicElementInterface.draggable;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.lang.reflect.Method;
import java.util.List;
import org.testng.annotations.Test;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Draggable;

/**
 * tests for draggable action types
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class DraggableActionTypeTests {

  @Test
  public void checkEachDraggableMethodHasActionType() {
    for (Method method : Draggable.class.getDeclaredMethods()) {
      DraggableActionType.valueOf(method.getName());
    }
  }

  @Test
  public void testDragAndDrop() {
    DraggableActionType action = DraggableActionType.dragAndDrop;
    assertThat(action.getReturnType().isSameType(VOID), is(true));
    assertThat(action.getApplyString(), is(equalTo(action.name())));

    List<List<TypeProvider>> parametersOptions = action.getParametersTypesOptions();
    assertThat(parametersOptions, hasSize(2));
    //1
    List<TypeProvider> parameters = parametersOptions.get(0);
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).isSameType(BASIC_ELEMENT), is(true));
    //2
    parameters = parametersOptions.get(1);
    assertThat(parameters, hasSize(2));
    assertThat(parameters.get(0).isSameType(BASIC_ELEMENT), is(true));
    assertThat(parameters.get(1).isSameType(PrimitiveType.NUMBER), is(true));
  }

  @Test
  public void testDragAndDropOffset() {
    DraggableActionType action = DraggableActionType.dragAndDropByOffset;
    assertThat(action.getReturnType().isSameType(VOID), is(true));
    assertThat(action.getApplyString(), is(equalTo(action.name())));

    List<List<TypeProvider>> parametersOptions = action.getParametersTypesOptions();
    assertThat(parametersOptions, hasSize(2));
    //1
    List<TypeProvider> parameters = parametersOptions.get(0);
    assertThat(parameters, hasSize(2));
    assertThat(parameters.get(0).isSameType(PrimitiveType.NUMBER), is(true));
    assertThat(parameters.get(1).isSameType(PrimitiveType.NUMBER), is(true));
    //2
    parameters = parametersOptions.get(1);
    assertThat(parameters, hasSize(3));
    assertThat(parameters.get(0).isSameType(PrimitiveType.NUMBER), is(true));
    assertThat(parameters.get(1).isSameType(PrimitiveType.NUMBER), is(true));
    assertThat(parameters.get(2).isSameType(PrimitiveType.NUMBER), is(true));
  }

  @Test
  public void testGetActionFromString() {
    DraggableActionType action = DraggableActionType.dragAndDrop;
    assertThat(getActionType(action.name(), draggable, "name"), is(equalTo(action)));
  }

  @Test
  public void testGetParameterTypesThrows() {
    ActionType action = DraggableActionType.dragAndDropByOffset;
    IllegalStateException e = expectThrows(IllegalStateException.class, action::getParametersTypes);
    assertThat(e.getMessage(), is(equalTo(ERR_MULTIPLE_EXPECTED_ARGS_TYPES)));
  }
}
