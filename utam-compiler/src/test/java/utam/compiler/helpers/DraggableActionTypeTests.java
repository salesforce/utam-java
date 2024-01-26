/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
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

    List<TypeProvider> parameters = action.getParametersTypes("test", 1);
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).isSameType(BASIC_ELEMENT), is(true));

    parameters = action.getParametersTypes("test", 2);
    assertThat(parameters, hasSize(2));
    assertThat(parameters.get(0).isSameType(BASIC_ELEMENT), is(true));
    assertThat(parameters.get(1).isSameType(PrimitiveType.NUMBER), is(true));

    RuntimeException e =
        expectThrows(RuntimeException.class, () -> action.getParametersTypes("test", 0));
    assertThat(
        e.getMessage(),
        containsString(
            "error 108: test action \"dragAndDrop\" arguments: "
                + "expected number of arguments is (check documentation), found 0"));
  }

  @Test
  public void testDragAndDropOffset() {
    DraggableActionType action = DraggableActionType.dragAndDropByOffset;
    assertThat(action.getReturnType().isSameType(VOID), is(true));
    assertThat(action.getApplyString(), is(equalTo(action.name())));

    List<TypeProvider> parameters = action.getParametersTypes("test", 2);
    assertThat(parameters, hasSize(2));
    assertThat(parameters.get(0).isSameType(PrimitiveType.NUMBER), is(true));
    assertThat(parameters.get(1).isSameType(PrimitiveType.NUMBER), is(true));

    parameters = action.getParametersTypes("test", 3);
    assertThat(parameters, hasSize(3));
    assertThat(parameters.get(0).isSameType(PrimitiveType.NUMBER), is(true));
    assertThat(parameters.get(1).isSameType(PrimitiveType.NUMBER), is(true));
    assertThat(parameters.get(2).isSameType(PrimitiveType.NUMBER), is(true));
  }
}
