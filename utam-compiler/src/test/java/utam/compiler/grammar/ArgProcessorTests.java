/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.grammar.UtamArgument.ArgsProcessorBasicAction.ERR_MATCHING_TYPES_NOT_FOUND;

import org.testng.annotations.Test;
import utam.compiler.grammar.UtamArgument.ArgsProcessor;
import utam.compiler.grammar.UtamArgument.ArgsProcessorBasicAction;
import utam.compiler.helpers.DraggableActionType;
import utam.core.framework.consumer.UtamError;

/**
 * @author elizaveta.ivanova
 * @since 236
 */
public class ArgProcessorTests {

  @Test
  public void testArgsNotMatchingThrows() {
    ArgsProcessor processor = new ArgsProcessorBasicAction(getTestTranslationContext(), "test",
        DraggableActionType.dragAndDrop);
    UtamArgument[] args = new UtamArgument[0];
    UtamError e = expectThrows(UtamError.class, () -> processor.getParameters(args));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_MATCHING_TYPES_NOT_FOUND, "test"))));
  }
}
