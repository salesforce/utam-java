/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.not;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.expectThrows;
import static utam.compiler.helpers.PrimitiveType.BOOLEAN;
import static utam.compiler.helpers.TypeUtilities.FUNCTION;
import static utam.compiler.representation.ComposeMethodStatement.Utility.ERR_NULLABLE_NOT_SUPPORTED;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.ActionableActionType;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.compiler.representation.ComposeMethodStatement.Operation;
import utam.compiler.representation.ComposeMethodStatement.Utility;
import utam.compiler.representation.ComposeMethodStatement.UtilityOperation;
import utam.core.declarative.representation.TypeProvider;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class ComposeMethodStatementTests {

  @Test
  public void testUtilityOperand() {
    Utility utilityOperand = new Utility(mock(Operand.class), mock(Operation.class), false);
    UtamCompilationError e = expectThrows(UtamCompilationError.class,
        () -> utilityOperand.getNullConditionCode(null, null, true));
    assertThat(e.getMessage(), is(equalTo(ERR_NULLABLE_NOT_SUPPORTED)));
  }

  @Test
  public void testUtilityOperation() {
    List<TypeProvider> imports = new ArrayList<>();
    UtilityOperation utilityOperation = new UtilityOperation(ActionableActionType.focus, BOOLEAN, new ArrayList<>());
    utilityOperation.setReturnTypeImport(imports);
    assertThat(imports, is(Collections.emptyList()));
    utilityOperation = new UtilityOperation(ActionableActionType.focus, FUNCTION, new ArrayList<>());
    utilityOperation.setReturnTypeImport(imports);
    assertThat(imports, is(not(Collections.emptyList())));
  }
}
