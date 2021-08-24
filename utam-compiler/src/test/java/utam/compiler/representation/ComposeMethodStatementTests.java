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
import static org.mockito.Mockito.mock;
import static org.testng.Assert.expectThrows;
import static utam.compiler.helpers.ActionableActionType.focus;
import static utam.compiler.helpers.PrimitiveType.BOOLEAN;
import static utam.compiler.helpers.PrimitiveType.NUMBER;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.representation.ComposeMethodStatement.Utility.ERR_NULLABLE_NOT_SUPPORTED;

import java.util.ArrayList;
import java.util.Collections;
import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.representation.ComposeMethodStatement.BasicElementOperation;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.compiler.representation.ComposeMethodStatement.Operation;
import utam.compiler.representation.ComposeMethodStatement.OperationWithPredicate;
import utam.compiler.representation.ComposeMethodStatement.SelfOperand;
import utam.compiler.representation.ComposeMethodStatement.Utility;
import utam.compiler.representation.ComposeMethodStatement.UtilityOperation;
import utam.compiler.representation.ComposeMethodStatement.VoidList;
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
  public void testUtilityOperationNoAddedImports() {
    UtilityOperation utilityOperation = new UtilityOperation(focus, BOOLEAN, new ArrayList<>());
    assertThat(utilityOperation.getAddedImports().isEmpty(), is(true));
    assertThat(utilityOperation.getAddedClassImports().isEmpty(), is(true));
  }

  @Test
  public void testUtilityOperationWithAddedImports() {
    UtilityOperation utilityOperation = new UtilityOperation(focus, SELECTOR, new ArrayList<>());
    assertThat(utilityOperation.getAddedImports().size(), is(1));
    TypeProvider importType = utilityOperation.getAddedImports().get(0);
    assertThat(importType.isSameType(SELECTOR), is(true));
    assertThat(utilityOperation.getAddedClassImports().size(), is(1));
    importType = utilityOperation.getAddedClassImports().get(0);
    assertThat(importType.isSameType(SELECTOR), is(true));
  }

  @Test
  public void testBasicOperationAddedImports() {
    Operation operation = new BasicElementOperation(focus, new ArrayList<>());
    assertThat(operation.getAddedImports().isEmpty(), is(true));
    assertThat(operation.getAddedClassImports().isEmpty(), is(true));
  }

  @Test
  public void testPredicateOperationAddedImports() {
    Operation operation = new BasicElementOperation(focus,
        Collections.singletonList(new Regular("x",
            PrimitiveType.NUMBER)));
    Operand operand = new SelfOperand();
    ComposeMethodStatement predicate = new VoidList(operand, operation, true);
    Operation test = new OperationWithPredicate(focus, SELECTOR,
        Collections.singletonList(predicate));
    assertThat(test.getAddedImports().get(0).isSameType(NUMBER), is(true));
    assertThat(test.getAddedClassImports().size(), is(1));
    assertThat(test.getAddedClassImports().get(0).isSameType(NUMBER), is(true));
  }
}
