/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.instanceOf;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.compiler.helpers.MethodContext.ERR_ARG_DUPLICATE_NAME;
import static utam.compiler.helpers.PrimitiveType.BOOLEAN;
import static utam.compiler.helpers.PrimitiveType.STRING;
import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.Collections;
import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.helpers.ParameterUtils.Primitive;
import utam.compiler.helpers.TypeUtilities.ListOf;
import utam.compiler.representation.ComposeMethodStatement;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

/**
 * @author elizaveta.ivanova
 * @since 232
 */
public class MethodContextTests {

  @Test
  public void testIncorrectReturnTypeThrows() {
    MethodContext methodContext = new MethodContext("name", VOID, false);
    ComposeMethodStatement statement = mock(ComposeMethodStatement.class);
    when(statement.getReturnType()).thenReturn(BOOLEAN);
    assertThrows(() -> methodContext.getReturnType(Collections.singletonList(statement), null));
  }

  @Test
  public void testListOfVoidThrows() {
    assertThrows(() -> new MethodContext("name", VOID, true));
  }

  @Test
  public void testReturnList() {
    MethodContext methodContext = new MethodContext("name", STRING, true);
    assertThat(methodContext.getReturnType(null), is(instanceOf(ListOf.class)));
    List<TypeProvider> imports = methodContext.getReturnTypeImports(Collections.emptyList());
    assertThat(imports.size(), is(2));
    assertThat(imports.get(0).isSameType(LIST_IMPORT), is(true));
    assertThat(imports.get(1).isSameType(STRING), is(true));
  }

  @Test
  public void testDuplicateArgNameInMethod() {
    MethodContext methodContext = new MethodContext();
    methodContext.setMethodArg(new Primitive("name", STRING));
    UtamError e = expectThrows(UtamError.class, () -> methodContext.setMethodArg(new Primitive("name", STRING)));
    assertThat(e.getMessage(), containsString(String.format(ERR_ARG_DUPLICATE_NAME, "method 'test'", "name")));
  }

  @Test
  public void testDuplicateArgNameInStmnt() {
    MethodContext methodContext = new MethodContext();
    methodContext.setStatementArg(new Primitive("name", STRING));
    UtamError e = expectThrows(UtamError.class, () -> methodContext.setStatementArg(new Primitive("name", STRING)));
    assertThat(e.getMessage(), containsString(String.format(ERR_ARG_DUPLICATE_NAME, "method 'test'", "name")));
  }
}
