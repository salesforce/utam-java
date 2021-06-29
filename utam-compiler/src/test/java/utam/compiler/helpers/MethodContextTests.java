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
import static utam.compiler.helpers.MethodContext.ERR_ARG_TYPE_MISMATCH;
import static utam.compiler.helpers.MethodContext.ERR_LIST_OF_VOID_NOT_ALLOWED;
import static utam.compiler.helpers.MethodContext.ERR_LITERAL_PARAMETER_NOT_ALLOWED;
import static utam.compiler.helpers.MethodContext.ERR_METHOD_REFERENCE_ARGS;
import static utam.compiler.helpers.MethodContext.ERR_REFERENCE_MISSING;
import static utam.compiler.helpers.PrimitiveType.BOOLEAN;
import static utam.compiler.helpers.PrimitiveType.STRING;
import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;
import static utam.compiler.helpers.TypeUtilities.REFERENCE;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.Collections;
import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.helpers.ParameterUtils.Literal;
import utam.compiler.helpers.ParameterUtils.Primitive;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.compiler.helpers.TypeUtilities.ListOf;
import utam.compiler.representation.ComposeMethodStatement;
import utam.core.declarative.representation.MethodParameter;
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
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setMethodArg(new Primitive("name", STRING)));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_ARG_DUPLICATE_NAME, "method 'test'", "name")));
  }

  @Test
  public void testDuplicateArgNameInStmnt() {
    MethodContext methodContext = new MethodContext();
    methodContext.setStatementArg(new Primitive("name", STRING));
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setStatementArg(new Primitive("name", STRING)));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_ARG_DUPLICATE_NAME, "method 'test'", "name")));
  }

  @Test
  public void testLiteralMethodParameter() {
    MethodContext methodContext = new MethodContext();
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setMethodArg(new Literal(true, BOOLEAN)));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_LITERAL_PARAMETER_NOT_ALLOWED, "method 'test'", "true")));
  }

  @Test
  public void testArgNotReferenced() {
    MethodContext methodContext = new MethodContext();
    methodContext.setMethodArg(new Primitive("arg", STRING));
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setStatementArg(new Primitive("arg1", STRING)));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_REFERENCE_MISSING, "method 'test'", "arg1")));
  }

  @Test
  public void testArgWrongType() {
    MethodContext methodContext = new MethodContext();
    methodContext.setMethodArg(new Primitive("arg", STRING));
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setStatementArg(new Primitive("arg", BOOLEAN)));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_ARG_TYPE_MISMATCH, "method 'test'", "arg", "Boolean")));
  }

  @Test
  public void testArgRightType() {
    MethodContext methodContext = new MethodContext();
    MethodParameter parameter = new Primitive("arg", STRING);
    methodContext.setMethodArg(parameter);
    methodContext.setStatementArg(parameter);
  }

  @Test
  public void testComposeWithInvalidArgReference() {
    MethodContext methodContext = new MethodContext();
    UtamError e = expectThrows(UtamError.class,
        () -> methodContext.setStatementArg(new Regular("arg", REFERENCE)));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_REFERENCE_MISSING, "method 'test'", "arg")));
  }

  @Test
  public void testComposeWithListThrows() {
    UtamError e = expectThrows(UtamError.class, () -> new MethodContext("test", null, true));
    assertThat(e.getMessage(), containsString(String.format(ERR_LIST_OF_VOID_NOT_ALLOWED, "method 'test'")));
  }

  @Test
  public void testReferenceTypeNotAllowed() {
    MethodContext methodContext = new MethodContext();
    UtamError e = expectThrows(UtamError.class, () -> methodContext.setMethodArg(new Regular("arg", REFERENCE)));
    assertThat(e.getMessage(), containsString(String.format(ERR_METHOD_REFERENCE_ARGS, "method 'test'", "arg")));
  }
}
