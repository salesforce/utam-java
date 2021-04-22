/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.PrimitiveType;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.compiler.helpers.TypeUtilities;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.mockito.Mockito.*;
import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.translator.TranslationUtilities.EMPTY_COMMENTS;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

/**
 * Provides tests for the MethodDeclaration class
 *
 * @author james.evans
 */
public class MethodDeclarationImplTests {

  /** A MethodDeclaration object should be able to be constructed without parameters */
  @Test
  public void testMethodDeclarationCreation() {
    TypeProvider returnType = new TypeUtilities.FromString("ReturnType", "test.ReturnType");

    MethodDeclarationImpl declaration =
        new MethodDeclarationImpl(
            "fakeMethod",
            EMPTY_PARAMETERS,
            returnType,
            Stream.of(returnType).collect(Collectors.toList()),
            EMPTY_COMMENTS, false);
    assertThat(declaration.getName(), is(equalTo("fakeMethod")));
    assertThat(declaration.getReturnType(), is(equalTo(returnType)));
    assertThat(declaration.getParameters(), hasSize(0));
    assertThat(declaration.getImports(), is(equalTo(Collections.singletonList(returnType))));
  }

  /** A MethodDeclaration object should be able to be constructed with parameters */
  @Test
  public void testMethodDeclarationCreationWithParameters() {
    MethodParameter param1 = mock(MethodParameter.class);
    when(param1.getDeclaration()).thenReturn("String param1");

    MethodParameter param2 = mock(MethodParameter.class);
    when(param2.getDeclaration()).thenReturn("");

    TypeProvider returnType = new TypeUtilities.FromString("ReturnType", "test.ReturnType");
    List<MethodParameter> parameters = Arrays.asList(param1, param2);

    MethodDeclarationImpl declaration =
        new MethodDeclarationImpl(
            "fakeMethod",
            parameters,
            returnType,
            Stream.of(returnType).collect(Collectors.toList()),
            EMPTY_COMMENTS, false);
    assertThat(declaration.getName(), is(equalTo("fakeMethod")));
    assertThat(declaration.getReturnType(), is(equalTo(returnType)));
    assertThat(declaration.getParameters(), is(equalTo(parameters)));
    assertThat(declaration.getImports(), is(equalTo(Collections.singletonList(returnType))));
    assertThat(declaration.getCodeLine(), is(equalTo("ReturnType fakeMethod(String param1)")));
  }

  /** A MethodDeclaration object should be able to be constructed with parameters and imports */
  @Test
  public void testMethodDeclarationCreationWithParametersAndImports() {
    MethodParameter param1 = mock(MethodParameter.class);
    when(param1.getDeclaration()).thenReturn("String param1");

    MethodParameter param2 = mock(MethodParameter.class);
    when(param2.getDeclaration()).thenReturn("String param2");

    TypeProvider returnType = new TypeUtilities.FromString("ReturnType", "test.ReturnType");
    List<MethodParameter> parameters = Arrays.asList(param1, param2);

    TypeProvider importType = new TypeUtilities.FromString("Import1Type", "test.Import1Type");

    MethodDeclarationImpl declaration =
        new MethodDeclarationImpl(
            "fakeMethod",
            parameters,
            returnType,
            Stream.of(returnType, importType).collect(Collectors.toList()),
            EMPTY_COMMENTS, false);
    assertThat(declaration.getName(), is(equalTo("fakeMethod")));
    assertThat(declaration.getReturnType(), is(equalTo(returnType)));
    assertThat(declaration.getParameters(), is(equalTo(parameters)));
    assertThat(declaration.getImports(), is(equalTo(Arrays.asList(returnType, importType))));
    assertThat(
        declaration.getCodeLine(),
        is(equalTo("ReturnType fakeMethod(String param1, String param2)")));
  }

  @Test
  public void testMethodDeclarationDuplicatedParamsRemovedIfRootLevelArgsTrue() {
    TypeProvider returnType = new TypeUtilities.FromString("void", "");
    List<MethodParameter> parameters = new ArrayList<>(Arrays.asList(getStringParameter(), getStringParameter()));
    List<MethodParameter> expectedParameters = Collections.singletonList(getStringParameter());

    MethodDeclarationImpl declaration =
            new MethodDeclarationImpl(
                    "fakeMethod",
                    parameters,
                    returnType,
                    Stream.of(returnType).collect(Collectors.toList()),
                    EMPTY_COMMENTS, true);
    assertThat(declaration.getName(), is(equalTo("fakeMethod")));
    assertThat(declaration.getReturnType(), is(equalTo(returnType)));
    assertThat(declaration.getParameters(), is(equalTo(expectedParameters)));
    assertThat(declaration.getImports(), is(equalTo(Collections.singletonList(returnType))));
    assertThat(declaration.getCodeLine(), is(equalTo("void fakeMethod(String text)")));
  }

  /** this class is used from translator utilities tests */
  public static class TestHelper extends MethodDeclarationImpl {

    public TestHelper(String methodName, List<MethodParameter> parameters, TypeProvider returns) {
      super(methodName, parameters, returns, new ArrayList<>(), "", false);
    }

    public TestHelper(String methodName) {
      super(methodName, EMPTY_PARAMETERS, VOID, new ArrayList<>(), "", false);
    }

    public TestHelper(String methodName, String comments) {
      super(methodName, EMPTY_PARAMETERS, VOID, new ArrayList<>(), comments, false);
    }

    public TestHelper(String methodName, boolean hasMethodLevelArgs) {
      super(methodName, EMPTY_PARAMETERS, VOID, new ArrayList<>(), "", hasMethodLevelArgs);
    }
  }
  private static MethodParameter getStringParameter() {
    return new ParameterUtils.Regular("text", PrimitiveType.STRING);
  }
}
