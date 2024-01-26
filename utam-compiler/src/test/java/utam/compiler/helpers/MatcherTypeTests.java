/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static utam.compiler.helpers.TypeUtilities.JAVA_OBJECT_TYPE;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

public class MatcherTypeTests {

  private static final String ACTUAL_VALUE = "test";
  private static final List<MethodParameter> EMPTY_PARAMETERS = new ArrayList<>();

  @Test
  public void isTrueTest() {
    MatcherType matcherType = MatcherType.isTrue;
    assertThat(
        matcherType.getCode(ACTUAL_VALUE, EMPTY_PARAMETERS),
        is(equalTo("Boolean.TRUE.equals(test)")));
    assertThat(matcherType.getOperandType().isSameType(PrimitiveType.BOOLEAN), is(true));
    assertThat(matcherType.getExpectedParametersTypes(), hasSize(0));
  }

  @Test
  public void isFalseTest() {
    MatcherType matcherType = MatcherType.isFalse;
    assertThat(
        matcherType.getCode(ACTUAL_VALUE, EMPTY_PARAMETERS),
        is(equalTo("Boolean.FALSE.equals(test)")));
    assertThat(matcherType.getOperandType().isSameType(PrimitiveType.BOOLEAN), is(true));
    assertThat(matcherType.getExpectedParametersTypes(), hasSize(0));
  }

  @Test
  public void stringEqualsTest() {
    MatcherType matcherType = MatcherType.stringEquals;
    List<MethodParameter> paramTypes =
        Collections.singletonList(new Regular("text", PrimitiveType.STRING));
    assertThat(matcherType.getCode(ACTUAL_VALUE, paramTypes), is(equalTo("text.equals(test)")));
    assertThat(matcherType.getOperandType().isSameType(PrimitiveType.STRING), is(true));
    List<TypeProvider> parameterTypes = matcherType.getExpectedParametersTypes();
    assertThat(parameterTypes, hasSize(1));
    assertThat(parameterTypes, contains(PrimitiveType.STRING));
  }

  @Test
  public void stringContainsTest() {
    MatcherType matcherType = MatcherType.stringContains;
    List<MethodParameter> paramTypes =
        Collections.singletonList(new Regular("text", PrimitiveType.STRING));
    assertThat(
        matcherType.getCode(ACTUAL_VALUE, paramTypes),
        is(equalTo("(test!= null && test.contains(text))")));
    assertThat(matcherType.getOperandType().isSameType(PrimitiveType.STRING), is(true));
    List<TypeProvider> parameterTypes = matcherType.getExpectedParametersTypes();
    assertThat(parameterTypes, hasSize(1));
    assertThat(parameterTypes, contains(PrimitiveType.STRING));
  }

  @Test
  public void notNullTest() {
    MatcherType matcherType = MatcherType.notNull;
    assertThat(matcherType.getCode(ACTUAL_VALUE, EMPTY_PARAMETERS), is(equalTo("test != null")));
    assertThat(matcherType.getOperandType().isSameType(JAVA_OBJECT_TYPE), is(true));
    assertThat(matcherType.getExpectedParametersTypes(), hasSize(0));
  }
}
