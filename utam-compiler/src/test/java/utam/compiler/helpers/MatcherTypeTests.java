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

import java.util.Collections;
import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

public class MatcherTypeTests {

  private static final String ACTUAL_VALUE = "getTestValue";
  private static final List<MethodParameter> EMPTY = ParameterUtils.EMPTY_PARAMETERS;

  @Test
  public void isTrueTest() {
    MatcherType matcherType = MatcherType.isTrue;
    assertThat(
        matcherType.getCode(false, EMPTY, ACTUAL_VALUE),
        is(equalTo(ACTUAL_VALUE)));
    assertThat(
        matcherType.getCode(true, EMPTY, ACTUAL_VALUE),
        is(equalTo("return " + ACTUAL_VALUE + ";")));
    assertThat(matcherType.getOperandType(), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(matcherType.getExpectedParametersTypes(), hasSize(0));
  }

  @Test
  public void isFalseTest() {
    MatcherType matcherType = MatcherType.isFalse;
    assertThat(
        matcherType.getCode(false, EMPTY, ACTUAL_VALUE),
        is(equalTo("Boolean.FALSE.equals(" + ACTUAL_VALUE + ")")));
    assertThat(
        matcherType.getCode(true, EMPTY, ACTUAL_VALUE),
        is(equalTo("return Boolean.FALSE.equals(" + ACTUAL_VALUE + ");")));
    assertThat(matcherType.getOperandType(), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(matcherType.getExpectedParametersTypes(), hasSize(0));
  }

  @Test
  public void stringEqualsTest() {
    MatcherType matcherType = MatcherType.stringEquals;
    List<MethodParameter> paramTypes = Collections.singletonList(
        new Regular("text", PrimitiveType.STRING));
    assertThat(
        matcherType.getCode(false, paramTypes, ACTUAL_VALUE),
        is(equalTo("text.equals(getTestValue)")));
    assertThat(
        matcherType.getCode(true, paramTypes, ACTUAL_VALUE),
        is(equalTo("return text.equals(getTestValue);")));
    assertThat(matcherType.getOperandType(), is(equalTo(PrimitiveType.STRING)));
    List<TypeProvider> parameterTypes = matcherType.getExpectedParametersTypes();
    assertThat(parameterTypes, hasSize(1));
    assertThat(parameterTypes, contains(PrimitiveType.STRING));
    assertThat(matcherType.getOperandType(), is(equalTo(PrimitiveType.STRING)));
  }

  @Test
  public void stringContainsTest() {
    MatcherType matcherType = MatcherType.stringContains;
    List<MethodParameter> paramTypes = Collections.singletonList(
        new Regular("text", PrimitiveType.STRING));
    assertThat(
        matcherType.getCode(false, paramTypes, ACTUAL_VALUE),
        is(equalTo("{ String tmp = getTestValue;\nreturn tmp!= null && tmp.contains(text); }")));
    assertThat(
        matcherType.getCode(true, paramTypes, ACTUAL_VALUE),
        is(equalTo("String tmp = getTestValue;\nreturn tmp!= null && tmp.contains(text);")));
    assertThat(matcherType.getOperandType(), is(equalTo(PrimitiveType.STRING)));
    List<TypeProvider> parameterTypes = matcherType.getExpectedParametersTypes();
    assertThat(parameterTypes, hasSize(1));
    assertThat(parameterTypes, contains(PrimitiveType.STRING));
    assertThat(matcherType.getOperandType(), is(equalTo(PrimitiveType.STRING)));
  }

  @Test
  public void notNullTest() {
    MatcherType matcherType = MatcherType.notNull;
    assertThat(
        matcherType.getCode(false, EMPTY, ACTUAL_VALUE), is(equalTo(ACTUAL_VALUE + " != null")));
    assertThat(
        matcherType.getCode(true, EMPTY, ACTUAL_VALUE),
        is(equalTo("return " + ACTUAL_VALUE + " != null;")));
    assertThat(matcherType.getOperandType(), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(matcherType.getExpectedParametersTypes(), hasSize(0));
  }
}
