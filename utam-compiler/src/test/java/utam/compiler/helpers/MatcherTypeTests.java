package utam.compiler.helpers;

import com.fasterxml.jackson.databind.deser.impl.CreatorCandidate;
import org.testng.annotations.Test;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

import java.util.Arrays;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

public class MatcherTypeTests {

  @Test
  public void testGetCode() {
    List<MethodParameter> paramTypes = Arrays.asList(
        new ParameterUtils.Regular("text", PrimitiveType.STRING));
    assertThat(
        MatcherType.stringContains.getCode(paramTypes, "test"),
        is(equalTo("test.contains(text)")));

    assertThat(
        MatcherType.stringEquals.getCode(paramTypes, "test"),
        is(equalTo("test.equals(text)")));

    assertThat(
        MatcherType.isTrue.getCode(ParameterUtils.EMPTY_PARAMETERS, "test"),
        is(equalTo("test")));

    assertThat(
        MatcherType.isFalse.getCode(ParameterUtils.EMPTY_PARAMETERS, "test"),
        is(equalTo("Boolean.FALSE.equals(test)")));

    UtamError e = expectThrows(
        UtamError.class,
        () -> MatcherType.stringContains.getCode(ParameterUtils.EMPTY_PARAMETERS, "test"));
    assertThat(e.getMessage(), containsString("expected 1 parameters with type {String}, provided were {}"));
  }

  @Test
  public void testGetExpectedParametersTypes() {
    List<TypeProvider> parameterTypes = MatcherType.stringContains.getExpectedParametersTypes();
    assertThat(parameterTypes, hasSize(1));
    assertThat(parameterTypes, contains(PrimitiveType.STRING));
    parameterTypes = MatcherType.stringEquals.getExpectedParametersTypes();
    assertThat(parameterTypes, hasSize(1));
    assertThat(parameterTypes, contains(PrimitiveType.STRING));
    assertThat(MatcherType.isTrue.getExpectedParametersTypes(), hasSize(0));
    assertThat(MatcherType.isFalse.getExpectedParametersTypes(), hasSize(0));
  }

  @Test
  public void testGetOperandType() {
    assertThat(MatcherType.stringContains.getOperandType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(MatcherType.stringEquals.getOperandType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(MatcherType.isTrue.getOperandType(), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(MatcherType.isFalse.getOperandType(), is(equalTo(PrimitiveType.BOOLEAN)));
  }
}
