/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import static utam.compiler.grammar.TestUtilities.getDeserializedObject;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.grammar.UtamMethod.ERR_METHOD_EMPTY_STATEMENTS;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.*;

/**
 * Provides deserialization tests for the UtamMethod class with abstract methods
 *
 * @author elizaveta.ivanova
 */
public class UtamMethod_DeserializeTests {

  @Test
  public void testAbstractMethod() {
    TranslationContext context = new DeserializerUtilities().getContext("abstractMethod");
    // return type that is not primitive nor a base element type
    MethodInfo methodInfo1 = new MethodInfo("returnsCustomType", "FakeType");
    PageObjectValidationTestHelper.validateMethod(
        context.getMethod("returnsCustomType"), methodInfo1);
    // return actionable
    MethodInfo methodInfo2 = new MethodInfo("returnsActionable", "Actionable");
    PageObjectValidationTestHelper.validateMethod(
        context.getMethod("returnsActionable"), methodInfo2);
    // return list of strings
    MethodInfo methodInfo3 = new MethodInfo("returnsListString", "List<String>");
    PageObjectValidationTestHelper.validateMethod(
        context.getMethod("returnsListString"), methodInfo3);
    // return void
    MethodInfo methodInfo4 = new MethodInfo("returnsVoid", "void");
    PageObjectValidationTestHelper.validateMethod(context.getMethod("returnsVoid"), methodInfo4);
  }

  @Test
  public void testChainMethod() {
    MethodInfo methodInfo1 = new MethodInfo("chainTest", "Type3");
    methodInfo1.addCodeLine("this.getEl1().getEl3()");
    TranslationContext context = new DeserializerUtilities().getContext("chainMethod");
    PageObjectValidationTestHelper.validateMethod(context.getMethod("chainTest"), methodInfo1);

    // test list
    MethodInfo methodInfo2 = new MethodInfo("chainTestList", "List<Type3>");
    methodInfo2.addCodeLine(
        "this.getEl2Element().stream().flatMap(element -> element.getEl3().stream()).collect(Collectors.toList())");
    PageObjectValidationTestHelper.validateMethod(context.getMethod("chainTestList"), methodInfo2);
  }

  /** A UtamMethodChain object should be able to be created through deserialization */
  @Test
  public void testDeserializationDefaultValues() {
    String json = "{" + "  \"element\": \"element\"," + "  \"type\": \"clickable\"" + "}";
    UtamMethodChainLink method = getDeserializedObject(json, UtamMethodChainLink.class);
    assertThat(method, is(not(nullValue())));
    assertThat(method.elementName, is(equalTo("element")));
    assertThat(method.isReturnList, is(equalTo(false)));
    assertThat(method.type, is(equalTo("clickable")));
  }

  @Test
  public void testChainRequiredFields() {
    String nameMissing = "{\"type\": \"clickable\"}";
    assertThrows(
        UtamError.class, () -> getDeserializedObject(nameMissing, UtamMethodChainLink.class));
    String typeMissing = "{\"element\": \"element\"}";
    assertThrows(
        UtamError.class, () -> getDeserializedObject(typeMissing, UtamMethodChainLink.class));
  }

  /** Tests that an empty statement array throws the appropriate exception */
  @Test
  public void testChainEmptyStatementsArrayThrows() {
    String json = "{\"name\": \"chainMethod\",\"chain\": []}";
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                getDeserializedObject(json, UtamMethod.class)
                    .getChainMethod(getTestTranslationContext()));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_METHOD_EMPTY_STATEMENTS, "chainMethod")));
  }
}
