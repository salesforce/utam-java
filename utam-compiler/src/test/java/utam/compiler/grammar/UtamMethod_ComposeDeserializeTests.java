/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethod;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.representation.PageObjectMethod;

import java.util.Collection;

import static utam.compiler.grammar.TestUtilities.*;
import static utam.compiler.grammar.UtamMethod.ERR_METHOD_EMPTY_STATEMENTS;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

/**
 * Provides deserialization tests for the UtamMethod class with compose methods
 *
 * @author james.evans
 */
public class UtamMethod_ComposeDeserializeTests {

  private static PageObjectMethod getMethodObject(String json, String rootJson) {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamPageObject pageObject = getDeserializedObject(rootJson, UtamPageObject.class);
    pageObject.compile(context);
    PageObjectMethod method = getDeserializedObject(json, UtamMethod.class).getMethod(context);
    context.setMethod(method);
    return method;
  }

  // TODO add test for deserialization errors as it's now a logic

  @Test
  public void testDeserializationDefaultValues() {
    String json =
        "{"
            + "  \"name\": \"composeMethod\","
            + "  \"compose\": ["
            + "    {"
            + "      \"element\": \"element\","
            + "      \"apply\": \"click\""
            + "    }"
            + "  ]"
            + "}";
    UtamMethod method = getDeserializedObject(json, UtamMethod.class);
    assertThat(method, is(not(nullValue())));
    assertThat(method.args, is(nullValue()));
    assertThat(method.chain, is(nullValue()));
    assertThat(method.compose, is(not(nullValue())));
    assertThat(method.isReturnList, is(nullValue()));
    assertThat(method.name, is(equalTo("composeMethod")));
    assertThat(method.returnStr, is(nullValue()));
  }

  /** Tests that a ComposeMethodNode can be created */
  @Test
  public void testComposeMethodNode() {
    String json =
        "{"
            + "  \"name\": \"composeMethod\","
            + "  \"compose\": ["
            + "    {"
            + "      \"element\": \"element1\","
            + "      \"apply\": \"click\""
            + "    }"
            + "  ]"
            + "}";
    String rootNodeJson =
        "{"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"element1\","
            + "      \"type\": \"clickable\","
            + "      \"selector\": {"
            + "        \"css\": \".element\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    MethodInfo methodInfo = new MethodInfo("composeMethod", "void");
    methodInfo.addCodeLine(getElementPrivateMethodCalled("element1") + "().click()");
    PageObjectMethod method = getMethodObject(json, rootNodeJson);
    assertThat(method, is(instanceOf(ComposeMethod.class)));
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  /** Tests that an empty statement array throws the appropriate exception */
  @Test
  public void testEmptyStatementsArrayThrows() {
    String json = "{\"name\": \"composeMethod\", \"compose\": []}";
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                getDeserializedObject(json, UtamMethod.class)
                    .getMethod(getTestTranslationContext()));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_METHOD_EMPTY_STATEMENTS, "composeMethod")));
  }

  /** Tests that a ComposeMethodNode with a list statement can be created */
  @Test
  public void testListComposeMethodNodeForListElement() {
    String json =
        "{"
            + "  \"name\": \"composeMethod\","
            + "  \"compose\": ["
            + "    {"
            + "      \"element\": \"element1\","
            + "      \"apply\": \"click\""
            + "    }"
            + "  ]"
            + "}";
    String rootNodeJson =
        "{"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"element1\","
            + "      \"type\": \"clickable\","
            + "      \"selector\": {"
            + "        \"css\": \".element\","
            + "        \"returnAll\": true"
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    MethodInfo methodInfo = new MethodInfo("composeMethod", "void");
    methodInfo.addCodeLine(
        getElementPrivateMethodCalled("element1") + "().forEach(element -> element.click())");

    PageObjectMethod method = getMethodObject(json, rootNodeJson);
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  /** Tests that a ComposeMethodNode with a list statement returning a value can be created */
  @Test
  public void testListComposeMethodNodeWithActionReturningValueForListElement() {
    String json =
        "{"
            + "  \"name\": \"composeMethod\","
            + "  \"compose\": ["
            + "    {"
            + "      \"element\": \"element1\","
            + "      \"apply\": \"getValue\""
            + "    }"
            + "  ]"
            + "}";
    String rootNodeJson =
        "{"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"element1\","
            + "      \"selector\": {"
            + "        \"css\": \".element\","
            + "        \"returnAll\": true"
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    MethodInfo methodInfo = new MethodInfo("composeMethod", "List<String>");
    methodInfo.addCodeLine(
        getElementPrivateMethodCalled("element1")
            + "().stream().map(element -> element.getValue()).collect(Collectors.toList())");

    PageObjectMethod method = getMethodObject(json, rootNodeJson);
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testComposeArgs() {
    String json =
        "{\n"
            + "  \"methods\" : [\n"
            + "    {\n"
            + "      \"name\" : \"getClassAttr1\",\n"
            + "      \"compose\" : [\n"
            + "        {\n"
            + "          \"apply\": \"getAttribute\",\n"
            + "          \"element\": \"root\",\n"
            + "          \"args\" : [\n"
            + "            { \"value\" :  \"class\" }\n"
            + "          ]\n"
            + "        }\n"
            + "      ]\n"
            + "    },\n"
            + "    {\n"
            + "      \"name\" : \"getClassAttr2\",\n"
            + "      \"compose\" : [\n"
            + "        {\n"
            + "          \"apply\": \"getAttribute\",\n"
            + "          \"element\": \"root\",\n"
            + "          \"args\" : [\n"
            + "            {\n"
            + "              \"name\" :  \"attrName\",\n"
            + "              \"type\" : \"string\"\n"
            + "            }\n"
            + "          ]\n"
            + "        }\n"
            + "      ]\n"
            + "    }\n"
            + "  ]\n"
            + "}";
    PageObjectDeclaration generated = getJsonStringDeserializer(json).getObject();
    assertThat(generated.getImplementation().getMethods().size(), is(equalTo(2)));
    MethodDeclaration firstMethod =
        generated.getImplementation().getMethods().get(0).getDeclaration();
    assertThat(firstMethod.getCodeLine(), is(equalTo("String getClassAttr1()")));
    MethodDeclaration secondMethod =
        generated.getImplementation().getMethods().get(1).getDeclaration();
    assertThat(secondMethod.getCodeLine(), is(equalTo("String getClassAttr2(String attrName)")));
  }

  /** Tests the isChain method returns the correct value for element containing chain property */
  @Test
  public void testMethodType() {
    final String ELEMENT_NAME = "elementName";
    String json =
        "{\n"
            + "      \"name\": \"click\",\n"
            + "      \"compose\": [\n"
            + "        {\n"
            + "          \"apply\": \"focus\",\n"
            + String.format("          \"element\": \"%s\"\n", ELEMENT_NAME)
            + "        }\n"
            + "      ]\n"
            + "    }";
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamElement utamElement = new UtamElement(ELEMENT_NAME);
    utamElement.selector = new UtamSelector("selector");
    utamElement.getAbstraction().testRootTraverse(context);
    UtamMethod method = getDeserializedObject(json, UtamMethod.class);
    assertThat(method.getMethod(context), instanceOf(ComposeMethod.class));
  }

  @Test
  public void getMethodComments() {
    String json =
        "{"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"element1\","
            + "      \"selector\": {"
            + "        \"css\": \".element\""
            + "      }"
            + "    }"
            + "  ],"
            + " \"methods\": ["
            + "{"
            + "  \"name\": \"composeMethod\","
            + "  \"compose\": ["
            + "    {"
            + "      \"element\": \"element1\","
            + "      \"apply\": \"size\""
            + "    }"
            + "  ]"
            + "}"
            + "]"
            + "}";
    JsonDeserializer deserializer = TestUtilities.getJsonStringDeserializer(json);
    PageObjectDeclaration declaration = deserializer.getObject();
    Collection<MethodDeclaration> methods = declaration.getInterface().getDeclaredApi();
    assertThat(methods.size(), is(equalTo(1)));
    assertThat(methods.iterator().next().getComments(), is(emptyString()));
  }

  @Test
  public void testRootComposeGetClass() {
    MethodInfo methodInfo = new MethodInfo("testCompose", "String");
    methodInfo.addCodeLine("this.getRootElement().getClassAttribute()");
    TranslationContext context = new DeserializerUtilities().getContext("composeMethod");
    PageObjectValidationTestHelper.validateMethod(context.getMethod("testCompose"), methodInfo);
  }

  @Test
  public void testComposeWaitForBasicActionThatReturnsString() {
    MethodInfo methodInfo = new MethodInfo("testCompose", "String");
    methodInfo.addCodeLine("this.getRootElement().waitFor(() -> {\n"
        + "return this.getRootElement().getText();\n"
        + "})");
    TranslationContext context = new DeserializerUtilities().getContext("composeWaitFor1");
    PageObjectValidationTestHelper.validateMethod(context.getMethod("testCompose"), methodInfo);
  }

  @Test
  public void testComposeWithCustomElementReturnsList() {
    MethodInfo methodInfo = new MethodInfo("testCompose", "List<String>");
    methodInfo.addParameter(new MethodParameterInfo("strArg", "String"));
    methodInfo.addCodeLine("this.getCustomElement().someMethod(strArg,true)");
    TranslationContext context = new DeserializerUtilities().getContext("composeCustom");
    PageObjectValidationTestHelper.validateMethod(context.getMethod("testCompose"), methodInfo);
  }

  @Test
  public void testComposeWaitForBasicVoidAction() {
    MethodInfo methodInfo = new MethodInfo("testCompose", "Boolean");
    methodInfo.addCodeLine("this.getRootElement().waitFor(() -> {\n"
        + "this.getRootElement().getText().contains(\"subString\");\n"
        + "this.getRootElement().focus();\n"
        + "return true;\n"
        + "})");
    TranslationContext context = new DeserializerUtilities().getContext("composeWaitVoidAction");
    PageObjectMethod method = context.getMethod("testCompose");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testComposeWaitForContainsElementAction() {
    MethodInfo methodInfo = new MethodInfo("testCompose", "Boolean");
    methodInfo.addCodeLine("this.getRootElement().waitFor(() -> {\n"
        + "this.getRootElement().getText();\n"
        + "return Boolean.FALSE.equals(this.getRootElement().containsElement(LocatorBy.byCss(\".css\")));\n"
        + "})");
    TranslationContext context = new DeserializerUtilities().getContext("composeWaitForSelector");
    PageObjectMethod method = context.getMethod("testCompose");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testComposeWaitForCustomElement() {
    MethodInfo methodInfo = new MethodInfo("testCompose", "Boolean");
    methodInfo.addParameter(new MethodParameterInfo("selectorArg", "LocatorBy"));
    methodInfo.addParameter(new MethodParameterInfo("matcherArg", "String"));
    methodInfo.addCodeLine("this.getCustomElement().waitFor(() -> {\n"
        + "return this.getCustomElement().returnsString(selectorArg).contains(matcherArg);\n"
        + "})");
    TranslationContext context = new DeserializerUtilities().getContext("composeWaitForCustom");
    PageObjectMethod method = context.getMethod("testCompose");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testComposeWithDocumentElement() {
    MethodInfo methodInfo = new MethodInfo("testCompose", "String");
    methodInfo.addCodeLine("this.getDocument().getUrl()");
    TranslationContext context = new DeserializerUtilities().getContext("composeDocument");
    PageObjectValidationTestHelper.validateMethod(context.getMethod("testCompose"), methodInfo);
  }
}
