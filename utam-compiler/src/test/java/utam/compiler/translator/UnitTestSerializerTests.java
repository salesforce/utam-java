/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.core.declarative.translator.UnitTestRunner;

public class UnitTestSerializerTests {

  private static UnitTestSerializer getUnitTestRunner(String json) {
    DeserializerUtilities.Result res = new DeserializerUtilities().getResultFromString(json);
    return new UnitTestSerializer(res.getPageObject().getImplementation(), res.getContext());
  }

  private static UnitTestSerializer getUnitTestRunner(UnitTestRunner testRunnerType) {
    DeserializerUtilities.Result res = new DeserializerUtilities().getResultFromString("{}");
    return new UnitTestSerializer(
        res.getPageObject().getImplementation(), res.getContext(), testRunnerType);
  }

  @Test
  public void testUnitTestRunnerNone() {
    assertThat(getUnitTestRunner(UnitTestRunner.NONE).toString(), is(emptyString()));
  }

  @Test
  public void testUnitTestRunnerJunit() {
    assertThat(
        getUnitTestRunner(UnitTestRunner.JUNIT).toString(), containsString("org.junit.Test"));
  }

  @Test
  public void testUnitTestRunnerTestNG() {
    assertThat(
        getUnitTestRunner(UnitTestRunner.TESTNG).toString(),
        containsString("org.testng.annotations.Test"));
  }

  @Test
  public void testWithActionableElement() {
    String json =
        "{"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"public\": true,"
            + "      \"selector\": {"
            + "        \"css\": \".fakeSelector\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode,
        containsString("assertThat(testObject.getTestElement().isPresent(), is(equalTo(true)));"));
    assertThat(unitTestCode, containsString(".withChild"));
    assertThat(unitTestCode, containsString("of the root element"));
  }

  @Test
  public void testWithClickableElement() {
    String json =
        "{"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"type\": [\"clickable\"],"
            + "      \"public\": true,"
            + "      \"selector\": {"
            + "        \"css\": \".fakeSelector\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode,
        containsString("assertThat(testObject.getTestElement().isPresent(), is(equalTo(true)));"));
    assertThat(unitTestCode, containsString(".withChild"));
    assertThat(unitTestCode, containsString("of the root element"));
  }

  @Test
  public void testWithEditableElement() {
    String json =
        "{"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"type\": [\"editable\"],"
            + "      \"public\": true,"
            + "      \"selector\": {"
            + "        \"css\": \".fakeSelector\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode,
        containsString("assertThat(testObject.getTestElement().isPresent(), is(equalTo(true)));"));
    assertThat(unitTestCode, containsString(".withChild"));
    assertThat(unitTestCode, containsString("of the root element"));
  }

  @Test
  public void testWithPageObjectElement() {
    String json =
        "{"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"type\": \"utam-test/pageObjects/test/testPageObject\","
            + "      \"public\": true,"
            + "      \"selector\": {"
            + "        \"css\": \".fakeSelector\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode,
        containsString("assertThat(testObject.getTestElement(), is(not(nullValue())));"));
    assertThat(unitTestCode, containsString(".withChild"));
    // todo - fix me
    // assertThat(unitTestCode, containsString("of the root element"));
  }

  @Test
  public void testWithPageObjectElementList() {
    String json =
        "{"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"type\": \"utam-test/pageObjects/test/testObject\","
            + "      \"public\": true,"
            + "      \"selector\": {"
            + "        \"returnAll\": true,"
            + "        \"css\": \".fakeSelector:nth-of-type(%d)\","
            + "        \"args\":["
            + "          {"
            + "            \"name\": \"index\","
            + "            \"type\": \"number\""
            + "          }"
            + "        ]"
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode, containsString("assertThat(testObject.getTestElement(-1), hasSize(-1));"));
    assertThat(unitTestCode, containsString(".withChild"));
    // todo - fix me
    // assertThat(unitTestCode, containsString("of the root element"));
  }

  @Test
  public void testWithListOfElements() {
    String json =
        "{"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"public\": true,"
            + "      \"type\": [\"clickable\"],"
            + "      \"selector\": {"
            + "        \"returnAll\": true,"
            + "        \"css\": \".fakeSelector\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode, containsString("assertThat(testObject.getTestElement(), hasSize(-1));"));
    assertThat(unitTestCode, containsString(".withChild"));
    assertThat(unitTestCode, containsString("of the root element"));
  }

  @Test
  public void testWithShadowElement() {
    String json =
        "{"
            + "  \"shadow\": {"
            + "    \"elements\": ["
            + "      {"
            + "        \"name\": \"testElement\","
            + "        \"public\": true,"
            + "        \"selector\": {"
            + "          \"css\": \".fakeSelector\""
            + "        }"
            + "      }"
            + "    ]"
            + "  }"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode,
        containsString("assertThat(testObject.getTestElement().isPresent(), is(equalTo(true)));"));
    assertThat(unitTestCode, containsString(".withChildInShadowDOM"));
    assertThat(unitTestCode, containsString("of the root element"));
  }

  @Test
  public void testWithNestedElement() {
    String json =
        "{"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testParent\","
            + "      \"selector\": {"
            + "        \"css\": \".fakeParent\""
            + "      },"
            + "      \"elements\": ["
            + "        {"
            + "          \"name\": \"testElement\","
            + "          \"public\": true,"
            + "          \"type\": [\"editable\"],"
            + "          \"selector\": {"
            + "            \"css\": \".fakeSelector\""
            + "          }"
            + "        }"
            + "      ]"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(2));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode,
        containsString("assertThat(testObject.getTestElement().isPresent(), is(equalTo(true)));"));
    assertThat(unitTestCode, containsString(".withChild"));
    assertThat(unitTestCode, containsString("of the element named 'testParent'"));
  }

  @Test
  public void testWithMethodReturningVoid() {
    String json =
        "{"
            + "  \"methods\": ["
            + "    {"
            + "      \"name\": \"clickTestElement\","
            + "      \"compose\": ["
            + "        {"
            + "          \"element\": \"testElement\","
            + "          \"apply\": \"click\""
            + "        }"
            + "      ]"
            + "    }"
            + "  ],"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"type\": [\"clickable\"],"
            + "      \"selector\": {"
            + "        \"css\": \".fakeSelector\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(unitTestCode, containsString("testObject.clickTestElement()"));
    assertThat(
        unitTestCode,
        containsString("// Simply calling the method is enough. We are simply asserting"));
    assertThat(unitTestCode, containsString(".withChild"));
    assertThat(unitTestCode, containsString("of the root element"));
  }

  @Test
  public void testWithMethodReturningString() {
    String json =
        "{"
            + "  \"methods\": ["
            + "    {"
            + "      \"name\": \"getTestElementText\","
            + "      \"compose\": ["
            + "        {"
            + "          \"element\": \"testElement\","
            + "          \"apply\": \"getText\""
            + "        }"
            + "      ]"
            + "    }"
            + "  ],"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"selector\": {"
            + "        \"css\": \".fakeSelector\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode,
        containsString(
            "assertThat(testObject.getTestElementText(),"
                + " is(equalTo(\"replaceWithValidExpectedValue\")));"));
    assertThat(unitTestCode, containsString(".withChild"));
    assertThat(unitTestCode, containsString("of the root element"));
  }

  @Test
  public void testWithMethodReturningNumber() {
    String json =
        "{"
            + "  \"methods\": ["
            + "    {"
            + "      \"name\": \"getTestElementCount\","
            + "      \"compose\": ["
            + "        {"
            + "          \"element\": \"testElement\","
            + "          \"apply\": \"size\""
            + "        }"
            + "      ]"
            + "    }"
            + "  ],"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"selector\": {"
            + "        \"css\": \".fakeSelector\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode,
        containsString("assertThat(testObject.getTestElementCount(), is(equalTo(-1)));"));
    assertThat(unitTestCode, containsString(".withChild"));
    assertThat(unitTestCode, containsString("of the root element"));
  }

  @Test
  public void testWithMethodReturningBoolean() {
    String json =
        "{"
            + "  \"methods\": ["
            + "    {"
            + "      \"name\": \"getTestElementVisible\","
            + "      \"compose\": ["
            + "        {"
            + "          \"element\": \"testElement\","
            + "          \"apply\": \"isVisible\""
            + "        }"
            + "      ]"
            + "    }"
            + "  ],"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"selector\": {"
            + "        \"css\": \".fakeSelector\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode,
        containsString("assertThat(testObject.getTestElementVisible(), is(equalTo(false)));"));
    assertThat(unitTestCode, containsString(".withChild"));
    assertThat(unitTestCode, containsString("of the root element"));
  }

  @Test
  public void testWithMethodReturningStringList() {
    String json =
        "{"
            + "  \"methods\": ["
            + "    {"
            + "      \"name\": \"getTestElementText\","
            + "      \"compose\": ["
            + "        {"
            + "          \"element\": \"testElement\","
            + "          \"apply\": \"getText\""
            + "        }"
            + "      ]"
            + "    }"
            + "  ],"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"selector\": {"
            + "        \"returnAll\": true,"
            + "        \"css\": \".fakeSelector\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode, containsString("assertThat(testObject.getTestElementText(), hasSize(-1));"));
    assertThat(unitTestCode, containsString("assertThat(testObject.getTestElementText(),"));
    assertThat(unitTestCode, containsString("\"replaceWithValidExpectedValueList\"));"));
    assertThat(unitTestCode, containsString(".withChild"));
    assertThat(unitTestCode, containsString("of the root element"));
  }

  @Test
  public void testWithElementRequiringStringParameter() {
    String json =
        "{"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"public\": true,"
            + "      \"selector\": {"
            + "        \"css\": \"a[title=*'%s']\","
            + "        \"args\": ["
            + "          {"
            + "            \"name\": \"title\","
            + "            \"type\": \"string\""
            + "          }"
            + "        ]"
            + "      },"
            + "      \"type\": [\"clickable\"]"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode,
        containsString(
            "assertThat(testObject.getTestElement(\"replaceWithValidExpectedValue\").isPresent(),"));
    assertThat(unitTestCode, containsString("is(equalTo(true)));"));
    assertThat(unitTestCode, containsString(".withChild"));
    assertThat(unitTestCode, containsString("of the root element"));
  }

  @Test
  public void testWithElementRequiringNumberParameter() {
    String json =
        "{"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"public\": true,"
            + "      \"selector\": {"
            + "        \"css\": \"a:nth-of-type(%d)\","
            + "        \"args\": ["
            + "          {"
            + "            \"name\": \"index\","
            + "            \"type\": \"number\""
            + "          }"
            + "        ]"
            + "      },"
            + "      \"type\": [\"clickable\"]"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode,
        containsString(
            "assertThat(testObject.getTestElement(-1).isPresent(), is(equalTo(true)));"));
    assertThat(unitTestCode, containsString(".withChild"));
    assertThat(unitTestCode, containsString("of the root element"));
  }

  @Test
  public void testWithTouchableElement() {
    String json =
        "{"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"type\": [\"touchable\"],"
            + "      \"public\": true,"
            + "      \"selector\": {"
            + "        \"uiautomator\": \"className(\\\"android.widget.RelativeLayout\\\")\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    UnitTestSerializer serializer = getUnitTestRunner(json);
    assertThat(serializer.getElementRegistrationStatements(), hasSize(1));
    assertThat(serializer.getTestMethods(), hasSize(1));
    String unitTestCode = serializer.toString();
    assertThat(
        unitTestCode,
        containsString("assertThat(testObject.getTestElement().isPresent(), is(equalTo(true)));"));
    assertThat(unitTestCode, containsString(".withChild"));
    assertThat(unitTestCode, containsString("of the root element"));
  }
}
