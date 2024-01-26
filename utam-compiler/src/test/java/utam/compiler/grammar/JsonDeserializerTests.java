/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static utam.compiler.grammar.TestUtilities.getJsonStringDeserializer;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.core.declarative.representation.PageObjectClass;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.representation.PageObjectInterface;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.translator.UnitTestRunner;

public class JsonDeserializerTests {

  private static final String INTERFACE_PACKAGE_NAME = "utam.test.pageobjects.test";
  private static final String INTERFACE_SIMPLE_NAME = "Test";
  private static final String INTERFACE_FULL_NAME = "utam.test.pageobjects.test.Test";
  private static final String IMPL_PACKAGE_NAME = "utam.test.pageobjects.test.impl";
  private static final String IMPL_SIMPLE_NAME = "TestImpl";
  private static final String IMPL_FULL_NAME = "utam.test.pageobjects.test.impl.TestImpl";

  private static PageObjectDeclaration createRootNode(String json) {
    return new DeserializerUtilities().getResultFromString(json).getPageObject();
  }

  /** A valid root node should be able to be created */
  @Test
  public void testRootCreation() {
    String json =
        "{"
            + "  \"selector\": {"
            + "    \"css\": \"rootSelector\""
            + "  },"
            + "  \"root\": true"
            + "}";
    PageObjectDeclaration rootNode = getJsonStringDeserializer(json).getObject();
    assertThat(rootNode.getInterface().getBaseInterfaceType(), is(equalTo(ROOT_PAGE_OBJECT)));
  }

  /** Tests that an empty root node is valid. */
  @Test
  public void testEmptyRootNode() {
    String json = "{" + "}";
    PageObjectClass generatedClass = createRootNode(json).getImplementation();
    assertThat(generatedClass.getFields(), is(empty()));
    assertThat(generatedClass.getMethods(), is(hasSize(0)));
  }

  /** Tests that a root node with a selector property is valid */
  @Test
  public void testValidRootNodeWithSelector() {
    String json =
        "{"
            + "  \"selector\": {"
            + "    \"css\": \"rootSelector\""
            + "  },"
            + "  \"root\": true"
            + "}";

    PageObjectClass pageObjectClass = createRootNode(json).getImplementation();
    // root method is declared
    assertThat(pageObjectClass.getMethods(), is(hasSize(0)));
    assertThat(pageObjectClass.getFields().isEmpty(), is(true));
  }

  /**
   * Tests that a root node with a compose method is valid (generated method content validated in
   * other tests)
   */
  @Test
  public void testValidRootNodeWithComposeMethod() {
    String json =
        "{"
            + "  \"methods\": ["
            + "    {"
            + "      \"name\": \"composeMethod\","
            + "      \"compose\": ["
            + "        {"
            + "          \"element\": \"childElement\","
            + "          \"apply\": \"click\""
            + "        }"
            + "      ]"
            + "    }"
            + "  ],"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"childElement\","
            + "      \"selector\": {"
            + "        \"css\": \"childSelector\""
            + "      },"
            + "      \"type\": [\"clickable\"]"
            + "    }"
            + "  ]"
            + "}";

    PageObjectDeclaration node = createRootNode(json);
    assertThat(node.getImplementation().getMethods(), hasSize(2));
    assertThat(node.getInterface().getDeclaredApi(), hasSize(1));
  }

  /** Tests that the getImplementedTypes method returns a valid PageObjectInterface object */
  @Test
  public void testGetImplementedType() {
    String json =
        "{"
            + "  \"type\":[\"clickable\"],"
            + "  \"exposeRootElement\" : true,"
            + "  \"selector\": {"
            + "    \"css\": \"rootSelector\""
            + "  },"
            + "  \"root\": true,"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"childElement\","
            + "      \"type\": [\"clickable\"],"
            + "      \"selector\": {"
            + "        \"css\": \".fakeSelector\""
            + "      }"
            + "    }"
            + "  ],"
            + "  \"methods\": ["
            + "    {"
            + "      \"name\": \"clickElement\","
            + "      \"compose\": ["
            + "        {"
            + "          \"element\": \"childElement\","
            + "          \"apply\": \"click\""
            + "        }"
            + "      ]"
            + "    }"
            + "  ]"
            + "}";

    PageObjectValidationTestHelper.EntityNameInfo nameInfo =
        new PageObjectValidationTestHelper.EntityNameInfo(
            INTERFACE_PACKAGE_NAME, INTERFACE_SIMPLE_NAME, INTERFACE_FULL_NAME);
    PageObjectValidationTestHelper.EntityNameInfo nameImplInfo =
        new PageObjectValidationTestHelper.EntityNameInfo(
            IMPL_PACKAGE_NAME, IMPL_SIMPLE_NAME, IMPL_FULL_NAME);
    PageObjectValidationTestHelper.FieldInfo fieldInfo =
        new PageObjectValidationTestHelper.FieldInfo("childElement");
    fieldInfo.addAnnotations("@ElementMarker.Find(css = \".fakeSelector\")");

    PageObjectValidationTestHelper.MethodInfo rootElementMethod =
        new PageObjectValidationTestHelper.MethodInfo("getRoot", "RootElement");
    rootElementMethod.addCodeLine("return getProxy(this.getRootElement(), RootElement.class)");

    PageObjectValidationTestHelper.MethodInfo childElementGetter =
        new PageObjectValidationTestHelper.MethodInfo(
            getElementGetterMethodName("childElement", false), "ChildElementElement");
    childElementGetter.addCodeLine("BasicElement root = this.getRoot()");
    childElementGetter.addCodeLine(
        "return basic(root, this.childElement).build(ChildElementElement.class,"
            + " ChildElementElementImpl.class)");
    childElementGetter.setNotPublic();

    PageObjectValidationTestHelper.MethodInfo composeMethod =
        new PageObjectValidationTestHelper.MethodInfo("clickElement", "void");
    composeMethod.addCodeLine("ChildElementElement childElement0 = this.getChildElementElement()");
    composeMethod.addCodeLine("childElement0.click()");

    PageObjectClass classObject = createRootNode(json).getImplementation();
    PageObjectInterface interfaceObject = classObject.getImplementedType();
    PageObjectValidationTestHelper.validateInterface(
        interfaceObject,
        nameInfo,
        Stream.of(rootElementMethod, composeMethod).collect(Collectors.toList()));
    PageObjectValidationTestHelper.validateImplementation(
        classObject,
        nameImplInfo,
        Stream.of("@PageMarker.Find(css = \"rootSelector\")").collect(Collectors.toList()),
        Stream.of(rootElementMethod, childElementGetter, composeMethod)
            .collect(Collectors.toList()),
        Stream.of(fieldInfo).collect(Collectors.toList()));
    assertThat(interfaceObject, is(not(nullValue())));
  }

  /**
   * Tests that the isClassWithInterface method returns true when the root node describe a basic
   * page object
   */
  @Test
  public void testIsClassWithInterfaceReturnsTrueForBasicPageObjects() {
    String json =
        "{"
            + "  \"selector\": {"
            + "    \"css\": \"rootSelector\""
            + "  },"
            + "  \"root\": true"
            + "}";

    PageObjectDeclaration node = createRootNode(json);
    assertThat(node.isClassWithInterface(), is(equalTo(true)));
  }

  /**
   * Tests that the isInterfaceOnly method returns false when the root node describe a basic page
   * object
   */
  @Test
  public void testIsInterfaceOnlyReturnsFalseForBasicPageObject() {
    String json =
        "{"
            + "  \"selector\": {"
            + "    \"css\": \"rootSelector\""
            + "  },"
            + "  \"root\": true"
            + "}";
    PageObjectDeclaration rootNode = getJsonStringDeserializer(json).getObject();
    assertThat(rootNode.isInterfaceOnly(), is(equalTo(false)));
  }

  /**
   * Tests that the isInterfaceOnly method returns true when the root node describe an interface
   * page object
   */
  @Test
  public void testIsInterfaceOnlyReturnsTrueForInterfacePageObject() {
    String json =
        "{"
            + "  \"interface\": true,"
            + "  \"methods\": ["
            + "    {"
            + "      \"name\": \"testMethod\","
            + "      \"returnType\": \"string\""
            + "    }"
            + "  ],"
            + "  \"root\": true"
            + "}";
    PageObjectDeclaration rootNode = getJsonStringDeserializer(json).getObject();
    assertThat(rootNode.isInterfaceOnly(), is(equalTo(true)));
  }

  /** Tests that the getClassType method returns a correct TypeProvider object */
  @Test
  public void testGetClassType() {
    String json =
        "{"
            + "  \"selector\": {"
            + "    \"css\": \"rootSelector\""
            + "  },"
            + "  \"root\": true"
            + "}";

    PageObjectClass node = createRootNode(json).getImplementation();
    TypeProvider provider = node.getClassType();
    assertThat(provider.getPackageName(), is(equalTo(IMPL_PACKAGE_NAME)));
    assertThat(provider.getSimpleName(), is(equalTo(IMPL_SIMPLE_NAME)));
    assertThat(provider.getFullName(), is(equalTo(IMPL_FULL_NAME)));
  }

  /**
   * Tests that the getImplCode method returns the string representation of the Java code for the
   * generated Page Object root node
   */
  @Test
  public void testGetImplCode() {
    String json = "{}";

    PageObjectClass node = createRootNode(json).getImplementation();
    String implCode = node.getGeneratedCode();
    assertThat(implCode, containsString("package " + IMPL_PACKAGE_NAME));
    assertThat(implCode, containsString("import utam.core.framework.base.BasePageObject"));
    assertThat(implCode, containsString("import " + INTERFACE_FULL_NAME));
    assertThat(
        implCode,
        containsString(
            "public final class "
                + IMPL_SIMPLE_NAME
                + " extends BasePageObject implements "
                + INTERFACE_SIMPLE_NAME));
  }

  /**
   * Tests that the getIGeneratedUnitTestCode method returns the string representation of the Java
   * code for the generated Page Object's unit tests with no unit test runner
   */
  @Test
  public void testGetGeneratedUnitTestCode() {
    String json =
        "{"
            + "  \"methods\": ["
            + "    {"
            + "      \"name\": \"composeMethod\","
            + "      \"compose\": ["
            + "        {"
            + "          \"element\": \"childElement\","
            + "          \"apply\": \"click\""
            + "        }"
            + "      ]"
            + "    }"
            + "  ],"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"childElement\","
            + "      \"selector\": {"
            + "        \"css\": \"childSelector\""
            + "      },"
            + "      \"type\": [\"clickable\"]"
            + "    }"
            + "  ]"
            + "}";

    PageObjectClass node = createRootNode(json).getImplementation();
    String unitTestCode = node.getGeneratedUnitTestCode(UnitTestRunner.NONE);
    assertThat(unitTestCode, is(emptyString()));
  }

  /**
   * Tests that the getIGeneratedUnitTestCode method returns the string representation of the Java
   * code for the generated Page Object's unit tests with the TestNG unit test runner
   */
  @Test
  public void testGetGeneratedUnitTestCodeWithTestNG() {
    String json =
        "{"
            + "  \"methods\": ["
            + "    {"
            + "      \"name\": \"composeMethod\","
            + "      \"compose\": ["
            + "        {"
            + "          \"element\": \"childElement\","
            + "          \"apply\": \"click\""
            + "        }"
            + "      ]"
            + "    }"
            + "  ],"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"childElement\","
            + "      \"selector\": {"
            + "        \"css\": \"childSelector\""
            + "      },"
            + "      \"type\": [\"clickable\"]"
            + "    }"
            + "  ]"
            + "}";

    PageObjectClass node = createRootNode(json).getImplementation();
    String unitTestCode = node.getGeneratedUnitTestCode(UnitTestRunner.TESTNG);
    assertThat(unitTestCode, containsString("package " + IMPL_PACKAGE_NAME));
    assertThat(unitTestCode, containsString("import static org.hamcrest.MatcherAssert.assertThat"));
    assertThat(unitTestCode, containsString("import static org.hamcrest.Matchers.*"));
    assertThat(unitTestCode, containsString("import org.testng.annotations.BeforeClass"));
    assertThat(unitTestCode, containsString("import org.testng.annotations.Test"));
    assertThat(unitTestCode, containsString("public class TestImplTests"));
    assertThat(unitTestCode, containsString("public void testComposeMethod()"));
    assertThat(unitTestCode, containsString("public void setupSimulator()"));
    assertThat(
        unitTestCode,
        containsString(
            "// .withChild(simulator.registerElement(\"childElement\", \"childSelector\"))"));
  }

  /**
   * Tests that the getIGeneratedUnitTestCode method returns the string representation of the Java
   * code for the generated Page Object's unit tests with the JUnit unit test runner
   */
  @Test
  public void testGetGeneratedUnitTestCodeWithJUnit() {
    String json =
        "{"
            + "  \"methods\": ["
            + "    {"
            + "      \"name\": \"composeMethod\","
            + "      \"compose\": ["
            + "        {"
            + "          \"element\": \"childElement\","
            + "          \"apply\": \"click\""
            + "        }"
            + "      ]"
            + "    }"
            + "  ],"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"childElement\","
            + "      \"selector\": {"
            + "        \"css\": \"childSelector\""
            + "      },"
            + "      \"type\": [\"clickable\"]"
            + "    }"
            + "  ]"
            + "}";

    PageObjectClass node = createRootNode(json).getImplementation();
    String unitTestCode = node.getGeneratedUnitTestCode(UnitTestRunner.JUNIT);
    assertThat(unitTestCode, containsString("package " + IMPL_PACKAGE_NAME));
    assertThat(unitTestCode, containsString("import static org.hamcrest.MatcherAssert.assertThat"));
    assertThat(unitTestCode, containsString("import static org.hamcrest.Matchers.*"));
    assertThat(unitTestCode, containsString("import org.junit.BeforeClass"));
    assertThat(unitTestCode, containsString("import org.junit.Test"));
    assertThat(unitTestCode, containsString("public class TestImplTests"));
    assertThat(unitTestCode, containsString("public void testComposeMethod()"));
    assertThat(unitTestCode, containsString("public void setupSimulator()"));
    assertThat(
        unitTestCode,
        containsString(
            "// .withChild(simulator.registerElement(\"childElement\", \"childSelector\"))"));
  }

  /**
   * Tests that the getApiCode method returns the string representation of the Java code for the
   * generated Page Object root node
   */
  @Test
  public void testGetGeneratedCode() {
    String json = "{}";
    PageObjectInterface node = createRootNode(json).getInterface();
    String apiCode = node.getGeneratedCode();
    assertThat(apiCode, containsString("package utam.test.pageobjects.test"));
    assertThat(apiCode, containsString("import utam.core.framework.base.PageObject"));
    assertThat(apiCode, containsString("public interface Test extends PageObject {}"));
  }

  /**
   * Tests that the getClassAnnotations method returns an empty annotation list for the generated
   * class with no root selector and no shadow element
   */
  @Test
  public void testGetClassAnnotations() {
    String json = "{}";
    List<String> expectedAnnotations = new ArrayList<>();

    PageObjectClass node = createRootNode(json).getImplementation();
    PageObjectValidationTestHelper.validateAnnotationList(
        node.getClassAnnotations(), expectedAnnotations);
  }

  /**
   * Tests that the getClassAnnotations method returns the proper annotation list for the generated
   * class with a root selector and no shadow element
   */
  @Test
  public void testGetClassAnnotationsWithRootNode() {
    String json =
        "{"
            + "  \"selector\": {"
            + "    \"css\": \"rootSelector\""
            + "  },"
            + "  \"root\": true"
            + "}";
    List<String> expectedAnnotations = new ArrayList<>();
    expectedAnnotations.add("@PageMarker.Find(css = \"rootSelector\")");

    PageObjectClass node = createRootNode(json).getImplementation();
    PageObjectValidationTestHelper.validateAnnotationList(
        node.getClassAnnotations(), expectedAnnotations);
  }

  /**
   * Tests that the getClassAnnotations method returns the proper annotation list for the generated
   * class with a platform configured
   */
  @Test
  public void testGetClassAnnotationsWithPlatformProperty() {
    String json = "{\"platform\": \"native\"}";
    List<String> expectedAnnotations = new ArrayList<>();
    expectedAnnotations.add("@PageMarker.Switch(PlatformType.NATIVE)");

    PageObjectClass node = createRootNode(json).getImplementation();
    PageObjectValidationTestHelper.validateAnnotationList(
        node.getClassAnnotations(), expectedAnnotations);
  }
}
