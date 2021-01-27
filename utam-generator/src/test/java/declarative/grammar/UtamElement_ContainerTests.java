package declarative.grammar;

import declarative.helpers.ElementContext;
import declarative.helpers.TranslationContext;
import declarative.helpers.TypeUtilities;
import declarative.representation.ContainerMethodTests;
import declarative.representation.PageClassField;
import declarative.representation.PageObjectMethod;
import declarative.representation.PageObjectValidationTestHelper;
import framework.base.ContainerElementPageObject;
import framework.consumer.ContainerElement;
import framework.consumer.UtamError;
import org.hamcrest.Matchers;
import org.testng.annotations.Test;

import java.util.Objects;

import static declarative.grammar.TestUtilities.getCssSelector;
import static declarative.grammar.TestUtilities.getTestTranslationContext;
import static declarative.grammar.UtamElement.Type;
import static declarative.grammar.UtamSelector_Tests.getUtamCssSelector;
import static declarative.representation.ContainerMethodTests.FIRST_CONTAINER_PARAMETER;
import static declarative.representation.ContainerMethodTests.SECOND_CONTAINER_PARAMETER;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamElement_ContainerTests {

  private static final String CONTAINER_TYPE_NAME = ContainerElement.class.getSimpleName();
  private static final String METHOD_NAME = "getTest";
  private static final String ELEMENT_NAME = "test";

  private static UtamElement getPublicContainerElement() {
    UtamElement utamElement = new UtamElement(ELEMENT_NAME);
    utamElement.type = "container";
    utamElement.isPublic = true;
    return utamElement;
  }

  private static PageObjectMethod getElementMethod(UtamElement element) {
    TranslationContext context = getTestTranslationContext();
    UtamElement.Traversal abstraction = getElementAbstraction(element);
    return abstraction.testRootTraverseComponentOrContainer(context).getElementMethod();
  }

  private static UtamElement.Traversal getElementAbstraction() {
    return getElementAbstraction(getPublicContainerElement());
  }

  static UtamElement.Traversal getElementAbstraction(UtamElement element) {
    UtamElement.Traversal res = element.getAbstraction();
    assertThat(res, is(instanceOf(UtamElement.Container.class)));
    return res;
  }

  @Test
  public void testValid() {
    UtamElement.Traversal element = getElementAbstraction();
    TranslationContext context = getTestTranslationContext();
    ElementContext self = element.testRootTraverseComponentOrContainer(context);
    PageObjectMethod method = self.getElementMethod();
    PageClassField field = self.getElementField();
    assertThat(field, Matchers.is(notNullValue()));
    assertThat(method, Matchers.is(notNullValue()));
    PageObjectValidationTestHelper.MethodInfo expectedMethod =
        new PageObjectValidationTestHelper.MethodInfo(
            METHOD_NAME, ContainerMethodTests.RETURN_TYPE);
    expectedMethod.addCodeLine(
        String.format(
            "if (pageObjectType.equals(%s.class)) {",
            ContainerElementPageObject.class.getSimpleName()));
    expectedMethod.addCodeLine(
        String.format(
            "return (T)(new %s(this.%s))",
            ContainerElementPageObject.class.getSimpleName(), ELEMENT_NAME));
    expectedMethod.addCodeLine("}");
    expectedMethod.addCodeLine(
        "this." + field.getName() + "." + ContainerMethodTests.EXPECTED_CODE_LOAD);
    expectedMethod.addParameter(FIRST_CONTAINER_PARAMETER);
    expectedMethod.addParameter(SECOND_CONTAINER_PARAMETER);
    PageObjectValidationTestHelper.validateMethod(method, expectedMethod);
  }

  @Test
  public void testNotAllowedSelector() {
    UtamElement element = getPublicContainerElement();
    element.selector = getUtamCssSelector();
    UtamError e = expectThrows(UtamError.class, () -> getElementAbstraction(element));
    assertThat(e.getMessage(), is(equalTo(element.getSupportedPropertiesErr(Type.CONTAINER))));
  }

  @Test
  public void testNotAllowedFilter() {
    UtamElement element = getPublicContainerElement();
    element.filter = UtamElementFilter_Tests.getInnerTextFilter();
    UtamError e = expectThrows(UtamError.class, () -> getElementAbstraction(element));
    assertThat(e.getMessage(), is(equalTo(element.getSupportedPropertiesErr(Type.CONTAINER))));
  }

  @Test
  public void testNotAllowedShadow() {
    UtamElement element = getPublicContainerElement();
    element.shadow = new UtamShadowElement(new UtamElement[] {});
    UtamError e = expectThrows(UtamError.class, () -> getElementAbstraction(element));
    assertThat(e.getMessage(), is(equalTo(element.getSupportedPropertiesErr(Type.CONTAINER))));
  }

  @Test
  public void testNotAllowedNestedElements() {
    UtamElement element = getPublicContainerElement();
    element.elements = new UtamElement[] {};
    UtamError e = expectThrows(UtamError.class, () -> getElementAbstraction(element));
    assertThat(e.getMessage(), is(equalTo(element.getSupportedPropertiesErr(Type.CONTAINER))));
  }

  @Test
  public void testNotAllowedExternal() {
    UtamElement element = getPublicContainerElement();
    element.isExternal = true;
    UtamError e = expectThrows(UtamError.class, () -> getElementAbstraction(element));
    assertThat(e.getMessage(), is(equalTo(element.getSupportedPropertiesErr(Type.CONTAINER))));
  }

  @Test
  public void testNotAllowedLoad() {
    UtamElement element = getPublicContainerElement();
    element.isNullable = false;
    UtamError e = expectThrows(UtamError.class, () -> getElementAbstraction(element));
    assertThat(e.getMessage(), is(equalTo(element.getSupportedPropertiesErr(Type.CONTAINER))));
  }

  @Test
  public void testGetAnnotationsWithRootScope() {
    UtamElement element = getPublicContainerElement();
    UtamElement.Traversal abstraction = getElementAbstraction(element);
    TranslationContext context = getTestTranslationContext();
    abstraction.testRootTraverse(context);
    assertThat(context.getFields(), hasSize(1));
    PageObjectValidationTestHelper.FieldInfo createdFieldInfo =
        new PageObjectValidationTestHelper.FieldInfo(
            ELEMENT_NAME, UtamElement_ContainerTests.CONTAINER_TYPE_NAME);
    createdFieldInfo.addAnnotationText("@ElementMarker.Find(css = \"\")");
    createdFieldInfo.validateField(context.getFields().get(0));
  }

  @Test
  public void testGetAnnotations() {
    UtamElement element = getPublicContainerElement();
    TranslationContext context = getTestTranslationContext();
    ElementContext scopeElement =
        new ElementContext.Basic(
            "ScopeElement",
            new TypeUtilities.FromString("actionable"),
            getCssSelector(".fakeSelector"));
    UtamElement.Traversal abstraction = getElementAbstraction(element);
    abstraction.traverse(context, scopeElement, false);
    assertThat(context.getFields(), hasSize(1));
    PageObjectValidationTestHelper.FieldInfo createdFieldInfo =
        new PageObjectValidationTestHelper.FieldInfo(ELEMENT_NAME, CONTAINER_TYPE_NAME);
    createdFieldInfo.addAnnotationText("@ElementMarker.Find(css = \"\", scope = \"ScopeElement\")");
    createdFieldInfo.validateField(context.getFields().get(0));
  }

  /** The getDeclaredMethod method should return null for a non-public container */
  @Test
  public void testGetDeclaredMethodWithNonPublicComponent() {
    UtamElement element = getPublicContainerElement();
    element.isPublic = false;
    assertThrows(() -> getElementMethod(element));
  }

  /** The getDeclaredMethods method should return the proper value for a container */
  @Test
  public void testGetDeclaredMethodsWithComponent() {
    UtamElement element = getPublicContainerElement();
    PageObjectValidationTestHelper.MethodInfo expectedMethod =
        new PageObjectValidationTestHelper.MethodInfo(
            METHOD_NAME, ContainerMethodTests.RETURN_TYPE);
    expectedMethod.addCodeLine(
        String.format(
            "if (pageObjectType.equals(%s.class)) {",
            ContainerElementPageObject.class.getSimpleName()));
    expectedMethod.addCodeLine(
        String.format(
            "return (T)(new %s(this.%s))",
            ContainerElementPageObject.class.getSimpleName(), ELEMENT_NAME));
    expectedMethod.addCodeLine("}");
    expectedMethod.addCodeLine("this.test." + ContainerMethodTests.EXPECTED_CODE_LOAD);
    expectedMethod.addParameter(FIRST_CONTAINER_PARAMETER);
    expectedMethod.addParameter(SECOND_CONTAINER_PARAMETER);

    PageObjectValidationTestHelper.validateMethod(
        Objects.requireNonNull(getElementMethod(element)), expectedMethod);
  }

  @Test
  public void testNestedContainerElement() {
    TranslationContext context = new DeserializerUtilities().getContext("containerElement");
    ElementContext element = context.getElement("nestedContainer");
    assertThat(element.isList(), is(false));
    PageObjectMethod method = element.getElementMethod();
    assertThat(element.getType().getSimpleName(), is(equalTo("ContainerElement")));
    PageObjectValidationTestHelper.MethodInfo expectedMethod =
        new PageObjectValidationTestHelper.MethodInfo(
            "getNestedContainer", "<T extends PageObject> T");
    expectedMethod.addParameter(FIRST_CONTAINER_PARAMETER);
    expectedMethod.addParameter(SECOND_CONTAINER_PARAMETER);
    expectedMethod.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("scopeArg", "String"));
    expectedMethod.addCodeLines(
        "if (pageObjectType.equals(ContainerElementPageObject.class)) {",
        "return (T)(new ContainerElementPageObject(this.nestedContainer))",
        "}",
        "setParameters(this.nestedContainer, scopeArg).load(pageObjectType, injectedSelector)");
    PageObjectValidationTestHelper.validateMethod(method, expectedMethod);
  }
}
