package utam.compiler.grammar;

import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ContainerMethod;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.grammar.UtamElement.ERR_CONTAINER_SHOULD_BE_PUBLIC;
import static utam.compiler.grammar.UtamElement.Type;
import static utam.compiler.grammar.UtamSelector_Tests.getUtamCssSelector;
import static utam.compiler.representation.ContainerMethodTests.FIRST_CONTAINER_PARAMETER;
import static utam.compiler.representation.ContainerMethodTests.SECOND_CONTAINER_PARAMETER;
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
  public void testPrivateContainer() {
    UtamElement element = getPublicContainerElement();
    element.selector = getUtamCssSelector();
    element.isPublic = false;
    UtamError e = expectThrows(UtamError.class, () -> getElementAbstraction(element));
    assertThat(
        e.getMessage(), is(equalTo(String.format(ERR_CONTAINER_SHOULD_BE_PUBLIC, ELEMENT_NAME))));
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

  /** The getDeclaredMethod method should return null for a non-public container */
  @Test
  public void testGetDeclaredMethodWithNonPublicComponent() {
    UtamElement element = getPublicContainerElement();
    element.isPublic = false;
    assertThrows(() -> getElementMethod(element));
  }

  /** The getDeclaredMethods method should return the proper value for a container */
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
    expectedMethod.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("scopeArg", "String"));
    expectedMethod.addParameter(FIRST_CONTAINER_PARAMETER);
    expectedMethod.addParameter(SECOND_CONTAINER_PARAMETER);

    expectedMethod.addCodeLines(
        "this.inContainer(this.getScopeElement(scopeArg), true).load(pageObjectType, injectedSelector)");
    PageObjectValidationTestHelper.validateMethod(method, expectedMethod);
  }

  @Test
  public void testContainerWithSelector() {
    TranslationContext context = new DeserializerUtilities().getContext("containerElement");
    ElementContext element = context.getElement("containerWithSelector");
    PageObjectMethod method = element.getElementMethod();
    assertThat(element.getType().getSimpleName(), is(equalTo("ContainerElement")));
    PageObjectValidationTestHelper.MethodInfo expectedMethod =
        new PageObjectValidationTestHelper.MethodInfo(
            "getContainerWithSelector", ContainerMethod.RETURNS_LIST.getSimpleName());
    expectedMethod.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("scopeArg", "String"));
    expectedMethod.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("selectorArg", "String"));
    expectedMethod.addParameter(FIRST_CONTAINER_PARAMETER);
    expectedMethod.addCodeLines(
        "this.inContainer(this.getScopeElement(scopeArg), false)"
            + ".loadList(pageObjectType, by(String.format(\".css%s\", selectorArg), Selector.Type.CSS))");
    PageObjectValidationTestHelper.validateMethod(method, expectedMethod);
  }
}
