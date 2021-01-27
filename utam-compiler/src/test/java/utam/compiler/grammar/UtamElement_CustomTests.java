package utam.compiler.grammar;

import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.TranslationContext;
import declarative.representation.PageObjectMethod;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import framework.consumer.UtamError;
import org.testng.annotations.Test;

import java.util.Objects;

import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.grammar.UtamElement.*;
import static utam.compiler.grammar.UtamElementFilter_Tests.getInnerTextFilter;
import static utam.compiler.grammar.UtamSelector_Tests.getListCssSelector;
import static utam.compiler.grammar.UtamSelector_Tests.getUtamCssSelector;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

public class UtamElement_CustomTests {

  private static final String ELEMENT_NAME = "test";
  private static final String METHOD_NAME = "getTest";
  private static final String COMPONENT_TYPE_URI = "utam-test/pageObjects/test/componentName";
  private static final String COMPONENT_TYPE_SHORT_NAME = "ComponentName";
  private static final String COMPONENT_TYPE_LONG_NAME = "utam.test.pageobjects.test.ComponentName";
  private static final String SCOPE_ELEMENT_NAME = "scope";

  private static UtamElement getPublicComponentElement(
      UtamSelector selector, UtamElementFilter filter) {
    UtamElement utamElement = new UtamElement(ELEMENT_NAME);
    utamElement.type = COMPONENT_TYPE_URI;
    utamElement.isPublic = true;
    utamElement.selector = selector;
    utamElement.filter = filter;
    return utamElement;
  }

  private static UtamElement getPublicComponentElement() {
    return getPublicComponentElement(getUtamCssSelector(), null);
  }

  private static UtamElement.Traversal getAbstraction(UtamElement element) {
    UtamElement.Traversal traversal = element.getAbstraction();
    assertThat(traversal, is(instanceOf(UtamElement.Custom.class)));
    return traversal;
  }

  private static PageObjectMethod getElementMethod(UtamElement element) {
    TranslationContext context = getTestTranslationContext();
    UtamElement scope = new UtamElement(SCOPE_ELEMENT_NAME);
    scope.selector = new UtamSelector("scopeSelector");
    scope.elements = new UtamElement[] {element};
    scope.testTraverse(context);
    return context.getElement(element.name).getElementMethod();
  }

  /** The getSimpleType method for an invalid element type should throw the proper exception */
  @Test
  public void testGetSimpleTypeWithInvalidTypeThrows() {
    UtamElement element = new UtamElement(ELEMENT_NAME);
    element.type = "invalid";
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ELEMENT_OF_UNKNOWN_TYPE, ELEMENT_NAME)));
  }

  /** The validateComponentElement method with a component should succeed */
  @Test
  public void testValidateComponentElement() {
    UtamElement element = getPublicComponentElement(getUtamCssSelector(), null);
    getAbstraction(element);
  }

  /** The validateComponentElement method with a component should succeed */
  @Test
  public void testValidateComponentElementWithFilter() {
    UtamElement element = getPublicComponentElement(getListCssSelector(), getInnerTextFilter());
    getAbstraction(element);
  }

  /**
   * The validateComponentElement method with a component and a null selector should throw the
   * appropriate exception
   */
  @Test
  public void testValidateComponentElementWithNullSelectorThrows() {
    UtamElement element = getPublicComponentElement(null, null);
    UtamError e = expectThrows(UtamError.class, () -> getAbstraction(element));
    assertThat(
        e.getMessage(), is(String.format(ERR_ELEMENT_MISSING_SELECTOR_PROPERTY, ELEMENT_NAME)));
  }

  /**
   * The validateComponentElement method with a filter and a non-list selector should throw the
   * appropriate exception
   */
  @Test
  public void testValidateComponentWithFilterAndNonListSelectorThrows() {
    UtamElement element = getPublicComponentElement(getUtamCssSelector(), getInnerTextFilter());
    UtamError e = expectThrows(UtamError.class, () -> getAbstraction(element));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ELEMENT_FILTER_NEEDS_LIST, ELEMENT_NAME)));
  }

  /**
   * The validateComponentElement method with a component and nested elements should throw the
   * appropriate exception
   */
  @Test
  public void testValidateComponentElementWithInnerElementsThrows() {
    UtamElement element = getPublicComponentElement();
    element.elements = new UtamElement[] {};
    UtamError e = expectThrows(UtamError.class, () -> getAbstraction(element));
    assertThat(e.getMessage(), containsString(element.getSupportedPropertiesErr(Type.CUSTOM)));
  }

  /**
   * The validateComponentElement method with a component and nested shadow elements should throw
   * the appropriate exception
   */
  @Test
  public void testValidateComponentElementWithInnerShadowElementsThrows() {
    UtamElement element = getPublicComponentElement();
    element.shadow = new UtamShadowElement(new UtamElement[] {});
    UtamError e = expectThrows(UtamError.class, () -> getAbstraction(element));
    assertThat(e.getMessage(), containsString(element.getSupportedPropertiesErr(Type.CUSTOM)));
  }

  /** The getAsComponent method should return the proper value */
  @Test
  public void testGetAsComponent() {
    UtamPageObject object = new UtamPageObject();
    TranslationContext context = getTestTranslationContext();
    object.elements = new UtamElement[] {getPublicComponentElement()};
    object.compile(context);
    ElementContext elementContext = context.getElement(ELEMENT_NAME);
    assertThat(elementContext.isList(), is(false));
    assertThat(elementContext.getType().getSimpleName(), is(equalTo(COMPONENT_TYPE_SHORT_NAME)));
    assertThat(elementContext.getType().getFullName(), is(equalTo(COMPONENT_TYPE_LONG_NAME)));
  }

  /** The getAsComponent method should return the proper value for a list selector */
  @Test
  public void testGetAsComponentWithListSelector() {
    UtamPageObject object = new UtamPageObject();
    TranslationContext context = getTestTranslationContext();
    object.elements = new UtamElement[] {getPublicComponentElement(getListCssSelector(), null)};
    object.compile(context);
    ElementContext elementContext = context.getElement(ELEMENT_NAME);
    assertThat(elementContext.isList(), is(true));
    assertThat(elementContext.getType().getSimpleName(), is(COMPONENT_TYPE_SHORT_NAME));
  }

  /**
   * The getAsComponent method should return the proper value for a list selector with an element
   * filter
   */
  @Test
  public void testGetAsComponentWithListSelectorAndFilter() {
    UtamPageObject object = new UtamPageObject();
    TranslationContext context = getTestTranslationContext();
    object.elements =
        new UtamElement[] {
          getPublicComponentElement(
              getListCssSelector(),
              new UtamElementFilter("apply", new UtamMatcher(MatcherType.isTrue, null)))
        };
    object.compile(context);
    ElementContext elementContext = context.getElement(ELEMENT_NAME);
    assertThat(elementContext.isList(), is(true));
    assertThat(elementContext.getType().getSimpleName(), is(equalTo(COMPONENT_TYPE_SHORT_NAME)));
  }

  @Test
  public void testGetDeclaredMethodsWithComponent() {
    UtamElement element = getPublicComponentElement();
    MethodInfo expectedMethodInfo = new MethodInfo(METHOD_NAME, COMPONENT_TYPE_SHORT_NAME);
    expectedMethodInfo.addCodeLines(
        "ComponentName instance = inScope(this.getScopeElement(), by(\"selector\", Selector.Type.CSS, false), false)"
            + ".build(ComponentName.class)",
        "instance.load()",
        "instance");
    PageObjectValidationTestHelper.validateMethod(
        Objects.requireNonNull(getElementMethod(element)), expectedMethodInfo);
  }

  /** The getDeclaredMethod method should return null for a non-public component */
  @Test
  public void testGetDeclaredMethodWithNonPublicComponent() {
    UtamElement element = getPublicComponentElement(getUtamCssSelector(), null);
    element.isPublic = false;
    assertThat(getElementMethod(element), is(not(nullValue())));
  }

  /**
   * The getDeclaredMethod method should return the proper value for a component with external set
   * to true
   */
  @Test
  public void testGetDeclaredMethodWithExternalComponent() {
    UtamElement element = getPublicComponentElement(getUtamCssSelector(), null);
    element.isExternal = true;
    MethodInfo expectedMethod = new MethodInfo(METHOD_NAME, COMPONENT_TYPE_SHORT_NAME);
    expectedMethod.addCodeLines(
        "ComponentName instance = "
            + "inScope(this.getScopeElement(), by(\"selector\", Selector.Type.CSS, false)).build(ComponentName.class)",
        "instance");
    PageObjectMethod method = getElementMethod(element);
    PageObjectValidationTestHelper.validateMethod(method, expectedMethod);
  }

  @Test
  public void testElementWithListCantBeExternal() {
    UtamElement element = getPublicComponentElement(getListCssSelector(), null);
    element.isExternal = true;
    UtamError e = expectThrows(UtamError.class, () -> element.getAbstraction());
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_ELEMENT_EXTERNAL_NOT_ALLOWED, ELEMENT_NAME)));
  }

  @Test
  public void testNestedCustomList() {
    TranslationContext context = new DeserializerUtilities().getContext("customElement");
    ElementContext element = context.getElement("nestedList");
    assertThat(element.getName(), is(equalTo("nestedList")));
    assertThat(element.isList(), is(equalTo(true)));
    PageObjectMethod method = element.getElementMethod();
    assertThat(element.getType().getSimpleName(), is(equalTo("Test")));
    MethodInfo expectedMethod = new MethodInfo("getNestedList", "List<Test>");
    expectedMethod.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("arg1", "String"));
    expectedMethod.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("arg2", "String"));
    expectedMethod.addCodeLines(
        "inScope(this.getElementElement(arg1), by(String.format(\"selector2 %s\", arg2), Selector.Type.CSS, false), false)"
            + ".buildList(Test.class)");
    PageObjectValidationTestHelper.validateMethod(method, expectedMethod);
  }
}
