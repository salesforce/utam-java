package utam.compiler.grammar;

import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import utam.core.declarative.representation.PageClassField;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.representation.PageObjectMethod;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.FieldInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;
import utam.core.selenium.element.Actionable;

import java.util.List;
import java.util.Objects;

import static utam.compiler.grammar.TestUtilities.*;
import static utam.compiler.grammar.UtamElement.*;
import static utam.compiler.grammar.UtamElementFilter_Tests.getInnerTextFilter;
import static utam.compiler.grammar.UtamSelector_Tests.getListCssSelector;
import static utam.compiler.grammar.UtamSelector_Tests.getUtamCssSelector;
import static utam.compiler.helpers.TypeUtilities.Element.actionable;
import static utam.compiler.helpers.TypeUtilities.Element.clickable;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

public class UtamElement_BasicTests {

  private static final String ELEMENT_NAME = "test";
  private static final String METHOD_NAME = "getTest";
  private static final String ACTIONABLE_TYPE_NAME = Actionable.class.getSimpleName();

  private static UtamElement.Traversal getAbstraction(UtamElement element) {
    UtamElement.Traversal res = element.getAbstraction();
    assertThat(res.getClass(), is(equalTo(UtamElement.Basic.class)));
    return res;
  }

  private static PageObjectMethod getElementMethod(UtamElement element) {
    TranslationContext context = getTestTranslationContext();
    UtamElement.Traversal abstraction = getAbstraction(element);
    return abstraction.testRootTraverse(context).getElementMethod();
  }

  private static UtamElement getPublicHtmlElement(UtamSelector selector, UtamElementFilter filter) {
    UtamElement utamElement = new UtamElement(ELEMENT_NAME);
    utamElement.selector = selector;
    utamElement.filter = filter;
    utamElement.isPublic = true;
    return utamElement;
  }

  private static UtamElement.Traversal getElementAbstraction(String json) {
    UtamElement utamElement = getDeserializedObject(json, UtamElement.class);
    return getAbstraction(utamElement);
  }

  /** The validateSimpleElement method with a filter should succeed */
  @Test
  public void testValidateSimpleElementWithFilter() {
    getAbstraction(getPublicHtmlElement(getListCssSelector(), getInnerTextFilter()));
  }

  /**
   * The validateSimpleElement method with a filter and a null selector should throw the appropriate
   * exception
   */
  @Test
  public void testValidateSimpleElementWithFilterAndNullSelectorThrows() {
    UtamError e =
        expectThrows(UtamError.class, () -> getAbstraction(getPublicHtmlElement(null, null)));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_ELEMENT_MISSING_SELECTOR_PROPERTY, ELEMENT_NAME)));
  }

  /**
   * The validateSimpleElement method with a filter and a non-list selector should throw the
   * appropriate exception
   */
  @Test
  public void testValidateSimpleElementWithFilterAndNonListSelectorThrows() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () -> getAbstraction(getPublicHtmlElement(getUtamCssSelector(), getInnerTextFilter())));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ELEMENT_FILTER_NEEDS_LIST, ELEMENT_NAME)));
  }

  /** The validateSimpleElement method with an element should succeed */
  @Test
  public void testValidateSimpleElementWithElement() {
    getAbstraction(getPublicHtmlElement(getUtamCssSelector(), null));
  }

  @Test
  public void testValidateRedundantProperties() {
    UtamElement externalElement = getPublicHtmlElement(getUtamCssSelector(), null);
    externalElement.isExternal = true;
    UtamError e = expectThrows(UtamError.class, () -> getAbstraction(externalElement));
    assertThat(e.getMessage(), is(equalTo(externalElement.getSupportedPropertiesErr(Type.BASIC))));
  }

  /** The getSimpleType method should return valid values for a null type */
  @Test
  public void testGetSimpleTypeWithNullType() {
    assertThat(
        getAbstraction(getPublicHtmlElement(getUtamCssSelector(), null))
            .testRootTraverse(getTestTranslationContext())
            .getType()
            .getFullName(),
        is(equalTo("utam.core.selenium.element.Actionable")));
  }

  /** The getSimpleType method should return valid values for a valid element type */
  @Test
  public void testGetSimpleTypeWithSimpleType() {
    UtamElement element = getPublicHtmlElement(getUtamCssSelector(), null);
    element.type = "clickable";
    assertThat(
        getAbstraction(element)
            .testRootTraverse(getTestTranslationContext())
            .getType()
            .getFullName(),
        is(equalTo("utam.core.selenium.element.Clickable")));
  }

  /** The getAsSimpleElement method should return the proper value */
  @Test
  public void testGetAsSimpleElement() {
    TranslationContext context = getTestTranslationContext();
    ElementContext scopeElement = new ElementContext.Basic(ELEMENT_NAME);
    UtamElement.Traversal element =
        getAbstraction(getPublicHtmlElement(getUtamCssSelector(), null));
    ElementContext elementContext = element.traverse(context, scopeElement, false)[0];
    assertThat(elementContext.getType().getSimpleName(), is(equalTo(ACTIONABLE_TYPE_NAME)));
    assertThat(context.getFields(), hasSize(1));
    FieldInfo createdFieldInfo = new FieldInfo(ELEMENT_NAME, ACTIONABLE_TYPE_NAME);
    createdFieldInfo.addAnnotationText(
        String.format(
            "@ElementMarker.Find(css = \"%s\", scope = \"%s\")",
            UtamSelector_Tests.SELECTOR_STRING, ELEMENT_NAME));
    createdFieldInfo.validateField(context.getFields().get(0));
  }

  /**
   * The getAsSimpleElement method should return the proper value for an element with a root scope
   * element
   */
  @Test
  public void testGetAsSimpleElementWithRootScope() {
    TranslationContext context = getTestTranslationContext();
    UtamElement.Traversal element =
        getAbstraction(getPublicHtmlElement(getUtamCssSelector(), null));
    ElementContext elementContext = element.testRootTraverse(context);
    assertThat(elementContext.getType().getSimpleName(), is(equalTo(ACTIONABLE_TYPE_NAME)));
    assertThat(context.getFields(), hasSize(1));
    FieldInfo createdFieldInfo = new FieldInfo(ELEMENT_NAME, ACTIONABLE_TYPE_NAME);
    createdFieldInfo.addAnnotationText(
        String.format("@ElementMarker.Find(css = \"%s\")", UtamSelector_Tests.SELECTOR_STRING));
    createdFieldInfo.validateField(context.getFields().get(0));
  }

  /** The getAsSimpleElement method should return the proper value for an element with a filter */
  @Test
  public void testGetAsSimpleElementWithFilter() {
    TranslationContext context = getTestTranslationContext();
    UtamElement.Traversal element =
        getAbstraction(getPublicHtmlElement(getListCssSelector(), getInnerTextFilter()));
    ElementContext elementContext = element.testRootTraverse(context);
    assertThat(elementContext.getType().getSimpleName(), is(equalTo(ACTIONABLE_TYPE_NAME)));
    assertThat(context.getFields(), hasSize(1));
    FieldInfo createdFieldInfo = new FieldInfo(ELEMENT_NAME, ACTIONABLE_TYPE_NAME);
    createdFieldInfo.addAnnotations(
        String.format("@ElementMarker.Find(css = \"%s\")", UtamSelector_Tests.SELECTOR_STRING));
    createdFieldInfo.validateField(context.getFields().get(0));
  }

  /** The getAsSimpleElement method should return the proper value for a list selector */
  @Test
  public void testGetAsSimpleElementWithListSelector() {
    TranslationContext context = getTestTranslationContext();
    UtamElement.Traversal element =
        getAbstraction(getPublicHtmlElement(getUtamCssSelector(), null));
    element.testRootTraverse(context);
    assertThat(context.getFields(), hasSize(1));
    FieldInfo createdFieldInfo = new FieldInfo(ELEMENT_NAME, ACTIONABLE_TYPE_NAME);
    createdFieldInfo.addAnnotations(
        String.format("@ElementMarker.Find(css = \"%s\")", UtamSelector_Tests.SELECTOR_STRING));
    createdFieldInfo.validateField(context.getFields().get(0));
  }

  /**
   * The getAsSimpleElement method should return the proper value for a list selector with a filter
   */
  @Test
  public void testGetAsSimpleElementWithListSelectorAndFilter() {
    TranslationContext context = getTestTranslationContext();
    UtamElement.Traversal element =
        getAbstraction(getPublicHtmlElement(getListCssSelector(), getInnerTextFilter()));
    element.testRootTraverse(context);
    assertThat(context.getFields(), hasSize(1));
    FieldInfo createdFieldInfo = new FieldInfo(ELEMENT_NAME, ACTIONABLE_TYPE_NAME);
    createdFieldInfo.addAnnotations(
        String.format("@ElementMarker.Find(css = \"%s\")", UtamSelector_Tests.SELECTOR_STRING));
    createdFieldInfo.validateField(context.getFields().get(0));
  }

  /** The getNextScope method should return the proper value for an element */
  @Test
  public void testGetNextScopeWithElement() {
    TranslationContext context = getTestTranslationContext();
    ElementContext scopeElement = new ElementContext.Basic("scope");
    assertThat(
        getAbstraction(getPublicHtmlElement(getUtamCssSelector(), null))
            .traverse(context, scopeElement, false)[0]
            .getType()
            .getSimpleName(),
        is(equalTo(ACTIONABLE_TYPE_NAME)));
  }

  /** The traverse method should add the proper methods to the element object */
  @Test
  public void testTraverse() {
    UtamElement element = getPublicHtmlElement(getUtamCssSelector(), null);
    MethodInfo info = new MethodInfo(METHOD_NAME, ACTIONABLE_TYPE_NAME);
    info.addCodeLine("this." + ELEMENT_NAME);
    PageObjectValidationTestHelper.validateMethod(
        Objects.requireNonNull(getElementMethod(element)), info);
  }

  /**
   * The traverse method should not add a method to the context object with a public and root object
   */
  @Test
  public void testTraverseWithNonPublicAndNonRoot() {
    UtamElement element = new UtamElement(ELEMENT_NAME);
    element.selector = getUtamCssSelector();
    assertThat(getElementMethod(element), is(not(nullValue())));
  }

  /**
   * The traverse method should add the proper methods to the element object for an element with
   * nested elements
   */
  @Test
  public void testTraverseWithInnerElements() {
    final String ELEMENT_NAME = "outerElement";
    UtamElement element = new UtamElement(ELEMENT_NAME);
    element.isPublic = true;
    element.selector = getUtamCssSelector();
    element.elements = new UtamElement[] {getPublicHtmlElement(new UtamSelector(".other"), null)};
    MethodInfo info = new MethodInfo("getOuterElement", ACTIONABLE_TYPE_NAME);
    info.addCodeLine(String.format("this.%s", ELEMENT_NAME));
    PageObjectValidationTestHelper.validateMethod(
        Objects.requireNonNull(getElementMethod(element)), info);
  }

  /**
   * The traverse method should add the proper methods to the element object for an element with
   * nested shadow elements
   */
  @Test
  public void testTraverseWithInnerShadowElements() {
    final String ELEMENT_NAME = "outerElement";
    UtamElement element = new UtamElement(ELEMENT_NAME);
    element.isPublic = true;
    element.selector = getUtamCssSelector();
    element.shadow =
        new UtamShadowElement(
            new UtamElement[] {getPublicHtmlElement(new UtamSelector(".other"), null)});
    MethodInfo info = new MethodInfo("getOuterElement", ACTIONABLE_TYPE_NAME);
    info.addCodeLine(String.format("this.%s", ELEMENT_NAME));
    PageObjectValidationTestHelper.validateMethod(
        Objects.requireNonNull(getElementMethod(element)), info);
  }

  /** The getDeclaredMethods method should return the proper value */
  @Test
  public void testGetDeclaredMethods() {
    UtamElement element = getPublicHtmlElement(getUtamCssSelector(), null);
    element.getAbstraction().testRootTraverse(getTestTranslationContext());
    MethodInfo info = new MethodInfo(METHOD_NAME, ACTIONABLE_TYPE_NAME);
    info.addCodeLine("this." + ELEMENT_NAME);
    PageObjectValidationTestHelper.validateMethod(
        Objects.requireNonNull(getElementMethod(element)), info);
  }

  /**
   * The getDeclaredMethods method should return the proper value for an element with a list
   * selector
   */
  @Test
  public void testGetDeclaredMethodsWithList() {
    UtamElement element = getPublicHtmlElement(getListCssSelector(), null);
    MethodInfo info = new MethodInfo(METHOD_NAME, String.format("List<%s>", ACTIONABLE_TYPE_NAME));
    info.addCodeLine("element(this.test, false).buildList(Actionable.class)");
    PageObjectValidationTestHelper.validateMethod(
        Objects.requireNonNull(getElementMethod(element)), info);
  }

  /**
   * The getDeclaredMethods method should return the proper value for an element not marked public
   */
  @Test
  public void testGetDeclaredMethodsWithComponentPrivateElement() {
    UtamElement element = new UtamElement(ELEMENT_NAME);
    element.selector = getUtamCssSelector();
    assertThat(getElementMethod(element), is(not(nullValue())));
  }

  @Test
  public void testElementWithListCantHaveNestedElements() {
    UtamElement element = new UtamElement(ELEMENT_NAME);
    element.selector = getListCssSelector();
    element.elements = new UtamElement[0];
    UtamError e = expectThrows(UtamError.class, () -> element.getAbstraction());
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ELEMENT_NESTED_ELEMENTS, ELEMENT_NAME)));
  }

  @Test
  public void testElementNodeWithNestedElements() {
    DeserializerUtilities.Result res = new DeserializerUtilities().getResultFromFile("nestedElements");
    TranslationContext context = res.getContext();
    ElementContext one = context.getElement("one");
    assertThat(one.getType(), is(equalTo(TypeUtilities.Element.actionable)));
    ElementContext nested = context.getElement("nestedCustom");
    assertThat(nested.getType().getSimpleName(), is(equalTo("ComponentType")));
    ElementContext nestedInShadow = context.getElement("nestedInsideShadow");
    assertThat(nestedInShadow.getType().getSimpleName(), is(equalTo(ACTIONABLE_TYPE_NAME)));

    PageObjectDeclaration objectDeclaration = res.getPageObject();
    List<PageClassField> fields = objectDeclaration.getImplementation().getFields();
    assertThat(fields.size(), is(equalTo(2)));
    assertThat(
        fields.get(0).getComments(),
        is(equalTo("javascript: $(<root selector here>).querySelector(\".one\")")));
  }

  @Test
  public void testDeserializationWithoutNameThrows() {
    String json = "{}";
    UtamError e =
        expectThrows(UtamError.class, () -> getDeserializedObject(json, UtamElement.class));
    assertThat(e.getCause().getMessage(), containsString(JACKSON_MISSING_REQUIRED_PROPERTY_ERROR));
  }

  @Test
  public void testElementNodeMinimalAttributes() {
    String json =
        "{"
            + "  \"name\": \"simpleElement\","
            + "  \"selector\": {"
            + "    \"css\": \"simpleSelector\""
            + "  }"
            + "}";
    ElementContext elementContext =
        getElementAbstraction(json).testRootTraverse(TestUtilities.getTestTranslationContext());
    assertThat(elementContext.getName(), is(equalTo("simpleElement")));
    assertThat(elementContext.isList(), is(false));
    assertThat(
        elementContext.getElementMethod().getDeclaration().getName(),
        is(equalTo("getSimpleElementElement")));
  }

  /** Test a valid element node with the list property set to true */
  @Test
  public void testElementNodeWithList() {
    String json =
        "{"
            + "  \"name\": \"simpleElement\","
            + "  \"selector\": {"
            + "    \"css\": \"simpleSelector\","
            + "    \"returnAll\": true"
            + "  }"
            + "}";
    ElementContext elementContext =
        getElementAbstraction(json).testRootTraverse(TestUtilities.getTestTranslationContext());
    assertThat(elementContext.getName(), is(equalTo("simpleElement")));
    assertThat(elementContext.isList(), is(true));
    assertThat(
        elementContext.getElementMethod().getDeclaration().getName(),
        is(equalTo("getSimpleElementElement")));
    assertThat(elementContext.getElementMethod().isPublic(), is(false));
  }

  @Test
  public void testValidPublicElementNodeWithType() {
    String json =
        "{"
            + "  \"name\": \"simpleElement\","
            + "  \"type\": \"clickable\","
            + "  \"selector\": {"
            + "    \"css\": \"customSelector\""
            + "  },"
            + "  \"public\": true"
            + "}";
    ElementContext elementContext =
        getElementAbstraction(json).testRootTraverse(TestUtilities.getTestTranslationContext());
    assertThat(
        elementContext.getElementMethod().getDeclaration().getName(),
        is(equalTo("getSimpleElement")));
    assertThat(elementContext.getElementMethod().isPublic(), is(true));
    assertThat(elementContext.getType(), is(equalTo(clickable)));
  }

  @Test
  public void testValidPrivateElementNode() {
    String json =
        "{"
            + "  \"name\": \"test\","
            + "  \"selector\": {"
            + "    \"css\": \"css\""
            + "  },"
            + "  \"public\": false"
            + "}";
    ElementContext elementContext =
        getElementAbstraction(json).testRootTraverse(TestUtilities.getTestTranslationContext());
    assertThat(
        elementContext.getElementMethod().getDeclaration().getName(),
        is(equalTo("getTestElement")));
    assertThat(elementContext.getElementMethod().isPublic(), is(false));
    assertThat(elementContext.getType(), is(equalTo(actionable)));
  }
}
