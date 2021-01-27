package declarative.representation;

import declarative.helpers.*;
import declarative.representation.CustomElementMethod.*;
import declarative.representation.PageObjectValidationTestHelper.MethodInfo;
import org.testng.annotations.Test;
import selenium.element.Selector;

import java.util.Collections;

import static declarative.grammar.TestUtilities.getCssSelector;
import static declarative.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static declarative.helpers.TypeUtilities.Element.actionable;
import static declarative.representation.CustomElementMethod.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Provides tests for the ComponentMethod class
 *
 * @author james.evans
 */
public class CustomElementMethodTests {

  private static final String ELEMENT_NAME = "test";
  private static final String ELEMENT_METHOD_NAME = "getTest";
  private static final String IMPORT_TYPE_SELECTOR = Selector.class.getName();
  private static final String TYPE_SHORT_NAME = "Type";
  private static final String TYPE_FULL_NAME = "my.package.Type";
  private static final TypeProvider TYPE =
      new TypeUtilities.FromString(TYPE_SHORT_NAME, TYPE_FULL_NAME);
  private static final Root INJECTED_ROOT =
      new Root(getCssSelector("css"), false, EMPTY_PARAMETERS);

  private static ElementContext getBasicScope() {
    final ElementContext scope =
        new ElementContext.Basic(ELEMENT_NAME, actionable.getType(), getCssSelector("css"));
    MethodDeclaration declaration = mock(MethodDeclaration.class);
    PageObjectMethod mock = mock(PageObjectMethod.class);
    when(declaration.getName()).thenReturn(ELEMENT_METHOD_NAME);
    when(mock.getDeclaration()).thenReturn(declaration);
    scope.setElementMethod(mock);
    return scope;
  }

  @Test
  public void testExternalComponentMethodCreation() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, TYPE_SHORT_NAME);
    info.addImpliedImportedTypes(IMPORT_TYPE_SELECTOR);
    info.addCodeLines(
        "Type instance = "
            + BASE_PAGE_OBJECT_METHOD
            + "(this.getTest(), by(\"css\", Selector.Type.CSS, false))."
            + BUILDER_METHOD
            + "(Type.class)",
        "instance");
    info.addImportedTypes(TYPE_FULL_NAME);
    PageObjectMethod method =
        new CustomElementMethod.Single(
            true, ELEMENT_NAME, INJECTED_ROOT, getBasicScope(), true, TYPE, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testComponentMethodWithParametrizedSelector() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, TYPE_SHORT_NAME);
    info.addCodeLines(
        "Type instance = "
            + BASE_PAGE_OBJECT_METHOD
            + "(this.getTest(), by(String.format(\".fakeSelector[title='%s']\", name), Selector.Type.CSS, false), false)."
            + BUILDER_METHOD
            + "(Type.class)",
        "instance.load()",
        "instance");
    info.addImportedTypes(TYPE_FULL_NAME);
    info.addImpliedImportedTypes(IMPORT_TYPE_SELECTOR);
    info.addParameter(new PageObjectValidationTestHelper.MethodParameterInfo("name", "String"));
    CustomElementMethod.Single method =
        new CustomElementMethod.Single(
            true,
            ELEMENT_NAME,
            new Root(
                getCssSelector(".fakeSelector[title='%s']"),
                false,
                Collections.singletonList(
                    new ParameterUtils.Regular("name", PrimitiveType.STRING))),
            getBasicScope(),
            false,
            TYPE,
            false);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  /** A ComponentMethod object that returns a list should be able to be created */
  @Test
  public void testComponentMethodReturningList() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, "List<Type>");
    info.addCodeLines(
        BASE_PAGE_OBJECT_METHOD
            + "(this.getTest(), by(\"css\", Selector.Type.CSS, false), true)."
            + LIST_BUILDER_METHOD
            + "(Type.class)");
    info.addImportedTypes("java.util.List");
    info.addImportedTypes(TYPE_FULL_NAME);
    info.addImpliedImportedTypes(IMPORT_TYPE_SELECTOR);
    PageObjectMethod method =
        new CustomElementMethod.Multiple(
            true, ELEMENT_NAME, INJECTED_ROOT, getBasicScope(), TYPE, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testComponentMethodWithFilterNullableFindFirst() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, TYPE_SHORT_NAME);
    info.addCodeLines(
        BASE_PAGE_OBJECT_METHOD
            + "(this.getTest(), by(\"css\", Selector.Type.CSS, false), false)."
            + BUILDER_METHOD
            + "(Type.class, elm -> elm.applyMethod())");
    info.addImportedTypes(TYPE_FULL_NAME);
    info.addImpliedImportedTypes(IMPORT_TYPE_SELECTOR);
    PageObjectMethod method =
        new CustomElementMethod.Filtered(
            true,
            ELEMENT_NAME,
            INJECTED_ROOT,
            getBasicScope(),
            TYPE,
            false,
            "applyMethod",
            EMPTY_PARAMETERS,
            MatcherType.isTrue,
            EMPTY_PARAMETERS,
            true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testComponentMethodWithFilterNotNullableFindAll() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, "List<Type>");
    info.addCodeLines(
        BASE_PAGE_OBJECT_METHOD
            + "(this.getTest(), by(\"css\", Selector.Type.CSS, false), true)."
            + LIST_BUILDER_METHOD
            + "(Type.class, elm -> elm.applyMethod())");
    info.addImportedTypes(TYPE_FULL_NAME, "java.util.List");
    info.addImpliedImportedTypes(IMPORT_TYPE_SELECTOR);
    PageObjectMethod method =
        new CustomElementMethod.Filtered(
            true,
            ELEMENT_NAME,
            INJECTED_ROOT,
            getBasicScope(),
            TYPE,
            true,
            "applyMethod",
            EMPTY_PARAMETERS,
            MatcherType.isTrue,
            EMPTY_PARAMETERS,
            false);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  /** An InjectedSelector object returns the proper string representation */
  @Test
  public void testInjectedSelector() {
    Root plain = new Root(getCssSelector("css"), false, EMPTY_PARAMETERS);
    assertThat(plain.getCodeString(), is(equalTo("by(\"css\", Selector.Type.CSS, false)")));
    Root wParams =
        new Root(
            getCssSelector(".fakeSelector[title='%s']"),
            true,
            Collections.singletonList(new ParameterUtils.Regular("name", PrimitiveType.STRING)));
    assertThat(
        wParams.getCodeString(),
        is(
            equalTo(
                "by(String.format(\".fakeSelector[title='%s']\", name), Selector.Type.CSS, true)")));
  }
}
