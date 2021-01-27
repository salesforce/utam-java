package declarative.representation;

import declarative.helpers.ElementContext;
import declarative.helpers.ParameterUtils;
import declarative.helpers.PrimitiveType;
import declarative.representation.PageObjectValidationTestHelper.MethodInfo;
import declarative.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import org.testng.annotations.Test;

import java.util.Collections;

import static declarative.grammar.TestUtilities.getCssSelector;
import static declarative.helpers.ElementContext.ROOT_SCOPE;
import static declarative.helpers.TypeUtilities.Element.actionable;
import static declarative.helpers.TypeUtilities.Element.clickable;
import static declarative.representation.ElementMethod.BASE_PAGE_OBJECT_METHOD;
import static declarative.representation.ElementMethod.LIST_BUILDER_METHOD;

/**
 * Provides tests for the ElementMethod class
 *
 * @author james.evans
 */
public class ElementMethodTests {

  private static final String ELEMENT_NAME = "test";
  private static final String ELEMENT_METHOD_NAME = "getTest";
  private static final TypeProvider ACTIONABLE_TYPE = actionable.getType();
  private static final TypeProvider CLICKABLE_TYPE = clickable.getType();

  @Test
  public void testSingleElementGetterMethodCreated() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, ACTIONABLE_TYPE.getSimpleName());
    info.addCodeLine("this." + ELEMENT_NAME);
    info.addImportedTypes(ACTIONABLE_TYPE.getFullName());

    ElementContext element =
        new ElementContext.Basic(ELEMENT_NAME, ACTIONABLE_TYPE, getCssSelector(".css"));
    PageObjectMethod method = new ElementMethod.Single(element, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testSingleElementWithParametersGetterMethodCreated() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, CLICKABLE_TYPE.getSimpleName());
    info.addCodeLine("element(this.test).build(Clickable.class, selectorArg)");
    info.addImportedTypes(CLICKABLE_TYPE.getFullName());
    info.addParameter(new MethodParameterInfo("selectorArg", "String"));
    ElementContext element =
        new ElementContext.Basic(
            ROOT_SCOPE,
            ELEMENT_NAME,
            CLICKABLE_TYPE,
            getCssSelector(".css[%s]"),
            false,
            Collections.singletonList(
                new ParameterUtils.Regular("selectorArg", PrimitiveType.STRING)));
    PageObjectMethod method = new ElementMethod.Single(element, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testListElementMethodCreation() {
    MethodInfo info = new MethodInfo(ELEMENT_METHOD_NAME, "List<Actionable>");
    info.addCodeLine(
        BASE_PAGE_OBJECT_METHOD + "(this.test)." + LIST_BUILDER_METHOD + "(Actionable.class)");
    info.addImportedTypes("java.util.List", ACTIONABLE_TYPE.getFullName());
    ElementContext element =
        new ElementContext.Basic(ELEMENT_NAME, ACTIONABLE_TYPE, getCssSelector("css"), true);
    PageObjectMethod method = new ElementMethod.Multiple(element, true);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }
}
