package declarative.representation;

import declarative.helpers.ElementContext;
import declarative.helpers.ParameterUtils;
import declarative.helpers.PrimitiveType;
import declarative.representation.PageObjectValidationTestHelper.MethodInfo;
import declarative.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import framework.base.ContainerElementPageObject;
import framework.base.PageObject;
import org.testng.annotations.Test;
import selenium.element.Selector;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static declarative.grammar.TestUtilities.getCssSelector;
import static declarative.helpers.ElementContext.ROOT_SCOPE;
import static declarative.helpers.TypeUtilities.Element.actionable;
import static declarative.representation.ContainerMethod.*;

/**
 * ContainerMethod representation class tests
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public class ContainerMethodTests {

  public static final String RETURN_TYPE = RETURN_TYPE_STRING;
  public static final String EXPECTED_CODE_LOAD = "load(pageObjectType, injectedSelector)";
  public static final MethodParameterInfo FIRST_CONTAINER_PARAMETER =
      new PageObjectValidationTestHelper.MethodParameterInfo(
          PAGE_OBJECT_TYPE_PARAMETER_NAME, PAGE_OBJECT_TYPE_PARAMETER_TYPE.getSimpleName());
  public static final MethodParameterInfo SECOND_CONTAINER_PARAMETER =
      new PageObjectValidationTestHelper.MethodParameterInfo(
          INJECTED_SELECTOR_PARAMETER_NAME, INJECTED_SELECTOR_PARAMETER_TYPE.getSimpleName());
  private static final String METHOD_NAME = "getContainer";
  private static final String ELEMENT_NAME = "container";

  @Test
  public void testContainerMethodWithoutParameters() {
    MethodInfo info = new MethodInfo(METHOD_NAME, RETURN_TYPE);
    info.addCodeLine(
        String.format(
            "if (pageObjectType.equals(%s.class)) {",
            CONTAINER_ELEMENT_PAGE_OBJECT_TYPE.getSimpleName()));
    info.addCodeLine(
        String.format(
            "return (T)(new %s(this.%s))",
            CONTAINER_ELEMENT_PAGE_OBJECT_TYPE.getSimpleName(), ELEMENT_NAME));
    info.addCodeLine("}");
    info.addCodeLine(String.format("this.%s.%s", ELEMENT_NAME, EXPECTED_CODE_LOAD));
    info.addImportedTypes(
        PageObject.class.getName(),
        Selector.class.getName());
    info.addImpliedImportedTypes(
            PageObject.class.getName(),
            Selector.class.getName(),
            ContainerElementPageObject.class.getName());
    info.addParameter(FIRST_CONTAINER_PARAMETER);
    info.addParameter(SECOND_CONTAINER_PARAMETER);
    ElementContext element = new ElementContext.Container(ELEMENT_NAME);
    ContainerMethod method = new ContainerMethod(element);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testContainerMethodWithParameters() {
    MethodInfo info = new MethodInfo(METHOD_NAME, RETURN_TYPE);
    info.addCodeLine(
        String.format(
            "if (pageObjectType.equals(%s.class)) {",
            CONTAINER_ELEMENT_PAGE_OBJECT_TYPE.getSimpleName()));
    info.addCodeLine(
        String.format(
            "return (T)(new %s(this.%s))",
            CONTAINER_ELEMENT_PAGE_OBJECT_TYPE.getSimpleName(), ELEMENT_NAME));
    info.addCodeLine("}");
    info.addCodeLine(
        String.format("setParameters(this.%s, parameter).%s", ELEMENT_NAME, EXPECTED_CODE_LOAD));
    info.addParameter(FIRST_CONTAINER_PARAMETER);
    info.addParameter(SECOND_CONTAINER_PARAMETER);
    info.addParameter(new MethodParameterInfo("parameter", "String"));
    List<MethodParameter> parameters =
        Stream.of(new ParameterUtils.Primitive("parameter", PrimitiveType.STRING))
            .collect(Collectors.toList());
    ElementContext scope =
        new ElementContext.Basic(
            ROOT_SCOPE,
            "scope",
            actionable.getType(),
            getCssSelector("css"),
            false,
            parameters);
    ElementContext element = new ElementContext.Container(scope, ELEMENT_NAME);
    ContainerMethod method = new ContainerMethod(element);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }
}
