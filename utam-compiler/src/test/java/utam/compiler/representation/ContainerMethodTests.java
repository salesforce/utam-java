package utam.compiler.representation;

import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.base.PageObject;
import org.testng.annotations.Test;
import utam.core.selenium.element.Selector;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.helpers.TypeUtilities.Element.actionable;
import static utam.compiler.representation.ContainerMethod.*;

/**
 * ContainerMethod representation class tests
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public class ContainerMethodTests {

  public static final String RETURN_TYPE = RETURNS.getSimpleName();;
  public static final String EXPECTED_CODE_LOAD = "load(pageObjectType, injectedSelector)";
  public static final MethodParameterInfo FIRST_CONTAINER_PARAMETER =
      new PageObjectValidationTestHelper.MethodParameterInfo(
          PAGE_OBJECT_PARAMETER.getValue(), PAGE_OBJECT_PARAMETER.getType().getSimpleName());
  public static final MethodParameterInfo SECOND_CONTAINER_PARAMETER =
      new PageObjectValidationTestHelper.MethodParameterInfo(
          SELECTOR_PARAMETER.getValue(), SELECTOR_PARAMETER.getType().getSimpleName());
  private static final String METHOD_NAME = "getContainer";
  private static final String ELEMENT_NAME = "container";

  private static ElementContext getScope() {
    final ElementContext scope =
        new ElementContext.Basic("scope", actionable.getType(), getCssSelector("css"));
    MethodDeclaration declaration = mock(MethodDeclaration.class);
    PageObjectMethod mock = mock(PageObjectMethod.class);
    when(declaration.getName()).thenReturn("getScope");
    when(mock.getDeclaration()).thenReturn(declaration);
    scope.setElementMethod(mock);
    return scope;
  }

  @Test
  public void testContainerMethodReturnsSingle() {
    MethodInfo info = new MethodInfo(METHOD_NAME, RETURN_TYPE);
    info.addCodeLine(
        "this.inContainer(this.getScope(), false).load(pageObjectType, injectedSelector)");
    info.addImportedTypes(PageObject.class.getName(), Selector.class.getName());
    info.addImpliedImportedTypes(PageObject.class.getName(), Selector.class.getName());
    info.addParameter(FIRST_CONTAINER_PARAMETER);
    info.addParameter(SECOND_CONTAINER_PARAMETER);
    ContainerMethod method = new ContainerMethod.ReturnsSingle(getScope(), false, ELEMENT_NAME);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testContainerMethodReturnsList() {
    MethodInfo info = new MethodInfo(METHOD_NAME, RETURNS_LIST.getSimpleName());
    info.addCodeLines(
        "this.inContainer(this.getScope(), true).loadList(pageObjectType, injectedSelector)");
    info.addParameter(FIRST_CONTAINER_PARAMETER);
    info.addParameter(SECOND_CONTAINER_PARAMETER);
    ContainerMethod method = new ContainerMethod.ReturnsList(getScope(), true, ELEMENT_NAME);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }
}
