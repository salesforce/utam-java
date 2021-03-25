package utam.compiler.representation;

import utam.core.declarative.representation.MethodDeclaration;
import utam.compiler.helpers.ElementContext;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.base.PageObject;
import org.testng.annotations.Test;
import utam.core.selenium.element.Selector;

import java.util.List;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_LIST_RETURN_TYPE;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_RETURN_TYPE;
import static utam.compiler.helpers.TypeUtilities.Element.actionable;
import static utam.compiler.representation.ContainerMethod.*;

/**
 * ContainerMethod representation class tests
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public class ContainerMethodTests {

  static final String RETURN_TYPE = CONTAINER_RETURN_TYPE.getSimpleName();
  static final String LIST_RETURN_TYPE = CONTAINER_LIST_RETURN_TYPE.getSimpleName();
  public static final MethodParameterInfo FIRST_CONTAINER_PARAMETER =
      new PageObjectValidationTestHelper.MethodParameterInfo(
          PAGE_OBJECT_PARAMETER.getValue(), PAGE_OBJECT_PARAMETER.getType().getSimpleName());
  private static final String METHOD_NAME = "getContainer";
  private static final String ELEMENT_NAME = "container";

  private static ElementContext getScope() {
    final ElementContext scope =
        new ElementContext.Basic("scope", actionable, getCssSelector("css"));
    MethodDeclaration declaration = mock(MethodDeclaration.class);
    PageObjectMethod mock = mock(PageObjectMethod.class);
    when(declaration.getName()).thenReturn("getScope");
    when(mock.getDeclaration()).thenReturn(declaration);
    scope.setElementMethod(mock);
    return scope;
  }

  @Test
  public void testContainerMethodWithSelector() {
    MethodInfo info = new MethodInfo(METHOD_NAME, RETURN_TYPE);
    info.addCodeLine(
        "this.inContainer(this.getScope(), false).load(pageObjectType, by(\".fakeSelector\", Selector.Type.CSS))");
    info.addImportedTypes(PageObject.class.getName());
    info.addImpliedImportedTypes(PageObject.class.getName(), Selector.class.getName());
    info.addParameter(FIRST_CONTAINER_PARAMETER);
    ContainerMethod method = new ContainerMethod.WithSelector(
        getScope(), false, ELEMENT_NAME, getCssSelector(".fakeSelector"), EMPTY_PARAMETERS);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testContainerMethodWithSelectorReturnsList() {
    MethodInfo info = new MethodInfo(METHOD_NAME, LIST_RETURN_TYPE);
    info.addCodeLine(
        "this.inContainer(this.getScope(), false).loadList(pageObjectType, by(\".fakeSelector\", Selector.Type.CSS))");
    info.addImportedTypes(PageObject.class.getName(), List.class.getName());
    info.addImpliedImportedTypes(PageObject.class.getName(), Selector.class.getName());
    info.addParameter(FIRST_CONTAINER_PARAMETER);
    ContainerMethod method = new ContainerMethod.WithSelectorReturnsList(
        getScope(), false, ELEMENT_NAME, getCssSelector(".fakeSelector"), EMPTY_PARAMETERS);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }
}
