package declarative.representation;

import static declarative.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static declarative.translator.TranslationUtilities.EMPTY_COMMENTS;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

import declarative.helpers.ParameterUtils;
import declarative.helpers.PrimitiveType;
import declarative.helpers.TypeUtilities;
import declarative.representation.PageObjectValidationTestHelper.MethodInfo;
import org.testng.annotations.Test;
import selenium.element.Actionable;

import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * tests for the InterfaceMethod representation class
 *
 * @author james.evans
 */
public class InterfaceMethodTests {

  /** An InterfaceMethod object should be able to be be created */
  @Test
  public void testInterfaceMethodReturnsActionableElement() {
    MethodInfo info = new MethodInfo("testMethod", "Actionable");
    TypeProvider returnType = new TypeUtilities.FromClass(Actionable.class);
    InterfaceMethod method =
        new InterfaceMethod("testMethod", returnType, false, EMPTY_PARAMETERS, EMPTY_COMMENTS);
    PageObjectValidationTestHelper.validateMethod(method, info);
    assertThat(method.getClassImports(), hasSize(1));
  }

  @Test
  public void testInterfaceMethodReturnsList() {
    TypeProvider returnType = new TypeUtilities.FromString("SomeReturnType");
    InterfaceMethod method = new InterfaceMethod("testMethod", returnType, true, EMPTY_PARAMETERS, EMPTY_COMMENTS);
    assertThat(method.getCodeLines().isEmpty(), is(true));
    assertThat(method.getClassImports(), hasSize(2));
    assertThat(method.getClassImports().get(0).getSimpleName(), is(equalTo("List")));
    assertThat(method.getClassImports().get(1).getSimpleName(), is(equalTo("SomeReturnType")));
    assertThat(method.getDeclaration().getCodeLine(), is(equalTo("List<SomeReturnType> testMethod()")));
  }

  @Test
  public void testInterfaceMethodWithParameters() {
    TypeProvider returnType = PrimitiveType.STRING;
    InterfaceMethod method = new InterfaceMethod("testMethod", returnType, false,
            Stream.of(new ParameterUtils.Regular("name", new TypeUtilities.FromString("Type"))).collect(Collectors.toList()),
            EMPTY_COMMENTS);
    assertThat(method.getCodeLines().isEmpty(), is(true));
    assertThat(method.getClassImports(), hasSize(2));
    assertThat(method.getClassImports().get(0).getSimpleName(), is(equalTo("Type")));
    assertThat(method.getClassImports().get(1).getSimpleName(), is(equalTo("String")));
    assertThat(method.getDeclaration().getCodeLine(), is(equalTo("String testMethod(Type name)")));
  }
}
