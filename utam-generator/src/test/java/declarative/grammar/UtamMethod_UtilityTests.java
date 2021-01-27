package declarative.grammar;

import declarative.helpers.TranslationContext;
import declarative.representation.PageObjectMethod;
import declarative.representation.PageObjectValidationTestHelper;
import declarative.representation.UtilityMethod;
import org.testng.annotations.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamMethod_UtilityTests {

  /** The getMethod method should return a valid value for a utility method */
  @Test
  public void testGetMethodForUtilityMethod() {
    final String METHOD_NAME = "testMethod";
    final String APPLY_METHOD_NAME = "utilityMethod";
    final String UTILITY_TYPE = "utam-test/utils/test/utilityMethodClass";
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod utamMethod =
        new UtamMethod(
            METHOD_NAME,
            "string",
            new UtamMethodUtil(UTILITY_TYPE, APPLY_METHOD_NAME, new UtamArgument[] {}));
    PageObjectValidationTestHelper.MethodInfo methodInfo =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "String");
    methodInfo.addCodeLine(String.format("this.utilUtilityMethodClass.%s()", APPLY_METHOD_NAME));
    PageObjectMethod methodObject = utamMethod.getMethod(context);
    assertThat(methodObject, is(instanceOf(UtilityMethod.class)));
    PageObjectValidationTestHelper.validateMethod(methodObject, methodInfo);
  }


}
