package declarative.representation;

import declarative.helpers.TypeUtilities;
import declarative.representation.PageObjectValidationTestHelper.MethodInfo;
import declarative.translator.TranslationTypesConfig;
import declarative.translator.TranslationTypesConfigJava;
import org.testng.annotations.Test;
import selenium.element.Actionable;

import static declarative.helpers.ParameterUtils.EMPTY_PARAMETERS;

public class UtilityMethodTests {

  private static final TranslationTypesConfig TYPES_CONFIG = new TranslationTypesConfigJava();
  private static final String UTILITY_FULL_TYPE = "utam-test/utils/test/utilityClass";

  static TypeProvider getUtilityType() {
    return TYPES_CONFIG.getUtilityType(UTILITY_FULL_TYPE);
  }

  /** A basic UtilityMethod object should be able to be created */
  @Test
  public void testUtilityMethodCreation() {
    TypeProvider returnType = new TypeUtilities.FromClass(Actionable.class);
    TypeProvider utilityClassType = getUtilityType();
    MethodInfo info = new MethodInfo("testMethod", returnType.getSimpleName());
    info.addCodeLine("this.utilUtilityClass.appliedMethod()");
    info.addImportedTypes(returnType.getFullName());
    info.addImpliedImportedTypes(utilityClassType.getFullName());

    UtilityMethod method =
        new UtilityMethod(
                new TypeUtilities.FromClass(Actionable.class),
            false,
            new UtilityMethod.Utility(
                "appliedMethod", EMPTY_PARAMETERS, new UtilityReferenceField(utilityClassType)));
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  /** A basic UtilityMethod object should be able to be created with a list return value */
  @Test
  public void testUtilityMethodCreationWithList() {
    TypeProvider returnType =
        new TypeUtilities.ListOf(new TypeUtilities.FromClass(Actionable.class));
    TypeProvider utilityClassType = getUtilityType();
    MethodInfo info = new MethodInfo("testMethod", returnType.getSimpleName());
    info.addCodeLine("this.utilUtilityClass.appliedMethod()");
    info.addImpliedImportedTypes(
        new TypeUtilities.FromClass(Actionable.class).getFullName(),
        "java.util.List",
        utilityClassType.getFullName());

    UtilityMethod method =
        new UtilityMethod(
                new TypeUtilities.FromClass(Actionable.class),
            true,
            new UtilityMethod.Utility(
                "appliedMethod", EMPTY_PARAMETERS, new UtilityReferenceField(utilityClassType)));
    PageObjectValidationTestHelper.validateMethod(method, info);
  }
}
