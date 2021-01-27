package declarative.representation;

import declarative.helpers.TypeUtilities;
import declarative.representation.PageObjectValidationTestHelper.MethodInfo;
import org.testng.annotations.Test;
import selenium.element.Actionable;
import selenium.element.Clickable;
import selenium.element.Editable;

/**
 * Provides tests for the ElementMethod class
 *
 * @author james.evans
 */
public class RootElementMethodTests {

  /** An ElementMethod object describing a root elements should be able to be created */
  @Test
  public void testPublicRootElementMethodCreation() {
    MethodInfo info = new MethodInfo("getRoot", Clickable.class.getSimpleName());
    info.addCodeLine("(Clickable) this.getRootElement()");
    info.addImportedTypes(Clickable.class.getName());
    PageObjectMethod method =
        new RootElementMethod.Public(new TypeUtilities.FromClass(Clickable.class));
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testProtectedRootElementMethodCreation() {
    MethodInfo info = new MethodInfo("getRootElement", Actionable.class.getSimpleName());
    info.addCodeLine("this.getRootElement()");
    info.addImportedTypes(Actionable.class.getName());
    PageObjectMethod method = new RootElementMethod.Protected();
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testPrivateRootElementMethodCreation() {
    MethodInfo info = new MethodInfo("getRoot", Editable.class.getSimpleName());
    info.addCodeLine("(Editable) this.getRootElement()");
    info.addImportedTypes(Editable.class.getName());
    PageObjectMethod method =
        new RootElementMethod.Private(TypeUtilities.Element.editable.getType());
    PageObjectValidationTestHelper.validateMethod(method, info);
  }
}
