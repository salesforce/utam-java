package utam.compiler.representation;

import org.testng.annotations.Test;
import utam.compiler.helpers.ElementContext;
import utam.core.declarative.representation.PageObjectMethod;

import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT;

public class FrameMethodTests {
  private static final String ELEMENT_NAME = "testFrame";
  private static final String ELEMENT_METHOD_NAME = "getTestFrame";
  private static final String FRAME_ELEMENT_IMPL_TYPE = "utam.core.framework.base.FrameElementImpl";

  @Test
  public void testFrameElementGetterMethodCreated() {
    PageObjectValidationTestHelper.MethodInfo
        info = new PageObjectValidationTestHelper.MethodInfo(ELEMENT_METHOD_NAME, FRAME_ELEMENT.getSimpleName());
    info.addCodeLine("element(this.testFrame).build(FrameElement.class, FrameElementImpl.class)");
    info.addImportedTypes(FRAME_ELEMENT.getFullName());
    info.addImpliedImportedTypes(FRAME_ELEMENT_IMPL_TYPE);

    ElementContext element =
        new ElementContext.Frame(null, ELEMENT_NAME, getCssSelector(".css"));
    PageObjectMethod method = new FrameMethod(element);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }
}
