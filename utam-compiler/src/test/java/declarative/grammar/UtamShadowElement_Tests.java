package declarative.grammar;

import static declarative.grammar.UtamShadowElement.ERR_SHADOW_EMPTY_ELEMENTS;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

import framework.consumer.UtamError;
import org.testng.annotations.Test;

/**
 * Provides tests for the UtamShadowElement class
 * @author james.evans
 *
 */
public class UtamShadowElement_Tests {

  /**
   * A valid UtamShadowElement should be able to be created
   */
  @Test
  public void testUtamShadowElementEmptyArray() {
    UtamShadowElement shadow = new UtamShadowElement(new UtamElement[] {});
    assertThat(shadow.elements, is(arrayWithSize(0)));
  }

  @Test
  public void testUtamShadowNestedElement() {
    UtamShadowElement shadow = new UtamShadowElement(new UtamElement[] { new UtamElement("nested")});
    assertThat(shadow.elements, is(arrayWithSize(1)));
  }


  /**
   * A valid UtamShadowElement should throw the proper exception when passed a null
   * arguments array
   */
  @Test
  public void testUtamShadowElementWithNullElementArrayThrows() {
    UtamError e = expectThrows(
        UtamError.class,
        () -> new UtamShadowElement(null));
    assertThat(e.getMessage(), containsString(ERR_SHADOW_EMPTY_ELEMENTS));
  }
}
