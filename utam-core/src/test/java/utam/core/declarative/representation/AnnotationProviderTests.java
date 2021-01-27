package utam.core.declarative.representation;

import org.testng.annotations.Test;
import utam.core.declarative.representation.AnnotationProvider;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

/**
 * Tests the default implementation and static members of the AnnotationProvider interface
 *
 * @author james.evans
 */
public class AnnotationProviderTests {

  /**
   * Tests that the default implementation of the getImportTypes method exists and can be called
   * even if a custom implementation does not override the interface default method definition
   */
  @Test
  public void testDefaultImportTypes() {
    AnnotationProvider provider = new MockAnnotationProvider();
    assertThat(provider.getImportTypes().size(), is(equalTo(0)));
  }

  /**
   * Tests that the default implementation of the isEmpty method returns true if the annotation text
   * is empty, and that this is so even if a custom implementation does not override the interface
   * default method definition
   */
  @Test
  public void testDefaultIsEmpty() {
    AnnotationProvider provider = new MockAnnotationProvider();
    assertThat(provider.getAnnotationText().isEmpty(), is(equalTo(true)));
  }

  /**
   * Tests that the default implementation of the isEmpty method returns false if the annotation
   * text is not empty, and that this is so even if a custom implementation does not override the
   * interface default method definition
   */
  @Test
  public void testDefaultIsEmptyWithAnnotationText() {
    AnnotationProvider provider = new MockAnnotationProvider("textValue");
    assertThat(provider.getAnnotationText().isEmpty(), is(equalTo(false)));
    assertThat(provider.getAnnotationText(), is(equalTo("textValue")));
  }

  private static class MockAnnotationProvider implements AnnotationProvider {
    private final String annotationText;

    MockAnnotationProvider() {
      this("");
    }

    MockAnnotationProvider(String annotationText) {
      this.annotationText = annotationText;
    }

    @Override
    public String getAnnotationText() {
      return annotationText;
    }
  }
}
