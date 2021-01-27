package declarative.representation;

import declarative.helpers.TypeUtilities;
import declarative.representation.PageObjectValidationTestHelper.FieldInfo;
import org.testng.annotations.Test;

import java.util.stream.Collectors;
import java.util.stream.Stream;

import static declarative.helpers.AnnotationUtils.EMPTY_ANNOTATIONS;
import static declarative.translator.TranslationUtilities.EMPTY_COMMENTS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Provides tests for the ElementField class
 *
 * @author james.evans
 */
public class ElementFieldTests {

  /** An ElementField object should be able to be created */
  @Test
  public void testElementFieldCreation() {
    FieldInfo info = new FieldInfo("fakeElementName", "FakeElementType");

    ElementField field =
        new ElementField(
            "fakeElementName",
            new TypeUtilities.FromString("FakeElementType", "test.FakeElementType"),
            EMPTY_ANNOTATIONS, EMPTY_COMMENTS);
    info.validateField(field);
  }

  /** An ElementField object should be able to be created with annotations */
  @Test
  public void testElementFieldCreationWithAnnotations() {
    FieldInfo info =
        new FieldInfo("fakeElementName", "FakeElementType", "isShadow", "fakeAnnotation");
    info.setComments("field comments");

    AnnotationProvider annotation1 = mock(AnnotationProvider.class);
    when(annotation1.getAnnotationText()).thenReturn("isShadow");
    AnnotationProvider annotation2 = mock(AnnotationProvider.class);
    when(annotation2.getAnnotationText()).thenReturn("fakeAnnotation");

    ElementField field =
        new ElementField(
            "fakeElementName",
            new TypeUtilities.FromString("FakeElementType", "test.FakeElementType"),
            Stream.of(annotation1, annotation2).collect(Collectors.toList()), "field comments");
    info.validateField(field);
  }
}
