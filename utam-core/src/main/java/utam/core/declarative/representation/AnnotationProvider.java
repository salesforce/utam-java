package utam.core.declarative.representation;

import java.util.Collections;
import java.util.List;

/**
 * representation of the annotation
 *
 * @author elizaveta.ivanova
 * @since 224
 */
public interface AnnotationProvider {

  /**
   * text of the annotation, can be empty
   *
   * @return annotation code
   */
  String getAnnotationText();

  /**
   * annotation classes to import, can be empty
   *
   * @return by default empty list
   */
  default List<TypeProvider> getImportTypes() {
    return Collections.EMPTY_LIST;
  }
}
