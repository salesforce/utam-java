package utam.core.declarative.representation;

import java.util.List;

/**
 * class field <br>
 * cab be elements and inner components
 *
 * @author elizaveta.ivanova
 * @since 224
 */
public interface PageClassField {

  /**
   * name of the field
   *
   * @return name of element
   */
  String getName();

  /**
   * list of field annotations
   *
   * @return field annotations
   */
  List<AnnotationProvider> getAnnotations();

  TypeProvider getType();

  String getDeclaration();

  String getComments();
}
