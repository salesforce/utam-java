package utam.core.declarative.representation;

import java.util.ArrayList;
import java.util.List;

/**
 * type of utam objects for imports and declarations
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface TypeProvider {

  List<TypeProvider> EMPTY_LIST = new ArrayList<>();

  /**
   * full class name to be used in imports statement
   *
   * @return full class name to use in import, ex. spec.generated.ui.Input
   */
  String getFullName();

  /**
   * Simple class name to be used in field declaration
   *
   * @return simple class name
   */
  String getSimpleName();

  /**
   * name of the package, used in translator to declare package
   *
   * @return package name
   */
  String getPackageName();

  @Override
  boolean equals(Object type);
}
