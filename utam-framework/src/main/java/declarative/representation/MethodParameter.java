package declarative.representation;

/**
 * method parameter
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface MethodParameter {

  boolean isLiteral();

  String getValue();

  String getDeclaration();

  TypeProvider getType();
}
