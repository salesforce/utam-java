package utam.core.declarative.representation;

import java.util.Collection;

/**
 * representation of the interface to generate
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface PageObjectInterface {

  Collection<MethodDeclaration> getDeclaredApi();

  TypeProvider getInterfaceType();

  TypeProvider getBaseInterfaceType();

  String getApiCode();

  String getComments();
}
