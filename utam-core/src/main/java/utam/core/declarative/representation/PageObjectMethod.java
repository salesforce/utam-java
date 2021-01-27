package utam.core.declarative.representation;

import java.util.List;

/**
 * page object method
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface PageObjectMethod {

  MethodDeclaration getDeclaration();

  List<String> getCodeLines();

  List<TypeProvider> getClassImports();

  boolean isPublic();
}
