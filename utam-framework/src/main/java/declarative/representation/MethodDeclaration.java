package declarative.representation;

import java.util.List;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public interface MethodDeclaration {

  String getName();

  List<MethodParameter> getParameters();

  TypeProvider getReturnType();

  List<TypeProvider> getImports();

  String getCodeLine();

  String getComments();
}
