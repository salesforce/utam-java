package utam.compiler.representation;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;

import java.util.ArrayList;
import java.util.List;
import utam.compiler.helpers.ElementContext;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * business method is a sequence of internal method calls
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class ComposeMethod implements PageObjectMethod {

  private final String name;
  private final List<MethodParameter> parameters;
  private final List<String> code = new ArrayList<>();
  private final List<TypeProvider> classImports = new ArrayList<>();
  private final List<TypeProvider> imports = new ArrayList<>();
  private final String comments;
  private TypeProvider returns;

  public ComposeMethod(String name, List<ComposeMethodStatement> statements,
      List<MethodParameter> parameters, String comments) {
    this.name = name;
    this.parameters = new ArrayList<>(parameters);
    statements.forEach(
        statement -> {
          code.addAll(statement.getCodeLines());
          imports.addAll(statement.getImports());
          classImports.addAll(statement.getClassImports());
          returns = statement.getReturnType(); // set to return from last action
        });
    this.comments = comments;
  }

  static String getElementGetterString(ElementContext elementContext) {
    List<MethodParameter> allParameters = elementContext.getParameters();
    String methodName = elementContext.getElementMethod().getDeclaration().getName();
    String parameters = getParametersValuesString(allParameters);
    return "this." + String.format("%s(%s)", methodName, parameters);
  }

  @Override
  public MethodDeclarationImpl getDeclaration() {
    return new MethodDeclarationImpl(name, parameters, returns, imports, comments);
  }

  @Override
  public List<TypeProvider> getClassImports() {
    return classImports;
  }

  @Override
  public List<String> getCodeLines() {
    return code;
  }

  @Override
  public boolean isPublic() {
    return true;
  }

}
