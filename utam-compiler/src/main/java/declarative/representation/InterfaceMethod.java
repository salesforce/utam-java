package declarative.representation;

import declarative.helpers.TypeUtilities;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static declarative.helpers.TypeUtilities.LIST_IMPORT;

/**
 * method declared inside interface
 * @author elizaveta.ivanova
 * @since 226
 */
public class InterfaceMethod extends MethodDeclarationImpl implements PageObjectMethod {

  private static final List<String> EMPTY_CODE = new ArrayList<>();

  public InterfaceMethod(String name, TypeProvider returnType, boolean isReturnList, List<MethodParameter> methodParameters, String comments) {
    super(
        name,
        methodParameters,
        isReturnList? new TypeUtilities.ListOf(returnType) : returnType,
        getImports(methodParameters, returnType, isReturnList), comments);
  }

  private static List<TypeProvider> getImports(List<MethodParameter> methodParameters, TypeProvider returnType, boolean isReturnList) {
    List<TypeProvider> imports = methodParameters.stream().map(MethodParameter::getType).collect(Collectors.toList());
    if(isReturnList) {
      imports.add(LIST_IMPORT);
    }
    imports.add(returnType);
    return imports;
  }

  @Override
  public List<TypeProvider> getClassImports() {
    return getDeclaration().getImports();
  }

  @Override
  public MethodDeclarationImpl getDeclaration() {
    return this;
  }

  @Override
  public List<String> getCodeLines() {
    return EMPTY_CODE;
  }

  @Override
  public boolean isPublic() {
    return true;
  }
}
