package utam.compiler.representation;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.TypeUtilities;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * generate code of getter method for basic element
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public abstract class ElementMethod implements PageObjectMethod {

  private static String getParametersVararg(List<MethodParameter> parameters) {
    if (!parameters.isEmpty()) {
      return ", " + getParametersValuesString(parameters);
    }
    return "";
  }

  private static String getElementMethodCode(ElementContext element, boolean isList) {
    return String.format("element(this.%s).%s(%s.class%s)",
        element.getName(),
        isList? "buildList" : "build",
        element.getType().getSimpleName(),
        getParametersVararg(element.getParameters()));
  }

  private static String getElementFilteredListMethodCode(
      String elementName,
      TypeProvider elementType,
      List<MethodParameter> elementParameters,
      String predicateCode,
      boolean isReturnFirstMatch) {
    return String.format(
        "element(this.%s).%s(%s.class, %s%s)",
        elementName,
        isReturnFirstMatch ? "build" : "buildList",
        elementType.getSimpleName(),
        predicateCode,
        getParametersVararg(elementParameters));
  }

  static String getPredicateCode(
      String applyMethod,
      List<MethodParameter> applyParameters,
      MatcherType matcherType,
      List<MethodParameter> matcherParameters) {
    String applyInvocationCode =
        String.format("elm.%s(%s)", applyMethod, getParametersValuesString(applyParameters));
    String matcherCode = matcherType.getCode(matcherParameters, applyInvocationCode);
    return String.format("elm -> %s", matcherCode);
  }

  public static final class Single implements PageObjectMethod {

    private final String methodCode;
    private final TypeProvider returnType;
    private final List<MethodParameter> parameters;
    private final String methodName;
    private final boolean isPublic;

    public Single(ElementContext element, boolean isPublic) {
      this.methodCode = getElementMethodCode(element, false);
      this.parameters = element.getParameters();
      this.methodName = getElementGetterMethodName(element.getName(), isPublic);
      this.returnType = element.getType();
      this.isPublic = isPublic;
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(
          methodName, parameters, returnType, Stream.of(returnType).collect(Collectors.toList()));
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return getDeclaration().getImports();
    }

    @Override
    public List<String> getCodeLines() {
      return Stream.of(methodCode).collect(Collectors.toList());
    }

    @Override
    public boolean isPublic() {
      return this.isPublic;
    }
  }

  public static final class Multiple implements PageObjectMethod {

    private final String methodCode;
    private final TypeProvider returnType;
    private final List<MethodParameter> parameters;
    private final String methodName;
    private final boolean isPublic;

    public Multiple(ElementContext element, boolean isPublic) {
      this.methodCode = getElementMethodCode(element, true);
      this.parameters = element.getParameters();
      this.methodName = getElementGetterMethodName(element.getName(), isPublic);
      this.returnType = element.getType();
      this.isPublic = isPublic;
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(
          methodName,
          parameters,
          new TypeUtilities.ListOf(returnType),
          Stream.of(returnType, LIST_IMPORT).collect(Collectors.toList()));
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return getDeclaration().getImports();
    }

    @Override
    public List<String> getCodeLines() {
      return Stream.of(methodCode).collect(Collectors.toList());
    }

    @Override
    public boolean isPublic() {
      return this.isPublic;
    }
  }

  public static final class Filtered implements PageObjectMethod {

    private final boolean isPublic;
    private final String methodName;
    private final TypeProvider returnType;
    private final List<MethodParameter> parameters;
    private final List<String> codeLines = new ArrayList<>();
    private final TypeProvider returnListType;
    private final List<TypeProvider> imports = new ArrayList<>();

    public Filtered(
        String elementName,
        TypeProvider elementType,
        List<MethodParameter> elementParameters,
        boolean isPublic,
        String applyMethod,
        List<MethodParameter> applyParameters,
        MatcherType matcherType,
        List<MethodParameter> matcherParameters,
        boolean isFindFirstMatch) {
      this.isPublic = isPublic;
      this.methodName = getElementGetterMethodName(elementName, isPublic);
      this.returnType = elementType;
      this.returnListType = isFindFirstMatch ? null : new TypeUtilities.ListOf(returnType);
      this.parameters = new ArrayList<>(elementParameters);
      this.parameters.addAll(applyParameters);
      this.parameters.addAll(matcherParameters);
      this.imports.add(returnType);
      if (returnListType != null) {
        this.imports.add(returnListType);
      }
      this.codeLines.add(
          getElementFilteredListMethodCode(
              elementName,
              elementType,
              elementParameters,
              getPredicateCode(applyMethod, applyParameters, matcherType, matcherParameters),
              isFindFirstMatch));
    }

    @Override
    public MethodDeclaration getDeclaration() {
      TypeProvider returns =
          returnListType != null ? new TypeUtilities.ListOf(returnType) : returnType;
      return new MethodDeclarationImpl(methodName, parameters, returns, imports);
    }

    @Override
    public List<String> getCodeLines() {
      return codeLines;
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return getDeclaration().getImports();
    }

    @Override
    public boolean isPublic() {
      return this.isPublic;
    }
  }
}
