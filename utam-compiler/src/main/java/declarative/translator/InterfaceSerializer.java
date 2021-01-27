package declarative.translator;

import declarative.representation.MethodDeclaration;
import declarative.representation.PageObjectInterface;
import declarative.representation.TypeProvider;

import java.util.*;

import static declarative.translator.TranslationUtilities.*;

/**
 * helper to generate java code from PO interface representation
 *
 * @author elizaveta.ivanova
 * @since 224
 */
public final class InterfaceSerializer {

  private final PageObjectInterface source;

  public InterfaceSerializer(PageObjectInterface source) {
    this.source = source;
  }

  private String getDeclaration() {
    return String.format(
        "public interface %s extends %s {",
        source.getInterfaceType().getSimpleName(), source.getBaseInterfaceType().getSimpleName());
  }

  private String getPackageName() {
    return getPackageDeclaration(source.getInterfaceType().getPackageName());
  }

  @Override
  public String toString() {
    Collection<MethodDeclaration> declaredApi = source.getDeclaredApi();
    List<String> out = new ArrayList<>();
    out.add(getPackageName());
    out.add(NEW_LINE);
    out.addAll(getImports(declaredApi));
    out.add(NEW_LINE);
    out.addAll(getWrappedClassJavadoc(source.getComments()));
    out.add(getDeclaration());
    declaredApi.forEach(
        declaration -> {
          out.add(NEW_LINE);
          out.addAll(getMethodWrappedJavadoc(declaration));
          out.add(NEW_LINE);
          out.add(getStatement(declaration.getCodeLine()));
        });
    out.add("}");
    out.removeIf(String::isEmpty);
    return applyJavaFormatter(out);
  }

  private String getImportStatement(TypeProvider type) {
    return getImportString(source.getInterfaceType(), type);
  }

  private Set<String> getImports(Collection<MethodDeclaration> declaredApi) {
    Set<String> res = new HashSet<>();
    res.add(getImportStatement(source.getBaseInterfaceType()));
    declaredApi.stream()
        .flatMap(method -> method.getImports().stream())
        .forEach(importStr -> res.add(getImportStatement(importStr)));
    res.removeIf(String::isEmpty);
    return res;
  }
}
