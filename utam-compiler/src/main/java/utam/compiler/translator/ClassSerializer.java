package utam.compiler.translator;

import utam.compiler.helpers.TranslationContext;
import declarative.representation.*;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static utam.compiler.translator.TranslationUtilities.*;

/**
 * helper to generate java code from PO class representation
 *
 * @author elizaveta.ivanova
 * @since 224
 */
public final class ClassSerializer {

  private final PageObjectClass source;
  private final TranslationContext translationContext;

  public ClassSerializer(PageObjectClass pageObject, TranslationContext translationContext) {
    this.source = pageObject;
    this.translationContext = translationContext;
  }

  private static List<String> getMethodDeclaration(PageObjectMethod method) {
    MethodDeclaration declaration = method.getDeclaration();
    List<String> out = new ArrayList<>();
    out.add(NEW_LINE);
    out.addAll(getMethodWrappedJavadoc(method.getDeclaration()));
    out.add(NEW_LINE);
    if (method.isPublic()) {
      out.add("@Override");
    }
    out.add(String.format("%sfinal %s {", (method.isPublic()? "public " : ""), declaration.getCodeLine()));
    for (int i = 0; i < method.getCodeLines().size() - 1; i++) {
      out.add(getStatement(method.getCodeLines().get(i)));
    }
    out.add(getLastStatement(method));
    out.add("}");
    return out;
  }

  private static List<String> getClassField(PageClassField field) {
    List<String> out = new ArrayList<>();
    out.add(NEW_LINE);
    out.addAll(getWrappedJavadoc(field.getComments()));
    field.getAnnotations().forEach(a -> out.add(a.getAnnotationText()));
    out.add(getStatement(field.getDeclaration()));
    out.removeIf(String::isEmpty);
    return out;
  }

  private List<AnnotationProvider> getClassAnnotations() {
    return source.getClassAnnotations().stream()
        .filter(annotation -> !annotation.getAnnotationText().isEmpty())
        .collect(Collectors.toList());
  }

  private String getPackageName() {
    return getPackageDeclaration(source.getClassType().getPackageName());
  }

  @Override
  public String toString() {
    List<AnnotationProvider> annotations = getClassAnnotations();
    List<String> out = new ArrayList<>();
    out.add(getPackageName());
    out.add(NEW_LINE);
    out.addAll(getImports(annotations));
    out.add(NEW_LINE);
    out.addAll(getWrappedClassJavadoc(source.getComments()));
    annotations.stream()
        .map(AnnotationProvider::getAnnotationText)
        .filter(s -> !s.isEmpty())
        .forEach(out::add);
    out.add(NEW_LINE);
    out.add(getDeclaration());
    out.add(NEW_LINE);
    getClassFields().forEach(out::addAll);
    out.add(NEW_LINE);
    out.add(NEW_LINE);
    source.getMethods().stream()
            // if method is private and never used, do not not declare to avoid test coverage alert
            .filter(method -> method.isPublic() || translationContext.getUsedPrivateMethods().contains(method.getDeclaration().getName()))
            .flatMap(method -> getMethodDeclaration(method).stream()).forEach(out::add);
    out.add(NEW_LINE);
    out.add(NEW_LINE);
    out.add("}");
    return applyJavaFormatter(out);
  }

  private String getDeclaration() {
    return String.format(
        "public final class %s extends %s implements %s {",
        source.getClassType().getSimpleName(),
        source.getBaseClassType().getSimpleName(),
        source.getImplementedType().getInterfaceType().getSimpleName());
  }

  private String getImportStatement(TypeProvider type) {
    return getImportString(source.getClassType(), type);
  }

  private Set<String> getImports(List<AnnotationProvider> annotations) {
    Set<String> res = new HashSet<>();
    res.add(getImportStatement(source.getBaseClassType()));
    res.add(getImportStatement(source.getImplementedType().getInterfaceType()));
    annotations.stream()
        .flatMap(a -> a.getImportTypes().stream())
        .forEach(a -> res.add(getImportStatement(a)));
    source.getFields().stream()
        .peek(field -> res.add(getImportStatement(field.getType())))
        .flatMap(classField -> classField.getAnnotations().stream())
        .flatMap(a -> a.getImportTypes().stream())
        .forEach(a -> res.add(getImportStatement(a)));
    source
        .getMethods()
        .forEach(
            m -> m.getClassImports().forEach(importStr -> res.add(getImportStatement(importStr))));
    res.removeIf(String::isEmpty);
    return res;
  }

  private List<List<String>> getClassFields() {
    return source.getFields().stream()
        .map(ClassSerializer::getClassField)
        .collect(Collectors.toList());
  }
}
