/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import utam.compiler.helpers.BasicElementUnionType;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.*;
import utam.core.framework.element.BasePageElement;

import java.util.ArrayList;
import java.util.Collection;
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
    for (int i = 0; i < method.getCodeLines().size(); i++) {
      out.add(getStatement(method.getCodeLines().get(i)));
    }
    out.add("}");
    return out;
  }

  private static List<String> getClassField(PageClassField field) {
    List<String> out = new ArrayList<>();
    out.add(NEW_LINE);
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
            .filter(this::isUsedMethod)
            .flatMap(method -> getMethodDeclaration(method).stream()).forEach(out::add);
    out.add(NEW_LINE);
    source.getUnionTypes().forEach(unionType -> {
      out.addAll(unionType.getDeclarationCode());
      out.add(NEW_LINE);
    });
    addPublicElementClassDeclarations(out);
    addPrivateElementClassDeclarations(out);
    out.add(NEW_LINE);
    out.add("}");
    return applyJavaFormatter(out);
  }

  private void addPublicElementClassDeclarations(List<String> out) {
    source.getDeclaredElementTypes(true).stream()
        .map(returnType -> String.format(
            "public static class %s extends %s implements %s {}",
            returnType.getSimpleName() + "Impl",
            BasePageElement.class.getSimpleName(),
            returnType.getSimpleName()))
        .forEach(out::add);
  }

  private void addPrivateElementClassDeclarations(List<String> out) {
    List<TypeProvider> privateElementTypes = source.getDeclaredElementTypes(false);
    privateElementTypes.stream()
        .map(returnType -> String.format(
            "interface %s extends %s {}",
            returnType.getSimpleName(),
            ((BasicElementUnionType)returnType).getBasicInterfaces().stream()
                .map(TypeProvider::getSimpleName)
                .collect(Collectors.joining(", "))))
        .forEach(out::add);

    privateElementTypes.stream()
        .map(returnType -> String.format(
            "public static class %s extends %s implements %s {}",
            returnType.getSimpleName() + "Impl",
            BasePageElement.class.getSimpleName(),
            returnType.getSimpleName()))
        .forEach(out::add);
  }

  private boolean isUsedMethod(PageObjectMethod method) {
    return method.isPublic() ||
        translationContext.getUsedPrivateMethods().contains(method.getDeclaration().getName());
  }

  private String getDeclaration() {
    return String.format(
        "public final class %s extends %s implements %s {",
        source.getClassType().getSimpleName(),
        source.getBaseClassType().getSimpleName(),
        source.getImplementedType().getInterfaceType().getSimpleName());
  }

  private Set<String> getImportStatements(TypeProvider type) {
    return getImportStrings(type, this.source.getClassType().getPackageName());
  }

  private Set<String> getImports(List<AnnotationProvider> annotations) {
    Set<String> res = new HashSet<>();
    res.addAll(getImportStatements(source.getBaseClassType()));
    res.addAll(getImportStatements(source.getImplementedType().getInterfaceType()));
    source.getDeclaredElementTypes(false).stream()
        .map(returnType -> ((BasicElementUnionType)returnType).getBasicInterfaces())
        .flatMap(Collection::stream)
        .collect(Collectors.toSet())
        .forEach(type -> res.addAll(getImportStatements(type)));
    annotations.stream()
        .flatMap(a -> a.getImportTypes().stream())
        .forEach(a -> res.addAll(getImportStatements(a)));
    source.getFields().stream()
        .peek(field -> res.addAll(getImportStatements(field.getType())))
        .flatMap(classField -> classField.getAnnotations().stream())
        .flatMap(a -> a.getImportTypes().stream())
        .forEach(a -> res.addAll(getImportStatements(a)));
    source
        .getMethods()
        .forEach(m -> m.getClassImports().forEach(importStr -> res.addAll(getImportStatements(importStr))));
    return res;
  }

  private List<List<String>> getClassFields() {
    return source.getFields().stream()
        .map(ClassSerializer::getClassField)
        .collect(Collectors.toList());
  }
}
