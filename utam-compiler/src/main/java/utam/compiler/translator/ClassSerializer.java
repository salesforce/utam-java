/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static utam.compiler.translator.TranslationUtilities.NEW_LINE;
import static utam.compiler.translator.TranslationUtilities.applyJavaFormatter;
import static utam.compiler.translator.TranslationUtilities.getImportStrings;
import static utam.compiler.translator.TranslationUtilities.getPackageDeclaration;
import static utam.compiler.translator.TranslationUtilities.getStatement;
import static utam.compiler.translator.TranslationUtilities.getWrappedJavadoc;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import utam.compiler.helpers.AnnotationUtils;
import utam.core.declarative.representation.AnnotationProvider;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageClassField;
import utam.core.declarative.representation.PageObjectClass;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * helper to generate java code from PO class representation
 *
 * @author elizaveta.ivanova
 * @since 224
 */
public final class ClassSerializer {

  private final PageObjectClass source;

  /**
   * Initializes a new instance of the ClassSerializer class
   *
   * @param pageObject the Page Object class to serialize
   */
  public ClassSerializer(PageObjectClass pageObject) {
    this.source = pageObject;
  }

  private static List<String> getMethodDeclaration(PageObjectMethod method) {
    MethodDeclaration declaration = method.getDeclaration();
    List<String> out = new ArrayList<>();
    out.add(NEW_LINE);
    out.addAll(getWrappedJavadoc(declaration.getDescription()));
    out.add(NEW_LINE);
    if (method.isPublic()) {
      out.add("@Override");
    }
    if (method.getDeclaration().isDeprecated()) {
      out.add(AnnotationUtils.DEPRECATED_ANNOTATION.getAnnotationText());
    }
    out.add(
        String.format(
            "%sfinal %s {", (method.isPublic() ? "public " : ""), declaration.getCodeLine()));
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
    List<String> out = new ArrayList<>();
    if (!source.getCopyright().isEmpty()) {
      out.addAll(getWrappedJavadoc(source.getCopyright()));
    }
    out.add(getPackageName());
    out.add(NEW_LINE);
    out.addAll(getImports());
    out.add(NEW_LINE);
    out.addAll(getWrappedJavadoc(source.getDescription()));
    getClassAnnotations().stream()
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
        .flatMap(method -> getMethodDeclaration(method).stream())
        .forEach(out::add);
    out.add(NEW_LINE);
    source
        .getUnionTypes()
        .forEach(
            unionType -> {
              out.addAll(unionType.getDeclarationCode());
              out.add(NEW_LINE);
            });
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

  private Set<String> getImportStatements(TypeProvider type) {
    return getImportStrings(type, this.source.getClassType().getPackageName());
  }

  private Set<String> getImports() {
    Set<String> res = new HashSet<>();
    res.addAll(getImportStatements(source.getBaseClassType()));
    res.addAll(getImportStatements(source.getImplementedType().getInterfaceType()));
    getClassAnnotations().stream()
        .flatMap(a -> a.getImportTypes().stream())
        .forEach(a -> res.addAll(getImportStatements(a)));
    source.getFields().stream()
        .peek(field -> res.addAll(getImportStatements(field.getType())))
        .flatMap(classField -> classField.getAnnotations().stream())
        .flatMap(a -> a.getImportTypes().stream())
        .forEach(a -> res.addAll(getImportStatements(a)));
    source
        .getMethods()
        .forEach(
            m ->
                m.getClassImports()
                    .forEach(importStr -> res.addAll(getImportStatements(importStr))));
    source
        .getUnionTypes()
        .forEach(
            unionType ->
                unionType
                    .getExtendedTypes()
                    .forEach(type -> res.addAll(getImportStatements(type))));
    return res;
  }

  private List<List<String>> getClassFields() {
    return source.getFields().stream()
        .map(ClassSerializer::getClassField)
        .collect(Collectors.toList());
  }
}
