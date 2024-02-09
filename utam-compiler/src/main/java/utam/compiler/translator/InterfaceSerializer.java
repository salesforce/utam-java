/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static utam.compiler.grammar.UtamPageObject.BEFORE_LOAD_METHOD_NAME;
import static utam.compiler.helpers.AnnotationUtils.DEPRECATED_ANNOTATION;
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
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageObjectInterface;
import utam.core.declarative.representation.TypeProvider;

/**
 * helper to generate java code from PO interface representation
 *
 * @author elizaveta.ivanova
 * @since 224
 */
public final class InterfaceSerializer {

  private final PageObjectInterface source;

  /**
   * Initializes a new instance of the InterfaceSerializer class
   *
   * @param source the interface object to serialize
   */
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

  private void addMethodDeclaration(List<String> out, MethodDeclaration declaration) {
    // The interface extends the PageObject interface, which already has a load
    // method, so we don't need to add a load method to the generated interface.
    if (declaration.getName().equals(BEFORE_LOAD_METHOD_NAME)) {
      return;
    }

    out.add(NEW_LINE);
    out.addAll(getWrappedJavadoc(declaration.getDescription()));
    if (declaration.isDeprecated()) {
      out.add(DEPRECATED_ANNOTATION.getAnnotationText());
    }
    out.add(NEW_LINE);
    out.add(getStatement(declaration.getCodeLine()));
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
    if (source.isDeprecated()) {
      out.add(DEPRECATED_ANNOTATION.getAnnotationText());
    }
    out.add(getDeclaration());
    source.getDeclaredApi().forEach(declaration -> addMethodDeclaration(out, declaration));
    source
        .getUnionTypes()
        .forEach(
            unionType -> {
              out.addAll(unionType.getDeclarationCode());
              out.add(NEW_LINE);
            });
    out.add("}");
    out.removeIf(String::isEmpty);
    return applyJavaFormatter(out);
  }

  private Set<String> getImportStatements(TypeProvider type) {
    return getImportStrings(type, source.getInterfaceType().getPackageName());
  }

  private Set<String> getImports() {
    Set<String> res = new HashSet<>(getImportStatements(source.getBaseInterfaceType()));
    source.getDeclaredApi().stream()
        .flatMap(method -> method.getImports().stream())
        .forEach(importStr -> res.addAll(getImportStatements(importStr)));
    source
        .getUnionTypes()
        .forEach(
            unionType ->
                unionType
                    .getExtendedTypes()
                    .forEach(type -> res.addAll(getImportStatements(type))));
    return res;
  }
}
