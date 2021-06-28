/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import utam.compiler.helpers.TypeUtilities;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageObjectInterface;
import utam.core.declarative.representation.TypeProvider;

import java.util.*;
import java.util.stream.Collectors;

import static utam.compiler.translator.TranslationUtilities.*;

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
    source.getNestedInterfaces().forEach(
        nested -> {
          out.add(NEW_LINE);
          out.add(getNestedInterfaceDeclaration(nested));
        }
    );
    out.add("}");
    out.removeIf(String::isEmpty);
    return applyJavaFormatter(out);
  }

  private String getNestedInterfaceDeclaration(TypeProvider type) {
    return String.format("interface %s extends %s {}",
        type.getSimpleName(),
        ((TypeUtilities.Element)type).getBasicInterfaces().stream()
            .map(TypeProvider::getSimpleName)
            .collect(Collectors.joining(", ")));
  }

  private String getImportStatement(TypeProvider type) {
    return getImportString(type, source.getInterfaceType().getPackageName());
  }

  private Set<String> getImports(Collection<MethodDeclaration> declaredApi) {
    Set<String> res = new HashSet<>();
    res.add(getImportStatement(source.getBaseInterfaceType()));
    source.getNestedInterfaces().stream()
        .flatMap(nested -> ((TypeUtilities.Element)nested).getBasicInterfaces().stream())
        .forEach(basicInterface -> res.add(getImportStatement(basicInterface)));
    declaredApi.stream()
        .flatMap(method -> method.getImports().stream())
        .forEach(importStr -> res.add(getImportStatement(importStr)));
    res.removeIf(String::isEmpty);
    return res;
  }
}
